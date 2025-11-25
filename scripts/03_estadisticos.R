#                     EXPLORACION ESTADISTICOS
#           Autoras: Florencia Blanco y Camila Germain

library(readr)
library(dplyr)
library(here)
library(tidyr)

#definicion de directorios
proyecto_dir <- here::here()
dir_data_raw <- file.path(proyecto_dir, "data", "raw")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")

#llamado de funcion guardar tablas
source(here("scripts", "functions", "guardar_imagenes.R"))
source(here("scripts", "functions", "guardar_tablas.R"))

#1.definicion de funciones

get_mode <- function(v) {
  #filtramos NAs antes de calcular la moda
  v <- v[!is.na(v)] 
  if (length(v) == 0) return(NA)
  
  uniqv <- unique(v)
  #tabulate cuenta la frecuencia de cada valor unico
  mode_val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  
  #redondeamos para datos continuos si es necesario, pero lo mantendremos como valor exacto
  return(mode_val)
}

#2.carga de datos
#rutas de los archivos
ruta_csv_clean <- file.path(dir_data_clean, "datos_filtrados.csv") 

#usamos el dataset final limpio (imputado) generado por 01_limpieza.R
ruta_csv_limpio_final <- file.path(dir_data_clean, "datos_limpios.csv") 

#nombres de las variables a analizar
columnas_indicadores <- c("apertura_comercial", "pbi_crec", "ied_pct_pbi")
anios_analisis <- c(1990, 2001, 2010)

#2.1.carga y preparacion de datos crudos
message("Cargando y preparando datos crudos...")
datos_filtrados_preparados <- readr::read_csv(ruta_csv_clean) %>% 
  filter(año %in% anios_analisis) %>% 
  #convertir a formato largo para el analisis de grupo
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  mutate(dataset = "Filtrado (con NAs)")

#2.2.carga y preparacion de datos limpios (imputados) 
message("Cargando y preparando datos limpios...")
datos_limpios_preparados <- readr::read_csv(ruta_csv_limpio_final) %>% 
  filter(año %in% anios_analisis) %>%
  #convertimos a formato largo para el analisis de grupo
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  mutate(dataset = "Limpio (Imputado)")

#2.3.combinacion de datasets
df_combinado <- bind_rows(datos_filtrados_preparados, datos_limpios_preparados)


#3.calculo de estadisticos descriptivos

message("Calculando estadísticos descriptivos por Dataset, Año y Variable...")

df_resumen_estadistico <- df_combinado %>%
  #agrupamos por dataset, año y variable para calcular las metricas de forma independiente
  group_by(dataset, año, variable) %>%
  summarise(
    #Medidas de Tendencia Central
    Media = mean(valor, na.rm = TRUE),
    Mediana = median(valor, na.rm = TRUE),
    Moda = get_mode(valor),
    
    #Medidas de Dispersion
    Desvio_Estandar = sd(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE),
    
    .groups = 'drop' #Quitar agrupacion despues del resumen
  ) %>%
  
  #redondeamos a 3 decimales para una mejor presentacion
  mutate(across(c(Media, Mediana, Moda, Desvio_Estandar, IQR), ~round(., 3))) %>%
  
  #ordenamos por variable, año y dataset para una facil comparacion
  arrange(variable, año, dataset)

#4.impresion y guardado de resultados

print("--- RESUMEN DE ESTADISTICOS DESCRIPTIVOS (POR AÑO Y VARIABLE) ---")
print("Comparación: Dataset Filtrado (con NAs) vs. Dataset Limpio (Imputado)")
print("------------------------------------------------------------------")
print(df_resumen_estadistico)
print("------------------------------------------------------------------")

#guardamos tabla de resultados
ruta_tabla_resumen <- file.path(dir_outputs_tables, "resumen_estadisticos_anual.csv")
guardar_tabla(df_resumen_estadistico, ruta_tabla_resumen)


#realizamos tabla de cambio porcentual en los estadisticos respecto del dataset "limpio"
#con el de "filtrado"
tabla_cambio_porcentual <- df_resumen_estadistico %>%
  
  #normalizamos los nombres del dataset
  mutate(dataset = case_when(
    grepl("Filtrado", dataset) ~ "Filtrado",
    grepl("Limpio", dataset) ~ "Limpio",
    TRUE ~ dataset
  )) %>%
  
  #pasamos a formato ancho
  pivot_wider(
    names_from = dataset,
    values_from = c(Media, Mediana, Moda, Desvio_Estandar, IQR),
    names_sep = "_"
  ) %>%
  
  #calculo de cambios porcentuales
  mutate(
    CambioPct_Media   = (Media_Limpio   - Media_Filtrado)   / Media_Filtrado   * 100,
    CambioPct_Mediana = (Mediana_Limpio - Mediana_Filtrado) / Mediana_Filtrado * 100,
    CambioPct_Moda    = (Moda_Limpio    - Moda_Filtrado)    / Moda_Filtrado    * 100,
    CambioPct_Desvio  = (Desvio_Estandar_Limpio - Desvio_Estandar_Filtrado) / Desvio_Estandar_Filtrado * 100,
    CambioPct_IQR     = (IQR_Limpio - IQR_Filtrado) / 
      IQR_Filtrado * 100
  ) %>%
  select(
    variable, año,
    CambioPct_Media,
    CambioPct_Mediana,
    CambioPct_Moda,
    CambioPct_Desvio,
    CambioPct_IQR
  ) %>%
  arrange(variable, año)

#guardamos tabla
ruta_cambio_pct<-file.path(dir_outputs_tables, "cambio_porcentual_estadisticos")
guardar_tabla(tabla_cambio_porcentual, ruta_cambio_pct)
