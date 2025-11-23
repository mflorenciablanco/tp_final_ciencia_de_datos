#                     EXPLORACION ESTADISTICOS
#                           19/11/2025 
#           Autoras: Florencia Blanco y Camila Germain

library(readr)
library(dplyr)
library(here)
library(tidyr) 

#definición de directorios
proyecto_dir <- here::here()
dir_data_raw <- file.path(proyecto_dir, "data", "raw")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")

#1.definición de funciones

get_mode <- function(v) {

  #filtramos NAs antes de calcular la moda
  v <- v[!is.na(v)] 
  if (length(v) == 0) return(NA)
  
  uniqv <- unique(v)
  # tabulate para contar la frecuencia de cada valor único
  mode_val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  
  #redondeamos para datos continuos si es necesario, pero lo mantenemos como valor exacto
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

#2.1.carga y preparación de datos crudos

message("Cargando y preparando datos crudos...")
datos_filtrados_preparados <- readr::read_csv(ruta_csv_clean) %>% 
  filter(año %in% anios_analisis) %>% 
  #convertir a formato largo para el análisis de grupo
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  mutate(dataset = "Filtrado (con NAs)")

#2.2.carga y preparación de datos limpios (imputados) 
message("Cargando y preparando datos limpios")
datos_limpios_preparados <- readr::read_csv(ruta_csv_limpio_final) %>% 
  filter(año %in% anios_analisis) %>%
  #conmos a formato largo para el análisis de grupo
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  mutate(dataset = "Limpio (Imputado)")

#2.3.combinación de datasets
df_combinado <- bind_rows(datos_filtrados_preparados, datos_limpios_preparados)


#cálculo de estadísticos descriptivos
message("Calculando estadísticos descriptivos por Dataset, Año y Variable")

df_resumen_estadistico <- df_combinado %>%
  #agrupamos por dataset, año y variable para calcular las métricas de forma independiente
  group_by(dataset, año, variable) %>%
  summarise(
    # Medidas de Tendencia Central
    Media = mean(valor, na.rm = TRUE),
    Mediana = median(valor, na.rm = TRUE),
    Moda = get_mode(valor),
    
    # Medidas de Dispersión
    Desvio_Estandar = sd(valor, na.rm = TRUE),
    Rango_Intercuartilico_IQR = IQR(valor, na.rm = TRUE),
    
    .groups = 'drop' # Quitar agrupación después del resumen
  ) %>%
  #redondeamos a 3 decimales para una mejor presentación
  mutate(across(c(Media, Mediana, Moda, Desvio_Estandar, Rango_Intercuartilico_IQR), ~round(., 3))) %>%
  #ordenamos por variable, año y dataset para fácil comparación
  arrange(variable, año, dataset)


#4.impresión y guardado de resultados

print("RESUMEN DE ESTADÍSTICOS DESCRIPTIVOS (POR AÑO Y VARIABLE)")
print("Comparación: Dataset Filtrado (con NAs) vs. Dataset Limpio (Imputado)")
print("------------------------------------------------------------------")
print(df_resumen_estadistico)
print("------------------------------------------------------------------")

#guardamos la tabla de resultados
ruta_tabla_resumen <- file.path(dir_outputs_tables, "resumen_estadisticos_anual.csv")

tryCatch({
  readr::write_csv(df_resumen_estadistico, file = ruta_tabla_resumen)
  message(paste("Tabla de resumen estadístico guardada exitosamente en:", ruta_tabla_resumen))
}, error = function(e) {
  message(paste("Error al guardar la tabla:", e$message))
})
