#                     DETECCION DE OUTLIERS
#           Autoras: Florencia Blanco y Camila Germain

library(tidyverse)
library(WDI)
library(naniar)
library(ggplot2)
library(dplyr)
library(mice)
library(VIM)
library(readr)
library(here)
library(tidyr) 

proyecto_dir <- here::here()
dir_data_raw      <- file.path(proyecto_dir, "data", "raw")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tables<-file.path(proyecto_dir, "outputs", "tables")
dir_scripts<-file.path(proyecto_dir, "scripts")


#cargamos el CSV guardado en el objeto 'datos_filtrados'
ruta_csv_clean <- file.path(dir_data_clean, "datos_filtrados.csv") 
datos_filtrados <- readr::read_csv(ruta_csv_clean)

#cargamos el CSV guardado en el objeto 'datos_limpios'
ruta_csv_clean <- file.path(dir_data_clean, "datos_limpios.csv") 
datos_limpios <- readr::read_csv(ruta_csv_clean)

#llamamos funciones de guardar tablas e imagenes
source(here("scripts", "functions", "guardar_imagenes.R"))
source(here("scripts", "functions", "guardar_tablas.R"))


#1.definicion de funciones 

#validamos si los valores de un vector estan dentro de un rango
validar_rango <- function(vector, min_val = -Inf, max_val = Inf) {
  vector >= min_val & vector <= max_val & !is.na(vector)
}

#detectamos y reportamos datos atipicos usando IQR agrupado por año
detectar_atipicos_por_anio <- function(df, columna, nombre_df) {
  
  #agrupamos por año y calculamos las estadisticas IQR
  df_reporte <- df %>%
    group_by(año) %>%
    summarise(
      dataset = nombre_df,
      columna = columna,
      n_total = n(),
      
      #1.1.calculo de limites IQR por grupo (año)
      Q1 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      limite_inferior = Q1 - 1.5 * IQR_val,
      limite_superior = Q3 + 1.5 * IQR_val,
      
      #1.2.conteo de atipicos usando los limites calculados
      n_atipicos = sum(
        .data[[columna]] < limite_inferior | .data[[columna]] > limite_superior,
        na.rm = TRUE
      ),
      
      #1.3.calculo de porcentaje
      porcentaje = round(100 * n_atipicos / n_total, 2)
    ) %>%
    
    #seleccionamos las columnas de salida finales
    select(dataset, año, columna, n_total, n_atipicos, porcentaje, limite_inferior, limite_superior)
  
  return(df_reporte)
}

#2.carga de datos, filtrado y aplicacion

#nombres de las variables a analizar 
columnas_indicadores <- c("apertura_comercial", "pbi_crec", "ied_pct_pbi")

#3.aplicacion de la funcion de deteccion de valores atipicos (comparacion por año)

message("Analizando valores atípicos por Año...")


#aplicamos la nueva funcion a cada columna y luego combinamos los resultados de la lista con bind_rows
resultados_crudos <- lapply(columnas_indicadores, function(col) {
  detectar_atipicos_por_anio(datos_filtrados, col, "Crudo (con NAs)")
})
df_crudos_anual <- bind_rows(resultados_crudos)

#aplicamos al dataset LIMPIO (sin NAs, imputado)
resultados_limpios <- lapply(columnas_indicadores, function(col) {
  #nos aseguramos que el dataset limpio tambien este filtrado por los años de interes
  detectar_atipicos_por_anio(datos_limpios, col, "Limpio (Imputado)")
})
df_limpios_anual <- bind_rows(resultados_limpios)

#combinamos los resultados finales en un unico data frame
df_resultados <- bind_rows(df_crudos_anual, df_limpios_anual)

#ordenamos
reporte_atipicos_iqr_anual<-df_resultados %>% arrange(columna, año, dataset)

#4.impresion de resultados

print("--- REPORTE COMPARATIVO DE VALORES ATÍPICOS (IQR) POR AÑO ---")
print("Análisis sobre: Apertura Comercial, Crecimiento PBI, IED (% PBI).")
print("-------------------------------------------------------")
print(df_resultados %>% arrange(columna, año, dataset))
print("-------------------------------------------------------")

#5.guardamos la tabla de resultados

ruta_tabla_outliers <- file.path(dir_outputs_tables, "reporte_atipicos_iqr_anual.csv")
guardar_tabla(reporte_atipicos_iqr_anual, ruta_tabla_outliers)

#6.visualizacion de atipicos con boxplots por año (mantenido)

message("Generando Boxplots comparativos por Año para visualizar atípicos...")

#unimos ambos datasets para la visualizacion comparativa
datos_crudos_plot <- datos_filtrados %>%
  select(pais, año, all_of(columnas_indicadores)) %>% 
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "Variable", 
    values_to = "Valor"
  ) %>%
  mutate(dataset = "Filtrado (con NAs)")

datos_limpios_plot <- datos_limpios %>%
  select(pais, año, all_of(columnas_indicadores)) %>%
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "Variable", 
    values_to = "Valor"
  ) %>%
  mutate(dataset = "Limpio (Imputado)")

df_plot <- bind_rows(datos_crudos_plot, datos_limpios_plot)

#generamos el Boxplot comparativo Año vs. Dataset

#definimos nuevas etiquetas
facet_labels <- c(
  "apertura_comercial" = "Apertura Comercial (índice)", 
  "ied_pct_pbi" = "IED (en % del PBI)",                         
  "pbi_crec" = "Crecimiento PBI (en %)"
)

#creamos el objeto labeller que usaremos en facet_wrap
etiquetas_personalizadas <- as_labeller(facet_labels)

#generamos grafico
grafico_atipicos <- df_plot %>%
  #preparacion y factores
  mutate(
    #aseguramos que 'year' sea factor para el eje X
    año = as.factor(año),
    #aseguramos el orden de los datasets para la leyenda/color
    dataset = factor(dataset, levels = c("Filtrado (con NAs)", "Limpio (Imputado)"))
  ) %>%
  ggplot(aes(x = año, y = Valor, color = dataset)) + # Usamos color para diferenciar datasets
  geom_boxplot(
    aes(fill = dataset), # Rellenamos por dataset
    outlier.shape = 21, 
    outlier.colour = "red", 
    outlier.fill = "red", 
    position = position_dodge(width = 0.8), # Evita que las cajas se superpongan
    width = 0.7, # Ancho de la caja
    na.rm=TRUE
  ) + 
  facet_wrap(
    ~ Variable, 
    scales = "free_y", 
    ncol = 3, 
    labeller = etiquetas_personalizadas # <--- Usa el diccionario de etiquetas
  ) + 
  
  #escalas de color y relleno
  scale_color_manual(values = c("Filtrado (con NAs)" = "#4E79A7", "Limpio (Imputado)" = "#F28E2B")) +
  scale_fill_manual(values = c("Filtrado (con NAs)" = "#4E79A7", "Limpio (Imputado)" = "#F28E2B")) +
  
  #etiquetas y tema
  labs(
    title = "Comparación de Distribución y Atípicos (IQR) antes y después de Imputación",
    subtitle = "Boxplots por Año: 1990 vs. 2001 vs. 2010",
    caption="Fuente: Elaboración propia en base a datos del Banco Mundial",
    x = "Año",
    y = NULL, 
    colour= "Dataset",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

#guardamos el grafico
print(grafico_atipicos)
ruta_grafico_atipicos <- file.path(dir_outputs_figures, "boxplot_comparativo_atipicos_por_anio.jpg")
guardar_imagen(grafico_atipicos, ruta_grafico_atipicos, , width = 14, height = 7)

#aumento la cantidad de casos atipicos con la imputacion multiple. si los datos economicos 
#son naturalmente volatiles,la imputacion multiple imita esa volatilidad, y por eso
#aparecen nuevos outliers