#                     EXPLORACION DESCRIPTIVA
#                           19/11/2025 
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

#1.definición de funciones

#validamos si los valores de un vector están dentro de un rango
validar_rango <- function(vector, min_val = -Inf, max_val = Inf) {
  vector >= min_val & vector <= max_val & !is.na(vector)
}

#detectamos y reportamos datos atípicos usando IQR agrupado por año
detectar_atipicos_por_anio <- function(df, columna, nombre_df) {
  
#agrupamos por año y calculamos las estadísticas IQR
df_reporte <- df %>%
group_by(año) %>%
summarise(
   dataset = nombre_df,
   columna = columna,
   n_total = n(),
      
   #1.cálculo de límites IQR por grupo (año)
   Q1 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
   Q3 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
   IQR_val = Q3 - Q1,
   limite_inferior = Q1 - 1.5 * IQR_val,
   limite_superior = Q3 + 1.5 * IQR_val,
      
   #2.conteo de atípicos utilizando los límites calculados
   n_atipicos = sum(
   .data[[columna]] < limite_inferior | .data[[columna]] > limite_superior,
   na.rm = TRUE
   ),
      
   #3.cálculo de porcentaje
   porcentaje = round(100 * n_atipicos / n_total, 2),
   ) %>%

#seleccionamos las columnas de salida finales
select(dataset, año, columna, n_total, n_atipicos, porcentaje, limite_inferior, limite_superior)
  
return(df_reporte) 
}

#2.carga de datos, filtrado y aplicación

#nombres de las variables a analizar (ya renombradas)
columnas_indicadores <- c("apertura_comercial", "pbi_crec", "ied_pct_pbi")

#3.aplicación de la función de detección de valores atípicos (comparación por año)

message("Analizando valores atípicos por año")

#3.1.aplicamos al dataset crudo filtrado (con NAs)
#aplicamos la nueva función a cada columna y luego combinamos los resultados de la lista con bind_rows
resultados_crudos <- lapply(columnas_indicadores, function(col) {
  detectar_atipicos_por_anio(datos_filtrados, col, "Crudo (con NAs)")
})
df_crudos_anual <- bind_rows(resultados_crudos)

#3.2.aplicamos al dataset LIMPIO (sin NAs, imputado)
resultados_limpios <- lapply(columnas_indicadores, function(col) {

#aseguramos que el dataset limpio también este filtrado por los años de interés
detectar_atipicos_por_anio(datos_limpios, col, "Limpio (Imputado)")
})
df_limpios_anual <- bind_rows(resultados_limpios)

#combinamos los resultados finales en un único data frame
df_resultados <- bind_rows(df_crudos_anual, df_limpios_anual)

#4.impresión de resultados
print("REPORTE COMPARATIVO DE VALORES ATÍPICOS (IQR) POR AÑO")
print("Análisis sobre: Apertura Comercial, Crecimiento PBI, IED (% PBI).")
print("-------------------------------------------------------")
print(df_resultados %>% arrange(columna, año, dataset))
print("-------------------------------------------------------")

#5.guardamos la tabla de resultados 
ruta_tabla_outliers <- file.path(dir_outputs_tables, "reporte_atipicos_iqr_anual.csv")

tryCatch({
  readr::write_csv(df_resultados, file = ruta_tabla_outliers)
  message(paste("Tabla de resultados por Año guardada exitosamente en:", ruta_tabla_outliers))
}, error = function(e) {
  message(paste("Error al guardar la tabla:", e$message))
})

#6.visualización de atípicos con boxplots por año (mantenido)
message("Generando Boxplots comparativos por Año para visualizar atípicos")

#6.1.unimos ambos datasets para la visualización comparativa
#usamos el mismo enfoque que antes, pero manteniendo la columna 'year'
datos_crudos_plot <- datos_filtrados %>%
  select(pais, año, all_of(columnas_indicadores)) %>% 
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "Variable", 
    values_to = "Valor"
  ) %>%
  mutate(dataset = "Crudo (con NAs)")

datos_limpios_plot <- datos_limpios %>%
  select(pais, año, all_of(columnas_indicadores)) %>%
  pivot_longer(
    cols = all_of(columnas_indicadores), 
    names_to = "Variable", 
    values_to = "Valor"
  ) %>%
  mutate(dataset = "Limpio (Imputado)")

df_plot <- bind_rows(datos_crudos_plot, datos_limpios_plot)

#6.2.generamos Boxplot comparativo Año vs. Dataset
grafico_atipicos <- df_plot %>%
#aseguramos que 'year' sea factor para el eje X
mutate(
año = as.factor(año),

#aseguramos el orden de los datasets para la leyenda/color
dataset = factor(dataset, levels = c("Crudo (con NAs)", "Limpio (Imputado)"))
) %>%
ggplot(aes(x = año, y = Valor, color = dataset)) + #usamos color para diferenciar datasets
geom_boxplot(
    aes(fill = dataset), #rellenamos por dataset
    outlier.shape = 21, 
    outlier.colour = "red", 
    outlier.fill = "red", 
    position = position_dodge(width = 0.8), #para evitar que las cajas se superpongan
    width = 0.7 #ancho de la caja
  , na.rm=TRUE) + 
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) + #una gráfica por variable
  scale_color_manual(values = c("Crudo (con NAs)" = "#4E79A7", "Limpio (Imputado)" = "#F28E2B")) +
  scale_fill_manual(values = c("Crudo (con NAs)" = "#4E79A7", "Limpio (Imputado)" = "#F28E2B")) +
  labs(
    title = "Comparación de Distribución y Atípicos (IQR) antes y después de Imputación",
    subtitle = "Boxplots por Año: 1990 vs. 2001 vs. 2010",
    x = "Año",
    y = "Valor del Indicador",
    colour= "Dataset",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(grafico_atipicos)

#6.3.guardamos el gráfico
ruta_grafico_atipicos <- file.path(dir_outputs_figures, "boxplot_comparativo_atipicos_por_anio.png")

tryCatch({
  ggsave(filename = ruta_grafico_atipicos, plot = grafico_atipicos, width = 14, height = 7)
  message(paste("Gráfico de boxplots por año guardado exitosamente en:", ruta_grafico_atipicos))
}, error = function(e) {
  message(paste("Error al guardar el gráfico:", e$message))
})

#aumentó la cantidad de casos atípicos con la imputacion multiple

