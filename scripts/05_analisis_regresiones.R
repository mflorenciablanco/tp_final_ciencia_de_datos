#                     ANALISIS DE REGRESIONES 
#           Autoras: Florencia Blanco y Camila Germain

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(stats)
library(here)
library(ggfortify)
library(lmtest)      
library(sandwich)

#definicion de directorios
proyecto_dir <- here::here()
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")

#cargamos del dataframe
ruta_csv_limpio <- file.path(dir_data_clean, "datos_limpios.csv")  

#creamos variable logaritmica
df_analisis <- readr::read_csv(ruta_csv_limpio) %>%
  mutate(apertura_comercial_log = log(apertura_comercial))

#llamamos funciones de guardar tablas e imagenes
source(here("scripts", "functions", "guardar_imagenes.R"))
source(here("scripts", "functions", "guardar_tablas.R"))

#seleccionamos solo las variables de interes
datos_reg <- df_analisis %>%
  select(pbi_crec, apertura_comercial)


cat("Tamaño de la muestra efectiva (N) para la regresión:", nrow(datos_reg), "\n\n")

#modelo de regresion lineal simple

# regresion 'lm' 
modelo_ols <- lm(pbi_crec ~ apertura_comercial, data = datos_reg)

cat("--- Resumen del Modelo de Regresión Lineal Simple ---\n")
print(summary(modelo_ols))

# QQ plot de los residuos
residuos <- rstandard(modelo_ols)

#extraer residuos estandarizados
res_est_ols <- rstandard(modelo_ols)

#crear data frame para graficar
df_res_ols <- data.frame(res_est_ols = res_est_ols)

#grafico QQ
qq_plot_residuos_original <- ggplot(df_res_ols, aes(sample = res_est_ols)) +
  stat_qq(color = "#1B4F72", size = 2, alpha = 0.6) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q–Q Plot de Residuos Estandarizados",
    subtitle = "Evaluación del supuesto de normalidad del modelo OLS",
    x = "Cuantiles teóricos",
    y = "Residuos estandarizados",
    caption = "Fuente: Elaboración propia con datos del proyecto"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12))

print(qq_plot_residuos_original)

#El TCL nos dice que, si el tamaño de la muestra "n" es lo suficientemente grande, 
#la distribución muestral de la media tiende a una distribución normal, 
#incluso si la variable original no es normal. sin embargo, vemos que la distribucion 
#de los errores no es normal

#analisis de correlacion: crecimiento del PBI (nivel) vs Apertura Comercial (log) y año

#grafico de dispersion (Scatter Plot)

#visualizamos la relacion Log-Linear 

#la variable de apertura comercial presenta asimetría positiva, 
#aunque por TCL la media se acerca a una distribucion normal,
#aplicar logaritmo hace que se compriman los valores altos, haciendo que la distribucion 
#de la variable se vuelva más simétrica y se acerque más a la normalidad.

#ajuste del modelo OLS con dummies segun periodo (año base 1990)
df_analisis$año <- factor(df_analisis$año)
modelo <- lm(pbi_crec ~ apertura_comercial_log + año, data = df_analisis)

#punto a la derecha del gráfico
x_right <- max(df_analisis$apertura_comercial_log)

#altura de la recta en ese punto
y_right <- coef(modelo)[1] + coef(modelo)[2] * x_right


#el siguiente grafico muestra la relación bivariada entre apertura comercial y crecimiento
#en la sección de regresiones incluimos controles por periodo (dummies de año)
#pero para la visualización gráfica mantenemos la relación simple entre ambas variables.

scatter_plot <- ggplot(df_analisis, aes(x = apertura_comercial_log, y = pbi_crec)) +
  geom_point(alpha = 0.6, color = "#5F9EA0") +
  geom_smooth(method = "lm", se = TRUE, color = "#ff7f0e") +
  # Etiqueta arriba de la recta
  annotate("text",
           x = x_right,
           y = y_right + (max(df_analisis$pbi_crec) - min(df_analisis$pbi_crec)) * 0.05,
           label = "Línea de regresión (OLS)",
           color = "#ff7f0e",
           hjust = 1,
           size = 4) +
  
  labs(
    title = "¿Cuál es la relación entre Crecimiento y Apertura Comercial?",
    subtitle = paste("Correlación de Pearson:",
                     round(cor(df_analisis$apertura_comercial_log,
                               df_analisis$pbi_crec), 3)),
    x = "Logaritmo Natural de Apertura Comercial",
    y = "Crecimiento del PBI (%)",
    caption = "Fuente: Elaboración propia con datos del Banco Mundial"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
    plot.caption = element_text(face = "italic", size = 9)
  )

print(scatter_plot)

ruta_scatter_plot<-file.path(dir_outputs_figures, "correlacion_apertura_crecimiento.jpg")
guardar_imagen(scatter_plot, ruta_scatter_plot)


#heterocedasticidad

#elegimos el Test de White ya que funciona bien incluso si los residuos no son normales
cat("\n===== TEST DE WHITE (BP con términos cuadráticos) =====\n")
bp_white <- bptest(modelo, ~ apertura_comercial_log + año+ I(apertura_comercial_log^2), data = df_analisis)
print(bp_white)

#conclusion: los datos presentan heterocedasticidad por lo que debemos utilizar los errores estandar robustos (errores de White)

#modelo con errores robustos 
cat("\n===== COEFICIENTES OLS CON ERRORES ROBUSTOS (HC1) =====\n")

#summary del modelo con errores robustos 
coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

#en 2010, el crecimiento promedio del PBI es aproximadamente 1.65 puntos porcentuales mayor que en el año base, 
#controlando por apertura comercial
#el resto de los coeficientes son no significativos (salvo el intercepto que no tiene una interpretacion economica clara)


#autocorrelacion (BG)
#dado que los residuos no se distribuyen normalmente utilizaremos el test de BG para 
#analizar autocorrelacion, ya que no requiere normalidad. 
cat("\n===== TEST DE BREUSCH-GODFREY =====\n")
bgtest(modelo, order = 1)

#conclusion: el resultado del BG (p > 0.05) indica que no existe evidencia estadística 
#de autocorrelación en los residuos del modelo

#test de Ramsey (RESET)

#test de Ramsey para evaluar la especificacion del modelo
cat("\n===== TEST DE RAMSEY (RESET) =====\n")
ramsey <- resettest(modelo, power = 2:3, type = "fitted")
print(ramsey)

#conclusion: el modelo esta bien especificado


#cálculo del coeficiente de correlación de Pearson


#el coeficiente de correlación mide la relacion simple entre las variables
#el modelo con dummies controla diferencias sistematicas entre periodos

cat("\n--- Coeficiente de Correlación de Pearson ---\n")

correlacion_pearson <- cor(df_analisis$apertura_comercial_log, df_analisis$pbi_crec)

cat("Correlación (Pearson) entre Log(Apertura Comercial) y Crecimiento PBI:", round(correlacion_pearson, 4), "\n")

cat("(Un valor cercano a 1 o -1 indica una relación lineal fuerte en esta escala Log-Linear)\n")

#prueba de Significancia de la correlacion

cat("\n--- Prueba de Correlación t-test ---\n")

#si el p-valor es bajo (e.g., < 0.05), la correlación es estadísticamente significativa

test_correlacion <- cor.test(df_analisis$apertura_comercial_log, df_analisis$pbi_crec, method = "pearson")

print(test_correlacion)

#conclusion: la correlacion no es estadisticamente significativa

#grafico distribucion de residuos

#extraer residuos estandarizados
res_est <- rstandard(modelo)

#crear data frame para graficar
df_res <- data.frame(res_est = res_est)

#grafico QQ
qq_plot_residuos <- ggplot(df_res, aes(sample = res_est)) +
  stat_qq(color = "#1B4F72", size = 2, alpha = 0.6) +
  stat_qq_line(color = "#ff7f0e", linewidth = 1) +
  labs(
    title = "Q–Q Plot de Residuos Estandarizados",
    subtitle = "Evaluación del supuesto de normalidad del modelo log_linear",
    x = "Cuantiles teóricos",
    y = "Residuos estandarizados",
    caption = "Fuente: Elaboración propia con datos del Banco Mundial (WDI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

#mostrar gráfico
print(qq_plot_residuos)


#conclusion: la distribucion de los residuos no cambio debido a que los residuos dependen 
#exclusivamente del modelo especificado y del comportamiento de la variable
#dependiente, no de la escala de la independiente

