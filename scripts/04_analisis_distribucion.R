#             ANALISIS DISTRIBUCION CAMBIOS ENTRE AÑOS
#                           19/11/2025 
#           Autoras: Florencia Blanco y Camila Germain

library(readr)
library(dplyr)
library(here)
library(tidyr) 
library(ggplot2) 
library(rstatix) 
library(dunn.test)
library(car)      

#definición de directorios y carga de datos 

#definición de directorios
proyecto_dir <- here::here()
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")

#ruta correcta
ruta_csv_limpio <- file.path(dir_data_clean, "datos_limpios.csv") 

df_analisis <- readr::read_csv(ruta_csv_limpio) 

#preparación de datos

#aplicamos las transformaciones y preparamos el factor 'periodo_omc'
df_analisis <- df_analisis %>%
  mutate(
    periodo_omc = case_when(
      año == 1990 ~ "A) Base (1990)",
      año == 2001 ~ "B) Adhesión (2001)",
      año == 2010 ~ "C) Post-OMC (2010)",
      TRUE ~ NA_character_ 
    ),
    #aplicamos logaritmo natural (ln) a Apertura Comercial
    apertura_comercial_log = log(apertura_comercial),
    #convertimos a factor ordenado
    periodo_omc = factor(periodo_omc, 
                         levels = c("A) Base (1990)", "B) Adhesión (2001)", "C) Post-OMC (2010)"))
  ) 


#análisis visual de distribuciones y supuestos

#1.Boxplot de Apertura Comercial (Escala Original)
#Revela heterocedasticidad (varianzas desiguales) y outliers
boxplot_original <- ggplot(df_analisis, aes(x = periodo_omc, y = apertura_comercial, fill = periodo_omc)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Apertura Comercial - Escala Original",
       subtitle = "Observese la asimetría y las varianzas diferentes entre grupos",
       x = "Período",
       y = "Apertura Comercial (%)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(boxplot_original)


#2.Boxplot de Apertura Comercial (Escala Logarítmica)
#Muestra cómo el logaritmo estabiliza la varianza y reduce la asimetría
boxplot_log <- ggplot(df_analisis, aes(x = periodo_omc, y = apertura_comercial_log, fill = periodo_omc)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Apertura Comercial - Escala Logarítmica (ln)",
       subtitle = "Mejora de la simetría y varianzas más estables (Homocedasticidad)",
       x = "Período",
       y = "ln(Apertura Comercial)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(boxplot_log)


#3. Histograma de la variable original para ver el sesgo
hist_original <- ggplot(df_analisis, aes(x = apertura_comercial)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Apertura Comercial (Original)",
       subtitle = "Típicamente sesgada a la derecha (asimetría positiva)",
       x = "Apertura Comercial (%)",
       y = "Frecuencia") +
  theme_minimal()
print(hist_original)


#4.análisis estadístico de supuestos

cat("\n--- PRUEBA DE NORMALIDAD (Shapiro-Wilk) ---\n")
cat("Hipótesis nula (H0): La distribución de los datos es normal.\n")
cat("Si p < 0.05, se rechaza H0 (los datos NO son normales).\n\n")

#4.1.Normalidad: Apertura Comercial ORIGINAL
shapiro_original <- df_analisis %>%
  group_by(periodo_omc) %>%
  shapiro_test(apertura_comercial)
cat("Normalidad (Original):\n")
print(shapiro_original)

#4.2.Normalidad: Apertura Comercial LOGARÍTMICA
shapiro_log <- df_analisis %>%
  group_by(periodo_omc) %>%
  shapiro_test(apertura_comercial_log)
cat("\nNormalidad (Logarítmica):\n")
print(shapiro_log)


cat("\n--- PRUEBA DE HOMOCEDASTICIDAD (Levene) ---\n")
cat("Hipótesis nula (H0): Las varianzas son iguales entre los grupos.\n")
cat("Si p < 0.05, se rechaza H0 (las varianzas son DESIGUALES / Heterocedasticidad).\n\n")

#4.3.Homocedasticidad: Apertura Comercial ORIGINAL
levene_original <- leveneTest(apertura_comercial ~ periodo_omc, data = df_analisis, center = mean)
cat("Homocedasticidad (Original):\n")
print(levene_original)

#4.4.Homocedasticidad: Apertura Comercial LOGARÍTMICA
levene_log <- leveneTest(apertura_comercial_log ~ periodo_omc, data = df_analisis, center = mean)
cat("\nHomocedasticidad (LogarÃ­tmica):\n")
print(levene_log)

#código sugerido para el anova de welch (el robusto)
cat("\n--- ANOVA DE WELCH (Usando la variable LOG) ---\n")
#el ANOVA de Welch no asume homocedasticidad, pero funciona mucho mejor
#si la normalidad ha sido mejorada por la transformación

anova_welch <- df_analisis %>%
  welch_anova_test(apertura_comercial_log ~ periodo_omc)
cat("Resultado del ANOVA de Welch (con Apertura Comercial LOG):\n")
print(anova_welch)

cat("\n--- POST-HOC: PRUEBA DE GAMES-HOWELL (Usando la variable LOG) ---\n")

#la prueba de Games-Howell se recomienda porque no asume homocedasticidad
#y es ideal para comparar todas las medias por pares tras el ANOVA de Welch
posthoc_games_howell <- df_analisis %>%
  games_howell_test(apertura_comercial_log ~ periodo_omc)
cat("Resultado de la prueba Post-Hoc de Games-Howell:\n")
print(posthoc_games_howell)


ruta_tabla_games_howell <- file.path(dir_outputs_tables, "games_howell_apertura_omc.csv")
write_csv(posthoc_games_howell, file = ruta_tabla_games_howell)
