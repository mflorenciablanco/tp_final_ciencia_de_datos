#             ANALISIS DISTRIBUCION CAMBIOS ENTRE AÑOS
#           Autoras: Florencia Blanco y Camila Germain

library(readr)
library(dplyr)
library(here)
library(tidyr) 
library(ggplot2) 
library(rstatix) 
library(dunn.test)
library(car)      
library(cowplot)
library(FSA)

#definicion de directorios y carga de datos 

#definicion de directorios
proyecto_dir <- here::here()
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")

#ruta correcta
ruta_csv_limpio <- file.path(dir_data_clean, "datos_limpios.csv") 

df_analisis <- readr::read_csv(ruta_csv_limpio) 

#llamamos funciones de guardar tablas e imagenes
source(here("scripts", "functions", "guardar_imagenes.R"))
source(here("scripts", "functions", "guardar_tablas.R"))

#preparacion de datos
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

#¿Por que aplicamos logaritmo?
#la variable Apertura Comercial (como % del PIB)
#presenta una distribucion fuertemente asimetrica hacia la derecha,con varios valores extremos
#este patron es caracteristico de variables economicas positivas y, por lo tanto, resulta conveniente 
#la aplicacion de una transformacion logaritmica

#analisis visual de distribuciones y supuestos

#1.Boxplot de Apertura Comercial (Escala Original)

boxplot_original <- ggplot(df_analisis, aes(x = periodo_omc, y = apertura_comercial)) +
  geom_boxplot(alpha = 0.7, fill = "darkblue") +
  labs(title = "Apertura Comercial - Escala Original",
       subtitle = "Obsérvese la asimetría positiva y homocedasticidad",
       x = "Periodo",
       y = "en % del PBI") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(boxplot_original)

#2.Boxplot de Apertura Comercial (Escala Logaritmica)

#este muestra como el logaritmo reduce la asimetria
boxplot_log <- ggplot(df_analisis, aes(x = periodo_omc, y = apertura_comercial_log)) +
  geom_boxplot(alpha = 0.7, fill = "darkgreen") +
  labs(title = "Apertura Comercial - Escala Logarítmica (ln)",
       subtitle = "Mejora de la simetría y homocedasticidad",
       x = "Período",
       y = "ln(Apertura Comercial)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(boxplot_log)

#combinamos ambos graficos

#titulo general para el panel combinado

titulo_general <- ggdraw() + 
  draw_label("Comparación de Distribución de Apertura Comercial por Período",
             fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7)) #ajuste el margen izquierdo para el titulo

#caption con fuente
caption_fuente <- ggdraw() +
  draw_label("Fuente: Elaboración propia con datos del Banco Mundial (WDI).",
             x = 0, hjust = 0, size = 10) +
  theme(plot.margin = margin(10, 0, 0, 7))

#combinar los dos boxplots uno al lado del otro (ncol = 2)
grafico_combinado <- plot_grid(boxplot_original, boxplot_log, 
                               ncol = 2, 
                               align = "h", #alinear horizontalmente
                               rel_widths = c(1, 1)) #igual ancho para ambos

#añadimos el titulo general encima de los boxplots
grafico_final <- plot_grid(titulo_general, grafico_combinado, caption_fuente,
                           ncol = 1, 
                           rel_heights = c(0.05, 1) #pequeño espacio para el titulo
                           
)

print(grafico_final)

#guardar grafico llamando la funcion

ruta_boxplot_conjunto<-file.path(dir_outputs_figures, "boxplot_apertura_original_&_log.jpg")
guardar_imagen(grafico_final,ruta_boxplot_conjunto, width = 10, height = 6, dpi = 300)

#3.histograma de la variable original para ver el sesgo

hist_original <- ggplot(df_analisis, aes(x = apertura_comercial)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  labs(title = "Distribución de Apertura Comercial (Original)",
       subtitle = "Típicamente sesgada a la derecha (asimetría positiva)",
       x = "Apertura Comercial (%)",
       y = "Frecuencia",
       caption = "Fuente: Elaboración propia con datos del Banco Mundial") +
  theme_minimal()
print(hist_original)

#guardo histograma llamando la funcion

ruta_histograma<-file.path(dir_outputs_figures, "histograma_apertura_com_original.png")
guardar_imagen(hist_original, ruta_histograma)

#4.analisis estadistico de supuestos

modelo_log <- aov(apertura_comercial_log ~ periodo_omc, data = df_analisis)

#NORMALIDAD DE RESIDUOS
#analizamos normalidad de los residuos
cat("\n--- PRUEBA DE NORMALIDAD (Shapiro-Wilk) ---\n")
cat("Hipótesis nula (H0): La distribucion de los datos es normal.\n")
cat("Si p < 0.05, se rechaza H0 (los datos NO son normales).\n\n")
shapiro_residuos <-shapiro.test(residuals(modelo_log))
print(shapiro_residuos)

#grafico QQ
qqnorm(residuals(modelo_log)); qqline(residuals(modelo_log))

#histograma
hist(residuals(modelo_log),
     main = "Histograma de residuos del ANOVA",
     xlab = "Residuos",
     ylab= "Frecuencia")

#HOMOCEDASTICIDAD
#analizamos homocedasticidad
cat("\n--- PRUEBA DE HOMOCEDASTICIDAD (Levene) ---\n")
cat("Hipótesis nula (H0): Las varianzas son iguales entre los grupos.\n")
cat("Si p < 0.05, se rechaza H0 (las varianzas son DESIGUALES / Heterocedasticidad).\n\n")

#homocedasticidad: Apertura Comercial ORIGINAL
levene_original <- leveneTest(apertura_comercial ~ periodo_omc, data = df_analisis, center = median)
cat("Homocedasticidad (Original):\n")
print(levene_original)

#se observa asimetria en los datos y una presencia de valores extremos, las varianzas no difieren 
#de forma estadisticamente significativa (Test de Levene).

#homocedasticidad: Apertura Comercial LOGARITMICA
levene_log <- leveneTest(apertura_comercial_log ~ periodo_omc, data = df_analisis, center = median)
cat("\nHomocedasticidad (Logarítmica):\n")
print(levene_log)

#Se reduce la asimetria y se mantiene la homocedasticidad 

#Kruskal Wallis
#debido a la presencia de outliers y la no normalidad de los residuos
#nos parece pertinente realizar el test de Kruskal wallis para poder 
#corroborar el cambio en la apertura comercial en los distintos periodos analizados

cat("\n--- TEST DE KRUSKAL-WALLIS (Usando la variable LOG) ---\n")

kruskal_res <- kruskal.test(apertura_comercial_log ~ periodo_omc,
                            data = df_analisis)

#realizamos la Prueba de Dunn

cat("\n--- POST-HOC: PRUEBA DE DUNN (no paramétrica) ---\n")

dunn_res <- dunnTest(apertura_comercial_log ~ periodo_omc,
                     data = df_analisis,
                     method = "bonferroni")  

print(dunn_res)

#guardamos tabla
ruta_dunn <- file.path(dir_outputs_tables, "dunn_apertura_log_omc.csv")
guardar_tabla(dunn_res$res, ruta_dunn)

#conclusion del dunn test: la significatividad cambia cuando realizamos el test no parametrico, 
#en este caso, la diferencia entre 1990 y 2010 sigue siendo significativa y la de 1990 y 2001 no significativa, 
#pero a la hora de comparar 2001 y 2010, aunque por poco, ahora nos da que la diferencia no es significativa 
