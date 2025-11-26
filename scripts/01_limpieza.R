#                        LIMPIEZA DE DATOS
#           Autoras: Florencia Blanco y Camila Germain

library(tidyverse)
library(WDI)
library(naniar)
library(ggplot2)
library(dplyr)
library(mice)
library(VIM)

proyecto_dir <- here::here()
dir_data_raw      <- file.path(proyecto_dir, "data", "raw")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tables<-file.path(proyecto_dir, "outputs", "tables")
dir_scripts<-file.path(proyecto_dir, "scripts")


#cargamos el CSV guardado en el objeto 'datos_crudos'
ruta_csv_raw <- file.path(dir_data_raw, "datos_crudos.csv") 
datos_crudos <- readr::read_csv(ruta_csv_raw)

#filtramos indicadores y años a analizar
datos_filtrados <- datos_crudos %>% 
  select(-c(iso2c, capital,longitude, latitude, 
            income, lending, status, lastupdated,NE.EXP.GNFS.ZS, NE.IMP.GNFS.ZS)) %>% 
  filter(year%in%c("1990","2001","2010")) %>%filter(region != "Aggregates") %>% 
  rename(apertura_comercial=NE.TRD.GNFS.ZS, pbi_crec=NY.GDP.MKTP.KD.ZG,
         ied_pct_pbi=BX.KLT.DINV.WD.GD.ZS, pais=country, año=year) %>% 
  select(-region)

#ruta
ruta_csv_clean <- file.path(dir_data_clean, "datos_filtrados.csv")

#guardamos csv
write.csv(datos_filtrados, 
          file = ruta_csv_clean, 
          row.names = FALSE)

#para ver la distribucion de los datos faltantes
#contamos NAs por variable
datos_filtrados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) 

#filas completas
sum(complete.cases(datos_filtrados))   #445

#graficos de NA
grafico_NA<-gg_miss_var(datos_filtrados, show_pct = TRUE) +
  labs(title = "NAs por variable",
       x = "Variables",
       y = "Proporción de NAs")

#usamos file.path para guardar en figures
ruta_guardar_grafico_NA <- file.path(dir_outputs_figures, "distribucion_na_por_variable.png")

#guardamos el gráfico
ggsave( filename = ruta_guardar_grafico_NA,
        plot = grafico_NA,
        width = 10,  
        height = 7,  
        dpi = 300    
)

#otro grafico de NAs
ruta_guardar_grafico_ <- file.path(dir_outputs_figures, "distribucion_na_combinacion.png")

#guardamos el grafico
png(
  filename = ruta_guardar_grafico_,
  width = 10, height = 7, units = "in", res = 300
)

NA_combinacion<-aggr(
  datos_filtrados,
  numbers = TRUE,
  prop = FALSE,
  sortVar = TRUE,
  labels = names(datos_filtrados),
  xlab = "Variables",
  ylabs = c("Cantidad de faltantes", "Patrones de combinación")
)
print(NA_combinacion)

#en el grafico se observa que los datos faltantes no son MCAR 
#una gran parte de los datos faltantes proviene de observaciones donde la tasa de 
#crecimiento del PIB (pbi_crec) no estan disponibles, pero el año y el pais estan registrados
#esto sugiere un problema de cobertura de indicadores para ciertos paoses o años

dev.off()

#analisis de distribucion NA
datos_filtrados <- datos_filtrados %>%
  mutate(na_apertura = is.na(apertura_comercial))

#prueba t para ver si el PBI promedio es diferente entre los grupos
#de datos FALTANTES y NO FALTANTES de la Apertura Comercial

t.test(pbi_crec ~ na_apertura, data = datos_filtrados)

#el p value > 0.05, por lo tanto, no rechazamos H0 y no podemos afirmar que son MAR
#la variable observada (crecimiento del PBI) NO es un buen predictor de si la variable clave (apertura_comercial) faltara

#test con IED 

t.test(ied_pct_pbi ~ na_apertura, data = datos_filtrados)

#ahora p value < 0.05, por lo tanto, rechazamos H0. Los datos faltantes de apertura comercial son MAR con respecto a la IED 


#utilizamos la imputacion multiple como estrategia para reemplazar los NA
datos_filtrados <- datos_filtrados %>%
  select(-na_apertura)

#seleccionamos solo las variables relevantes para el modelo MICE
#MICE automaticamente usa las variables completas (country, year) para predecir las faltantes

#ejecutamos la imputacion multiple

imputacion_mice <- mice(
  data = datos_filtrados, 
  m = 5, 
  method = 'pmm', 
  maxit = 10,
  seed = 2025 # Usar una semilla para que el resultado sea reproducible
)

#dataset completo con valores de NA con imputacion simple 
datos_limpios <- complete(imputacion_mice, 1)

ruta_csv_raw <- file.path(dir_data_clean, "datos_limpios.csv")

#guardamos csv
write.csv(datos_limpios, 
          file = ruta_csv_raw, 
          row.names = FALSE)
