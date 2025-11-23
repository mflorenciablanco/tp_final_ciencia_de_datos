#          ORGANIZACION DEL PROYECTO Y DESCARGA DE DATOS
#                           19/11/2025 
#           Autoras: Florencia Blanco y Camila Germain

rm(list = ls())

library(tidyverse)
library(WDI)
library(naniar)
library(ggplot2)

proyecto_dir <- here::here()
dir_data_raw <- file.path(proyecto_dir, "data", "raw")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tables<-file.path(proyecto_dir, "outputs", "tables")
dir_scripts<-file.path(proyecto_dir, "scripts")

#creamos directorios si no existen
dirs_crear <- c(dir_data_raw, dir_data_clean, dir_data_processed,
                dir_outputs_figures, dir_outputs_tables, dir_scripts)

for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

ruta_destino <- file.path(dir_outputs_figures, "00_organizacion.R")
ruta_destino

#obtenemos los datos de paquete WDI
datos_crudos_originales<- WDI(indicator = c("NE.TRD.GNFS.ZS", "NY.GDP.MKTP.KD.ZG", 
                                            "BX.KLT.DINV.WD.GD.ZS", "NE.EXP.GNFS.ZS",
                                            "NE.IMP.GNFS.ZS"), extra=TRUE)
#ruta
ruta_csv_raw <- file.path(dir_data_raw, "datos_crudos.csv")

#guardamos csv
write.csv(datos_crudos_originales, 
          file = ruta_csv_raw, 
          row.names = FALSE)

