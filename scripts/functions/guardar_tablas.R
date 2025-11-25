#                    FUNCION PARA GUARDAR TABLAS  
#           Autoras: Florencia Blanco y Camila Germain

#source("scripts/functions/guardar_tablas.R")

guardar_tabla <- function(df, ruta_archivo) {
  tryCatch({
    readr::write_csv(df, file = ruta_archivo)
    message(paste("??? Tabla guardada exitosamente en:", ruta_archivo))
  },
  error = function(e) {
    message(paste("??? Error al guardar la tabla:", e$message))
  })
}

