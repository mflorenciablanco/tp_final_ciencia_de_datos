#                    FUNCION PARA GUARDAR IMAGENES  
#           Autoras: Florencia Blanco y Camila Germain

#source("scripts/functions/guardar_imagenes.R")
guardar_imagen <- function(plot_obj, ruta_archivo, width = 8, height = 6, dpi = 300) {
  tryCatch({
    
    #comprobar si es un objeto ggplot (incluyendo cowplot, que a veces hereda de ggplot 
    #o si requiere la misma función de guardado)
    #hacemos que la condición sea más amplia para capturar objetos combinados de cowplot/ggdraw
    if (inherits(plot_obj, "ggplot") || inherits(plot_obj, "gtable") || inherits(plot_obj, "grob")) {
      
      #usamos ggsave para objetos ggplot y cowplot (gtable/grob)
      ggplot2::ggsave(
        filename = ruta_archivo,
        plot = plot_obj,
        width = width,
        height = height,
        units = "in", # Añadir units es buena práctica
        dpi = dpi
      )
      
    } else {
      
      #si es un gráfico de base R u otro tipo de objeto gráfico
      #ajustamos width y height para usar pulgadas (in) por defecto
      png(filename = ruta_archivo, width = width, height = height, units = "in", res = dpi)
      print(plot_obj)
      dev.off()
    }
    
    message(paste("??????? Imagen guardada exitosamente en:", ruta_archivo))
    
  }, error = function(e) {
    message(paste("??? Error al guardar la imagen:", e$message))
  })
}
