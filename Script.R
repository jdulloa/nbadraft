# Importar datos
library(dplyr)
data <- read.csv("NBAdraft.csv")
# Asegurarnos de que las columnas sean numéricas y limpiar datos
data <- data %>%
  mutate(
    bench = as.numeric(gsub("-", NA, bench)),     # Convertir bench a numérico
    shuttle = as.numeric(gsub("-", NA, shuttle)) # Convertir shuttle a numérico
  )
# Crear variables derivadas
data <- data %>%
  mutate(
    # Índice de explosividad: Relación entre el salto máximo (maxverticalleap) 
    # y la fuerza medida en repeticiones de press de banca (bench).
    # Si maxverticalleap o bench es 0 o NA, asignamos NA.
    explosividad = ifelse(is.na(maxverticalleap) | maxverticalleap == 0, NA, maxverticalleap / bench),
    
    # Relación fuerza-agilidad: Combina la fuerza (bench) con la agilidad lateral (shuttle).
    # Calcula la potencia relativa de un jugador en ejercicios de cambio de dirección.
    fuerzaAgilidad = ifelse(!is.na(bench) & !is.na(shuttle) & shuttle > 0, bench / shuttle, NA),
    
    # Rendimiento de salto: Promedio entre el salto vertical estándar (verticalleap)
    # y el salto máximo (maxverticalleap). Da una idea del rendimiento general en saltos.
    rendimientoSalto = (verticalleap + maxverticalleap) / 2,
    
    # Índice de velocidad: Relación entre la velocidad medida en el sprint de tres cuartos 
    # (threequartersprint) y la carrera en línea recta (lane). Indica eficiencia de velocidad.
    velocidad = ifelse(lane == 0, NA, threequartersprint / lane),
    
    # Diferencia de saltos: Diferencia entre el salto máximo (maxverticalleap) 
    # y el salto estándar (verticalleap). Muestra el rango adicional de explosividad.
    diferenciaSalto = maxverticalleap - verticalleap
  )