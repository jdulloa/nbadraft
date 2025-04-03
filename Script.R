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
# Ver estadísticas básicas
summary(data)
# Cálculo de estadísticas descriptivas para cada variable clave
descriptivas <- data %>%
  summarise(
    Media_Explosividad = mean(explosividad, na.rm = TRUE),
    Mediana_Explosividad = median(explosividad, na.rm = TRUE),
    Rango_Explosividad = max(explosividad, na.rm = TRUE) - min(explosividad, na.rm = TRUE),
    SD_Explosividad = sd(explosividad, na.rm = TRUE),
    
    Media_Fuerza_Agilidad = mean(fuerzaAgilidad, na.rm = TRUE),
    Mediana_Fuerza_Agilidad = median(fuerzaAgilidad, na.rm = TRUE),
    Rango_Fuerza_Agilidad = max(fuerzaAgilidad, na.rm = TRUE) - min(fuerzaAgilidad, na.rm = TRUE),
    SD_Fuerza_Agilidad = sd(fuerzaAgilidad, na.rm = TRUE),
    
    Media_Rendimiento_Salto = mean(rendimientoSalto, na.rm = TRUE),
    Mediana_Rendimiento_Salto = median(rendimientoSalto, na.rm = TRUE),
    Rango_Rendimiento_Salto = max(rendimientoSalto, na.rm = TRUE) - min(rendimientoSalto, na.rm = TRUE),
    SD_Rendimiento_Salto = sd(rendimientoSalto, na.rm = TRUE)
  )
# Mostrar las estadísticas descriptivas
print(descriptivas)
# Calcular correlaciones entre las variables
cor_data <- data %>%
  select(explosividad, fuerzaAgilidad, rendimientoSalto, velocidad, diferenciaSalto) %>%
  na.omit() %>%
  cor()
print(cor_data)
# Generar gráficos usando la librería de ggplot
library(ggplot2)
# Relación entre explosividad y rendimiento de salto
ggplot(data, aes(x = explosividad, y = rendimientoSalto)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre explosividad y rendimiento de salto",
       x = "Explosividad", y = "Rendimiento de salto")
# Diferencia de saltos relacionada con la fuerza en press de banca
ggplot(data, aes(x = diferenciaSalto, y = bench)) +
  geom_point(color = "red") +
  labs(title = "Relación entre diferencia de saltos y fuerza en press de banca",
       x = "Diferencia de saltos", y = "Repeticiones en press de banca")
# Comparación de velocidad por posición de jugador
ggplot(data, aes(x = position, y = velocidad)) +
  geom_boxplot() +
  labs(title = "Distribución de velocidad según la posición",
       x = "Posición del jugador", y = "Velocidad")