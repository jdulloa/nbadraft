# Importar datos
library(dplyr)
data <- read.csv("NBAdraft.csv")
# Asegurarnos de que las columnas sean numéricas y limpiar datos
data <- data %>%
  mutate(
    bench = as.numeric(gsub("-", NA, bench)),     # Convertir bench a numérico
    shuttle = as.numeric(gsub("-", NA, shuttle)) # Convertir shuttle a numérico
  )