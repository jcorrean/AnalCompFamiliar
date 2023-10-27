# VA SIN ACENTOS
# Analisis de composicion familiar
library(readxl)
Simon <- read_excel("Downloads/Datos xa analizar teams.xlsx")
colnames(Simon)[5] <- "Familia"

library(tidyverse)
Simon2 <- Simon %>%
  separate_rows(Familia, sep = ",\\s*") %>%
  mutate(Familia = trimws(Familia))  # Elimina espacios en blanco alrededor de los elementos

 