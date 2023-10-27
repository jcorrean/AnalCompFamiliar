# VA SIN ACENTOS
# Analisis de composicion familiar
library(readxl)
Simon <- read_excel("Datos xa analizar teams.xlsx")
colnames(Simon)[5] <- "Familia"

library(tidyverse)
Simon2 <- Simon %>%
  separate_rows(Familia, sep = ",\\s*") %>%
  mutate(Familia = trimws(Familia))  # Elimina espacios en blanco alrededor de los elementos

library(igraph)
# Figure Panel A
Network <- Simon2

bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "green", "red")
V(bn2)$shape <- ifelse(V(bn2)$type, "square", "circle")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 0.5)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite.projection(bn2)
Sujeto <- bn2.pr$proj2
CompFamiliar <- bn2.pr$proj1
class(Sujeto)
#matw1 <- as.matrix(get.adjacency(Skills))

# Plot the network with node colors based on centrality
plot(CompFamiliar, vertex.label.color = "black", 
     vertex.label.cex = 1, 
     vertex.color = "lightgreen", 
     vertex.size = 6, 
     edge.width = 1, 
     edge.color = "gray50", 
     layout = layout_components, main = "")

plot(Sujeto, vertex.label.color = "black", 
     vertex.label.cex = 1.2, 
     vertex.color = "pink", 
     vertex.size = 40, 
     edge.width = 10, 
     edge.color = "gray30", 
     layout = layout_components, 
     main = "")
