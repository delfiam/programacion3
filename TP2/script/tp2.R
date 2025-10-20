library(dplyr)
library(ggplot2)

datos <- read.csv("data/clientes.csv", sep = ",", header = TRUE) |>
  mutate(
    edad = as.numeric(edad),
    gasto = as.numeric(gasto)
  ) |>
  na.omit()

str(datos)
resultados <- data.frame(k = integer(), tot.withinss = numeric())

set.seed(1234)
for (k in 2:10) {
  modelo <- kmeans(datos[, c("edad", "gasto")], centers = k)
  resultados <- rbind(resultados, data.frame(k = k, tot.withinss = modelo$tot.withinss))
}



print(resultados)

# determinamos mejor k 4 usando el método codo
mejor_k <- 4

#seed así el grupo tiene más o menos el mismo resultado
set.seed(1234)
misGrupos <- kmeans(datos[, c("edad", "gasto")], centers = mejor_k)

head(datos)
print(
  ggplot(resultados, aes(x = k, y = tot.withinss)) +
    geom_point(color = "blue", size = 3) +          # puntos
    geom_line(color = "steelblue", linewidth = 1) + # línea
    ggtitle("Método del codo para k-means") +
    xlab("Número de clusters (k)") +
    ylab("Total withinss") +
    theme_minimal()
)

# agregamos el cluster asignado a cada observación
datos$cluster <- as.factor(misGrupos$cluster)

print(
  ggplot(datos, aes(x = edad, y = gasto, color = cluster)) +
    geom_point(size = 2) +
    ggtitle(paste("Clustering con k =", mejor_k)) +
    xlab("Edad") +
    ylab("Gasto") +
    theme_minimal()
)
# Contar cuántos clusters tienen tamaño 2
tabla_clusters <- table(misGrupos$cluster)
cat("Cantidad de clusters con tamaño 2:", sum(tabla_clusters == 2), "\n")

# Calcular promedio de puntos por cluster
promedio_puntos <- mean(tabla_clusters)
cat("Promedio de puntos por cluster:", promedio_puntos, "\n")

# Imprime un tabla que denota la cantidad de puntos en cada cluster.
print(table(misGrupos$cluster))