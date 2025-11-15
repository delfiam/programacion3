library(dplyr)
library(ggplot2)
library(caret) 

# Paso 1: Importar datos
datos <- read.csv("data/Titanic_data.csv", sep = ",", header = TRUE)

cat("Estructura original del dataset:\n")
str(datos)

# Paso 2: Limpieza de datos ------------------------------
# Eliminar columnas innecesarias
datos <- datos |> select(-x, -name, -ticket, -home.dest, cabin)

# Verificar valores faltantes
cat("\nCantidad de valores faltantes por variable:\n")
print(colSums(is.na(datos)))

# Eliminar filas con NA
datos <- na.omit(datos)
cat("\nFilas después de eliminar NA:", nrow(datos), "\n")

# Limpieza de variables numéricas (quitamos símbolos o texto, si los hay)
datos$pclass <- as.numeric(gsub("[^0-9.]", "", datos$pclass))
datos$age <- as.numeric(gsub("[^0-9.]", "", datos$age))
datos$sibsp <- as.numeric(gsub("[^0-9.]", "", datos$sibsp))
datos$parch <- as.numeric(gsub("[^0-9.]", "", datos$parch))
datos$fare <- as.numeric(gsub("[^0-9.]", "", datos$fare))

# Conversión de variables categóricas importantes
datos$survived <- as.factor(datos$survived)
datos$sex <- as.factor(datos$sex)
datos$embarked <- as.factor(datos$embarked)
datos$pclass <- as.factor(datos$pclass)



# Resumen general
cat("\nResumen de datos limpios:\n")
print(summary(datos))

# Paso 3: Crear conjuntos de entrenamiento y test ----------------------

set.seed(1234)
entrenaIndex <- createDataPartition(datos$survived, p = 0.8, list = FALSE)
entrenamiento <- datos[entrenaIndex, ]
test <- datos[-entrenaIndex, ]

cat("\nTamaño del conjunto de entrenamiento:", nrow(entrenamiento), "\n")
cat("Tamaño del conjunto de test:", nrow(test), "\n")

# Paso 4: Construir modelo (logístico)
modeloregresion <- glm(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
              data = entrenamiento, family = binomial)

cat("\nResumen del modelo logístico de regresion:\n")
options(scipen = 999)  
print(summary(modeloregresion))

# Paso 5: Predicción y matriz de confusión -----------------------------

predicciones <- predict(modeloregresion, newdata = test, type = "response")
pred_binaria <- ifelse(predicciones > 0.5, 1, 0)
pred_binaria <- as.factor(pred_binaria)

cat("\nMatriz de confusión:\n")
print(confusionMatrix(pred_binaria, test$survived))

print(ggplot(datos, aes(x = sex, fill = survived)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de Supervivencia por Sexo",
       y = "Proporción",
       x = "Sexo") +
  scale_fill_brewer(palette = "Set1"))

print(ggplot(datos, aes(x = pclass, fill = survived)) +
  geom_bar(position = "fill") +
  labs(title = "Supervivencia según Clase",
       y = "Proporción",
       x = "Clase") +
  scale_fill_brewer(palette = "Paired"))
  
datos$grupo_edad <- cut(datos$age,
                        breaks = c(0, 5, 12, 18, 40, 60, 100),
                        labels = c("0-5", "6-12", "13-18", "19-40", "41-60", "60+"))

print(ggplot(datos, aes(x = grupo_edad, fill = survived)) +
  geom_bar(position = "fill") +
  labs(title = "Supervivencia según rango de edad",
       y = "Proporción",
       x = "Grupo de edad") +
  scale_fill_brewer(palette = "Purples"))

