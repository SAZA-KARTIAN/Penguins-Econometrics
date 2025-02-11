library(readxl)
library(lattice)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(car)
library(tseries)
library(leaps)

# Paso 1: Cargar datos
Datos_Penguins <- read_excel("Datos Penguins.xlsx")
print(summary(Datos_Penguins))
print(str(Datos_Penguins))

# Seleccionar columnas numéricas para análisis estadístico
numeric_data <- Datos_Penguins[sapply(Datos_Penguins, is.numeric)]

# Paso 2: Análisis exploratorio
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Paso 3: Modelos lineales simples
Y <- numeric_data$Y  # Reemplaza con la columna dependiente
X1 <- numeric_data$X1  # Reemplaza con la columna explicativa 1
X2 <- numeric_data$X2  # Reemplaza con la columna explicativa 2
X3 <- numeric_data$X3  # Reemplaza con la columna explicativa 3

m1 <- lm(Y ~ X1)
m2 <- lm(Y ~ X2)
m3 <- lm(Y ~ X3)

plot(X1, Y, main = "Y vs X1", col = "blue", pch = 19)
abline(m1, col = "red")
plot(X2, Y, main = "Y vs X2", col = "green", pch = 19)
abline(m2, col = "red")
plot(X3, Y, main = "Y vs X3", col = "purple", pch = 19)
abline(m3, col = "red")

hist(Y, col = "orange3", main = "Histograma de Y")
hist(X1, col = "orange3", main = "Histograma de X1")
hist(X2, col = "orange3", main = "Histograma de X2")
hist(X3, col = "orange3", main = "Histograma de X3")

boxplot(X1, horizontal = TRUE, main = "Boxplot de X1")
boxplot(X2, horizontal = TRUE, main = "Boxplot de X2")
boxplot(X3, horizontal = TRUE, main = "Boxplot de X3")

# Paso 4: Modelo completo
Datos_Penguins$D1 <- as.factor(Datos_Penguins$D1)
Datos_Penguins$D2 <- as.factor(Datos_Penguins$D2)
Datos_Penguins$D3 <- as.factor(Datos_Penguins$D3)

modelo <- lm(Y ~ X1 + X2 + X3 + D1 + D2 + D3, data = Datos_Penguins)
summary(modelo)

anova_result <- anova(modelo)
print(anova_result)

# Paso 5: Valores atípicos y observaciones influyentes
residuals_standardized <- rstandard(modelo)
print(which(abs(residuals_standardized) > 2))

leverage <- hatvalues(modelo)
influyentes <- which(leverage > (2 * ((ncol(Datos_Penguins) - 1) + 1) / nrow(Datos_Penguins)))
print(influyentes)

cooks <- cooks.distance(modelo)
cooks_cutoff <- 4 / (nrow(Datos_Penguins) - (ncol(Datos_Penguins) - 1))
print(which(cooks > cooks_cutoff))

# Paso 6: Normalidad del error
resid <- resid(modelo)
hist(resid, breaks = 50, col = "lightblue", main = "Histograma de residuales")
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), add = TRUE, col = "red")

jarque_bera_test <- jarque.bera.test(resid)
print(jarque_bera_test)

# Paso 7: Multicolinealidad (VIF)
vif_values <- vif(modelo)
print(vif_values)

# Paso 8: Selección de modelos
fit1 <- lm(Y ~ X1 + X2 + X3 + D1 + D2 + D3, data = Datos_Penguins)
fit2 <- lm(Y ~ 1, data = Datos_Penguins)

step(fit2, direction = "forward", scope = list(lower = fit2, upper = fit1))
step(fit1, direction = "backward")
step(fit1, direction = "both")

# Paso 9: Modelos de todas las regresiones posibles
best_subset <- regsubsets(Y ~ X1 + X2 + X3 + D1 + D2 + D3, data = Datos_Penguins, nbest = 3)
summary_best_subset <- summary(best_subset)
print(as.data.frame(summary_best_subset$outmat))

layout(matrix(1:2, ncol = 2))
subsets(best_subset, statistic = "adjr2", legend = FALSE, main = "Adjusted R^2")
subsets(best_subset, statistic = "cp", legend = FALSE, main = "Mallow Cp")
summary(Majustado)





