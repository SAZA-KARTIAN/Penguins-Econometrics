library(readxl)
Datos_penguins <- read_excel("C:/Users/Sebastian Zuniga/Desktop/Documentos Informe/Datos_penguins.xlsx")
View(Datos_penguins)

#1.Analisis exploratorio de los datos
data=data.frame(Y,X1,X2,X3,D1,D2,D3)
#y= MASA PROMEDIO DEL PINGUINO (g)
#X1= LONGITUD DEL CULMEN (mm)
#X2= PROFUNDIDAD DEL CULMEN (mm)
#X3= LONGITUD DE LA ALETA (mm)
#D1= SEXO (macho o hembra)
#D2= ISLA (1 si es de la isla Torgersen y 0 si no lo es)
#D3= ISLA (1 si es de la isla Dream)

##Categorias base #hembre y # Isla Biscoe


#1.1 Histogramas
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library("Hmisc")

hist(Y,col = "orange3")
hist(X1,col = "orange3")
stripchart(X1, method = "jitter", pch = 19, add = TRUE, col = "blue") #muestra la  distribución de puntos
hist(X2,col = "orange3")
stripchart(X2, method = "jitter", pch = 19, add = TRUE, col = "blue")
hist(X3,col = "orange3")
stripchart(X3, method = "jitter", pch = 19, add = TRUE, col = "blue")
hist(D1,col = "blue")
stripchart(D1, method = "jitter", pch = 19, add = TRUE, col = "red")
hist(D2, col = "bLUE")
stripchart(D2, method = "jitter", pch = 19, add = TRUE, col = "red")
hist(D3, col="blue")
stripchart(D3, method = "jitter", pch = 19, add = TRUE, col = "red")

#gráficos de bigotes
boxplot(X1, horizontal = TRUE, main="Y vs X1", col = "green3")
boxplot(X2, horizontal = TRUE, main ='Y vs X2',col = "green3")
boxplot(X3, horizontal = TRUE, main = 'Y vs X3', col = "green3")


#gráfico Y vs variables explicativas y analisis de correlacion (Pearson)

#modelos 
modelo1=lm(Y~X1)
modelo2=lm(Y~X2)
modelo3=lm(Y~X3)
modelo4=lm(X1~X2)
modelo5=lm(X1~X3)
modelo6=lm(X2~X3)
modelo7=lm(X1~D1)
modelo8=lm(X1~D2)
modelo9=lm(X1~D3)

plot(X1,Y,main="MASA PINGUINO vs LONGITUD DEL CULMEN", col= "blue")#grafico
abline(modelo1, col="red")#linea de MRL
cor.test(X1,Y,alternative = "two.side", method = "pearson")

plot(X2,Y,main="MASA PINGUINO vs PROFUNDIDAD DEL CULMEN",col="blue")
abline(modelo2, col="red")#linea de MRL
cor.test(X2,Y,alternative = "two.side", method = "pearson")

plot(X3,Y,main="MASA PINGUINO vs LONGITUD DE LA ALETA",col="blue")
abline(modelo3, col="red")#linea de MRL
cor.test(X3,Y,alternative = "two.side", method = "pearson")

#Entre variables explicativas#

plot(X1,X2,main="LONGITUD DEL CULMEN vs PROFUNDIDAD DEL CULMEN", col= "blue")#grafico
abline(modelo4, col="red")#linea de MRL
cor.test(X1,X2,alternative = "two.side", method = "pearson")

plot(X1,X3,main="LONGITUD DEL CULMEN vs LONGITUD DEL CULMEN", col= "blue")#grafico
abline(modelo5, col="red")#linea de MRL
cor.test(X1,X3,alternative = "two.side", method = "pearson")

plot(X2,X3,main="LONGITUD DEL CULMEN vs LONGITUD DE LA ALETA ", col= "blue")#grafico
abline( modelo6, col="red")#linea de MRL
cor.test(X2,X3,alternative = "two.side", method = "pearson")


#Construccion de la matriz de correlacion
rcorr(as.matrix(Datos_penguins),type="pearson")
cor(Datos_penguins)
library(corrplot)
mat_cor=cor(Datos_penguins)
corrplot(mat_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

#Criterios de MULTICOLINEALIDAD "VIF"

#MODELO FULL

M1=lm(Y~X1+X2+X3+D1+D2+D3) #para que sea con intecepto
summary(M1)
M1.1=lm(Y~X1+X2+X3+D1+D2+D3-1) #para que sea sin intecepto
summary(M1.1)

library(carData)
library(car)
vif(M1)
# si el VIF es mayor a 5, se dice que existen problemas de multicolinealidad
# en este caso no se presenta entre variables segun el VIF


#Tabla ANDEVA
anova(M1)
M1$residuals #entrega los residuales
M1$coefficients #entrega los coeficientes, SE VE CAMBIO DE SIGNOS EM D2 Y D3


#Observaciones atípicas/influyentes.----

#datos atípicos


#Residuales estunderizados

round(head(rstandard(M1),n=147),d=4)  #residuales estudentizados y 4 es que sean 4 decimales
which(abs(rstandard(M1))>2) #cuales son los atipicos (MAYORES A 2)
sum(abs(rstandard(M1))>2) #cantidad total de atípicos
###de 147 datos existen 11 atipicos que corresponen a los numeros  7  35  41  77  81  89 100 105 115 117 127 

Majustado= lm(Y~X1+X2+X3+D1)

#datos influyentes

#leverage
round(head(hatvalues(M1),n=147),d=4)
hat=abs(hatvalues(M1))
which(hat>(2*((ncol(Datos_penguins)-1)+1)/nrow(Datos_penguins))) #cuales son influyentes
sum(hat>(2*((ncol(Datos_penguins)-1)+1)/nrow(Datos_penguins)))
###de 147 datos existen 4 influyentes que corresponen a los numeros  10  15 110 125 

Majustado= lm(Y~X1+X2+X3+D1)

#distancia de cook
round(head(cooks.distance(M1),n=147),d=4)
cooks=cooks.distance(M1)
which(cooks>4/(nrow(Datos_penguins)-(ncol(Datos_penguins)-1)))
sum(abs(cooks)>4/(nrow(Datos_penguins)-(ncol(Datos_penguins)-1)))#cantidad total de influyentes

cutoff <- 4 / (nrow(Datos_penguins)-(ncol(Datos_penguins)-1))  # Cota
plot(M1, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")   ###genera que fuera pivoyte y afectara a los coef

Majustado= lm(Y~X1+X2+X3+D1)

#distancia de cook  ##sacar los 7 datos influyentes y probar con el modelo ajustado
round(head(cooks.distance(Majustado),n=140),d=4)
cooks=cooks.distance(Majustado)
which(cooks>4/(nrow(Datos_penguins)-(ncol(Datos_penguins)-1)))
sum(abs(cooks)>4/(nrow(Datos_penguins)-(ncol(Datos_penguins)-1)))#cantidad total de influyentes

cutoff <- 4 / (nrow(Datos_penguins)-(ncol(Datos_penguins)-1))  # Cota
plot(Majustado, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")


### Corroborar calculos sacando los datos atipicos e influyentes###




#Calculo de la normalidad del error

#jarque-bera
resid=resid(M1)
histo=hist(resid,breaks=50)
curve(dnorm(x,mean=mean(resid),  sd=sd(resid)), add=TRUE, col="red")
library(tseries)
jarque.bera.test(resid)

library(timeDate)
library(timeSeries)
library(fBasics)
jarqueberaTest(resid(M1))

#ho: error se comporta de forma normal, se acepta ya que el valor del estadistico 0.438<5.991

#valor del chi-cuadrado
qchisq(0.95,2)


#Verificacion si existe Heterocedasticidad en el modelo Y vs X1,X2,X3,D1,D2,D3.

#por medio de:

#Prueba de Goldfend-Quant (c=49).
#Prueba de White.

#heterocedasticidad-----
#h0: hay homocedasticidad/ no hay heterocedasticidad
#ha: no hay homcedasticidad/ hay heterocedasticidad

#Goldfeld-Quandt
#ordenar segun X1
datos <- Datos_penguins[with(Datos_penguins, order(Datos_penguins$X1)), ]
#c=49 c<1/3*n
cjo1 <- datos[-c(49:147), ]
cjo2 <- datos[-c(1:98), ]

model1 = lm(Y~X1+X2+X3+D1+D2+D3, data=cjo1)
model2 = lm(Y~X1+X2+X3+D1+D2+D3, data=cjo2)
anova(model1)
summary(model1)

anova1 =anova(model1)
anova2 =anova(model2)

CME1 = anova1$`Mean Sq`[7]
CME2 = anova2$`Mean Sq`[7]

model1$df.residual
#F(n1-p-1)=F(167-3-1) ##GRADOS DE LIBERTAD## EN ESTE CASO = 41
E = CME2/CME1 
qf(0.95,model1$df.residual,model2$df.residual)#valor de tabla
pf(E, model2$df.residual,model1$df.residual,lower.tail=FALSE) #valores p
E

# E>V.tabla se rechaza ho
## dado que E=1.3979<1.674758 se ACEPTA h0, osea hay homocedasticidad segun la prueba de Goldfeld - Quandt

#white
Err<-resid(M1)
Aux<-lm(I(Err^2)~X1+X2+X3+D1+D2+D3+I(X1^2)+I(X2^2)+I(X3^2)+I(D1^2)+I(D2^2)+I(D3^2)+
          I(X1*X2)+I(X1*X3)+I(X1*D1)+I(X1*D2)+I(X1*D3)+
          I(X2*X3)+I(X2*D1)+I(X2*D2)+I(X2*D3)+
          I(X3*D1)+I(X3*D2)+I(X3*D3)+
          I(D1*D2)+I(D1*D3)+
          I(D2*D3)  ) #27
summary(Aux)
Aux1<-lm(I(Err^2)~X1+X2+X3+D1+I(X1^2)+I(X2^2)+I(X3^2)+
           I(X1*X2)+I(X1*X3)+I(X1*D1)+
           I(X2*X3)+I(X2*D1)+
           I(X3*D1)) #13
summary(Aux1)


Raux <- summary(Aux)$r.squared
E<-147*Raux
E
Raux1 <- summary(Aux1)$r.squared
E1<-147*Raux1
E1

#vatb= chi(0,95,p*(p+3)/2)
Vtab1<-qchisq(0.95,5*(3+5)/2)#malo, YA NO NOS SIRVE POR TENER VARIABLES CUALITATIVAS
Vtab<-qchisq(0.95,27)#correcto
Vtab

E
## el estadistico es menor que el valor de tabla (28.57<40.1132)
##se acepta h0 y hay homocedasticidad

E1
Vtab1<-qchisq(0.95,13)#correcto
Vtab1
## el estadistico es menor que el valor de tabla (11.49<22.3620)
##se acepta h0 y hay homocedasticidad

##2. CONSTRUCCION DE MODELOS

#crear modelos auxiliares, uno con todas las variables y otro con un 1

fit1<-lm(Y~X1+X2+X3+D1+D2+D3, data = Datos_penguins) #poner la base por si hay otra base que tiene variables ocn los mismos nombres y no se sobreescriba
fit2<-lm(Y~1)


#Construccion de MRL para el método "paso a paso ascendente".---- ###hasta tener el menor AIK
step(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))

#Construccion de MRL para el método "paso a paso descendente".----
step(fit1,direction="backward")

step(fit1,direction="both")#metodo en ambas direcciones


#Construccion de MRL para el método de todas las regresiones posibles.----
library(tidyverse)
library(caret)
library(leaps)
best.subset <- regsubsets(Y~X1+X2+X3+D1+D2+D3,data=Datos_penguins,nbest = 4, nvmax=NULL)
summary.out <-summary(best.subset)
as.data.frame(summary.out$outmat)


#COMPARACION de modelos segun criterios

library(tidyverse)
library(caret)
library(leaps)
best.subset <- regsubsets(Y~X1+X2+X3+D1+D2+D3,data=Datos_penguins,nbest = 3, nvmax=NULL)
summary.out <-summary(best.subset)
as.data.frame(summary.out$outmat)

#r2ajustado
summary.out$adjr2 
which.max(summary.out$adjr2)

#cpmallows
summary.out$cp 
which.min(summary.out$cp)

#suma cuadrada de residuos
summary.out$rss 
which.min(summary.out$rss)


#Graficos que muestan la COMPARACION de modelos segun criterios

library(car)
layout(matrix(1:2, ncol = 2))
# Adjusted R2
res.legend <-
  subsets(best.subset, statistic="adjr2", legend = FALSE, min.size = 1, main = "Adjusted R^2")

# Mallow Cp
res.legend <-
  subsets(best.subset, statistic="cp", legend = FALSE, min.size = 1, main = "Mallow Cp")

res.legend <-
  subsets(best.subset, statistic="rss", legend = FALSE, min.size = 1, main = "rss")

summary(Majustado)





