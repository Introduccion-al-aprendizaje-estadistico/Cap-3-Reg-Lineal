library(MASS)
library(ISLR)


fix(Boston)
names(Boston)
str(Boston)
head(Boston)


#Datos de 506 vecindarios de casas en Boston.
#RESPUESTA: medv (la mediana del valor de las casas) 
#1)rm (numero promedio de cuartos por casa)
#2)age (promedio de edad de las casas)
#3) lstat Porcentaje de los hogares con nivel socioeconomico bajo

#Iniciamos haciendo una regresion lineal con la variable lstat como predictora y
#claro, medv como respuesta

lm.fit=lm(medv???lstat ,data=Boston )
summary(lm.fit)

#Muestra las funciones del objeto lm.fit
names(lm.fit)


coef(lm.fit) #Diferentes formas de llamar a los coeficientes
lm.fit$coefficients #Diferentes formas de llamar a los coeficientes

#Muestra los residuos
lm.fit$residuals

min(lm.fit$residuals)
max(lm.fit$residuals)

#Como corroborar los errores residuales ?
summary(Boston[,"medv"]-lm.fit$fitted.values)

#Intervalos de confianza para los parámetros obtenidos
confint(lm.fit)

#data.frame(lstat=c(5,10,15) )
#Boston[,"lstat"]

#La funcion predict nos 
predict(lm.fit,data.frame(lstat=c(5,10,15) ), interval = "confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15) ), interval = "prediction")

#Asi vemos que el intervalo de 95% de confianza para un valor lstat de 10 es 
#(24.47, 25.63),
#Asi vemos que el intervalo de 95% de predicción para un valor lstat de 10 es 
#(12.828, 37.28)
#Y el valor de repuesta medv es 25.05
#Usamos el intervalo de confianza, lo veremos adelante en clase
#Usamos el intervalo de prediccion, lo veremos adelante en clase


plot(Boston[,"lstat"],Boston[,"medv"])
#Dibujar la linea de regresion lineal
abline(lm.fit)

#Hay algo de evidencia de que la relacion entre las variables es no lineal 

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(Boston[,"lstat"],Boston[,"medv"],col="red")
plot(Boston[,"lstat"],Boston[,"medv"],pch=20)
plot(Boston[,"lstat"],Boston[,"medv"],pch="+")
#pch para diferentes simbolos
plot(1:20,1:20,pch=1:20)

#Ahora de ser posible dividiremos la pantalla 
par(mfrow=c(2,2))
plot(lm.fit)


#head(predict(lm.fit)) Es lo mismo predict and fitted values
#head(lm.fit$fitted.values)


plot(predict(lm.fit), residuals(lm.fit))
#plot(predict(lm.fit), rstudent(lm.fit))
# plot(hatvalues(lm.fit))
#  which.max(hatvalues(lm.fit))
