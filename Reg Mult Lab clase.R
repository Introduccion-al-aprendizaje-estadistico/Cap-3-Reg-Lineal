data<-MyData <- read.csv(file="Advertising.csv", header=TRUE, sep=",")

head(data)

MultLinReg<-lm(formula = data$sales ~ data$TV+ data$radio+ data$newspaper, data = data)
plot(MultLinReg$fitted.values,MultLinReg$residuals)
MultLinReg<-lm(formula = data$sales ~ data$TV+ data$radio, data = data)
plot(MultLinReg$fitted.values,MultLinReg$residuals)
MultLinReg<-lm(formula = data$sales ~ data$TV , data = data)
summary(MultLinReg)
plot(MultLinReg$fitted.values,MultLinReg$residuals)

#############################################################Practica de término de interacción########
Reglin<-lm(formula = data$sales ~ data$TV + data$radio, data = data)

str(RegLin)
summary(Reglin)

Reglin$residuals[Reglin$residuals>0]
head(Reglin$residuals)

newdata<-cbind(datos,Reglin$residuals)
head(newdata)
names(newdata)[names(newdata) == "Reglin$residuals"] <- "Residuales"

head(newdata)

positivos = newdata$Residuales>0
negativos = newdata$Residuales<0

datapositivos = newdata[positivos,]
datanegativos = newdata[negativos,]


datapositivos
datanegativos

plot(datapositivos$TV, datapositivos$radio )
plot(datanegativos$TV, datanegativos$radio )
##########################################################################################
MultLinReg<-lm(formula = data$sales ~ data$TV+data$radio, data = data)
summary(MultLinReg)
plot(MultLinReg$fitted.values,MultLinReg$residuals)

#Sin aditividad
MultLinReg2<-lm(formula = data$sales ~ data$TV+data$radio+data$TV+data$radio*data$TV  , data = data)
summary(MultLinReg2)
plot(MultLinReg2$fitted.values, MultLinReg2$residuals) #Arreglar esta gráfica

anova(MultLinReg,MultLinReg2)
#El p-valor es muy chico, lo que indica que los modelos no ajustan los datos de manera igual, esto es
#El modelo con aditividad lo ajusta mejor.


#Intervalos de confianza y prediccion
head(predict(MultLinReg, data, interval="confidence") )
head(predict(MultLinReg, data, interval="predict") )

confint(MultLinReg, level = 0.95)

#################Fin #######################################
library(MASS)
library(ISLR)

Boston

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
##################################################################################################
##################################################################################################
#Regresion Multiple
lm.fit=lm(medv???lstat+age ,data=Boston )
summary(lm.fit)

lm.fit=lm(medv???.,data=Boston )

lm.fit1=lm(medv???.-age ,data=Boston )
summary(lm.fit1)

summary(lm(medv???lstat *age ,data=Boston ))

#TRansformacion no lineales
lm.fit2=lm(medv???lstat +I(lstat ^2))
summary(lm.fit2)

lm.fit=lm(medv???lstat)
anova(lm.fit,lm.fit2)
#The null hypothesis is that the two models 
#fit the data equally well, and the alternative hypothesis is that the full
# model is superior 
 
par(mfrow=c(2,2))
plot(lm.fit2)


#lm.fit5=lm(medv???poly(lstat ,5))
# summary(lm.fit5)

summary(lm(medv???log(rm),data=Boston ))
#############################CAR SEATS####################################################

#We will now examine the Carseats data, which is part of the ISLR library.
#We will attempt to predict Sales (child car seat sales) in 400 locations
#based on a number of predictors.

fix(Carseats)
names(Carseats)

str(Carseats)

#Shelveloc, an indicator
#of the quality of the shelving location-that is, the space within
#a store in which the car seat is displayed-at each location


head(Carseats)

lm.fit=lm(Sales???.+ Income :Advertising +Price :Age ,data=Carseats )
lm.fit=lm(Sales???.-ShelveLoc-Urban-US  ,data=Carseats )
summary(lm.fit)


