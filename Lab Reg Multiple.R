data<-MyData <- read.csv(file="Advertising.csv", header=TRUE, sep=",")

datos<-data[c("radio","TV","sales")]
head(datos)

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
