---


##### PRÁCTICA NBA SALARIOS - PREDICCIÓN#####

rm(list = ls())

library(dplyr)
library(tidyverse)
library(ggplot2)

#Importamos y observamos los datos
datos_nba <- read.csv("nba.csv")
datos_nba
dim(datos_nba)
summary(datos_nba)

#Eliminación de NAs
datos_nba <- na.omit(datos_nba)

#Ver si hay jugadores duplicados
duplicated(datos_nba)
nrow(datos_nba[duplicated(datos_nba$Player),])
datos_nba <- datos_nba[!duplicated(datos_nba$Player),]

#Gráficos de comparación entre variables
ggplot(datos_nba, aes(NBA_DraftNumber, Salary)) +
  geom_point(color = "purple")

ggplot(datos_nba, aes(Tm, Salary)) +
  geom_point(color = "green")

ggplot(datos_nba, aes(Age, Salary)) +
  geom_point(color = "blue")

ggplot(datos_nba, aes(MP, Salary)) +
  geom_point(color = "red")

ggplot(datos_nba, aes(G, Salary)) +
  geom_point(color = "yellow")

ggplot(datos_nba, aes(PER, Salary)) +
  geom_point(color = "blue")


#####REGRESIÓN#####

regres01 = lm(Salary~.-Player, data = datos_nba)
summary(regres01)

regres02 = lm(Salary~.-Player -NBA_Country-Tm, data = datos_nba)
summary(regres02)

regres03 = lm(Salary~NBA_DraftNumber+Age+G+MP, data = datos_nba)
summary(regres03)

######CONTRASTE DE NORMALIDAD - qqplot#####
library(car)
qqPlot(regres02, labels=row.names(datos_nba), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


##### HISTOGRAMA + DENSIDAD + NORMAL + RUG#####
#Distribución de errores
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(regres02)

#####JARQUE-BERA#####
library(fBasics)
library(akima)

vResid <- resid(regres01)
jbTest(vResid)

#####SHAPIRO-WILK#####
shapiro.test(vResid)

#####LINEALIDAD#####
crPlots(regres02)
#Si la línea morada se separa mucho de la azul podría haber problemas de
#linealidad. En este caso, no parece. Se ve la relación de cada variable
#con el índice.

#####VARIANZA NO CONSTANTE = HOMOCEDASTICIDAD#####
ncvTest(regres02)
#Rosa debe ir pegada a la línea azul. Si no, hay problemas.


#####VALIDACIÓN GLOBAL#####
#Valida todo de una sola vez
library(gvlma)
gvmodel <- gvlma(regres02)
summary(gvmodel)
#Se ve que no satisface ninguno de los test que hace, salvo el de la 
#heterocedasticidad, que quiere decir que hay homocedasticidad o varianza 
#constante.

#####MULTICOLINEALIDAD#####
vif(regres02)
sqrt(vif(regres02))

#####OUTLIERS#####
outlierTest(regres02)

#####VALORES EXTREMOS#####
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regres02)
#En este gráfico se observan los valores extremos.

#####VALORES INFLUYENTES#####
cutoff <- 4/(nrow(datos_nba)-length(regres02$coefficients)-2)
plot(regres02, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

avPlots(regres02, ask=FALSE, id.method="identify")
#Se hace sobre cada variable, para ver cuál es significativa.

influencePlot(regres02, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

#####BOX-COX#####
forecast::BoxCox.lambda(datos_nba$Salary) 
newData <- forecast::BoxCox(datos_nba$Salary,forecast::BoxCox.lambda(datos_nba$Salary))


#####INTERACIÓN#####
#Cogemos solo algunas variables, no todas. No salen interacciones

regresInter <- lm(Salary~NBA_DraftNumber*	Age*	G*	MP* PER*
                    USG.,data=datos_nba)
summary(regresInter)

#####POLINOMIOS#####
#El resultado parece decir que no tengo un modelo no lineal
regresPoly = lm(Salary~.-Player-NBA_Country-Tm+I(NBA_DraftNumber^2)+I(NBA_DraftNumber^3),data=datos_nba)
summary(regresPoly)

#####SELECCIÓN DE VARIABLES : COMPARANDO MODELOS#####
AIC(regres01,regres02,regres03) #Se elige el menor AIC
BIC(regres01,regres02,regres03) #Se elige el menor BIC


#####MÉTODOS DE SELECCIÓN : BEST SUBSET#####
library (leaps)
regfit.full=regsubsets(Salary~.-Player-NBA_Country-Tm,datos_nba)
reg.summary <- summary(regfit.full)
reg.summary
#Este método consiste en estimar la posibilidad de que sea bueno incluir una
#combinación de variables

reg.summary$rss #Hay que fijarse en el dato más pequeño. En este caso es el 
#número 8


#Ahora realizamos una regresión  con esas variables que han salido, para ver
#si el modelo mejora.

regres04 = lm(Salary~NBA_DraftNumber+Age+G+MP+DRB.+USG.+OWS+VORP, data = datos_nba)
summary(regres04)

#####SELECCIÓN DE VARIABLES : COMPARANDO MODELOS#####
AIC(regres01,regres02,regres03, regres04)
BIC(regres01,regres02,regres03, regres04)

#BACKWARD STEPWISE
library(MASS)

stepAIC(regres02, direction="backward")

regfit.bwd=regsubsets(Salary~.-Player,datos_nba,method ="backward")
summary (regfit.bwd)

stepAIC(regres02, direction="both")


#Creación de regresfinal 
regresfinal = lm(Salary~NBA_DraftNumber + Age + G + MP + PER + X3PAr + ORB. + 
                   TRB. + USG. + WS + OBPM , data = datos_nba)
summary(regresfinal)

#####PREDICCIÓN#####
set.seed(1234)
prediccion <- predict(regresfinal, datos_nba)
prediccion[sample(1:481,10)]
