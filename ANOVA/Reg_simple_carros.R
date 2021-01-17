#BORRAR MEMORIA
rm(list=ls(all=TRUE))

#LIMPIAR CONSOLA
#ctrl+l

#ELEGIR DIRECTORIO DE TRABAJO

library(lmtest)
library(car)
library(randtests)
library(normtest)


datos<-read.csv(file=file.choose(),header=T,sep=';')
attach(datos)
head(datos)

#TEMA 1: REGRESIÓN SIMPLE
#REGRESIÓN SIMPLE
#Y ES EL mpg y X ES EL TAMAÑO (LENGHT)

#NORMALIDAD: como n>30 la prueba que aplica es la K-S 
#Probar si (Y) es normal 
ks.test(mpg,pnorm,mean(mpg),sd(mpg))


#Estimo el modelo
mpg_simple <- lm(mpg ~ length) 
summary(mpg_simple)
plot(mpg ~ length)

#VALIDO SUPUESTOS
#1. Especificación
reset(mpg_simple)

#2. Homosedasticidad
#Goldfeld-Quandt Test
gqtest(mpg_simple)

#3. No autocorrelación: residuos independientes
residuos<-residuals(mpg_simple)
residuos
plot(residuos)+ abline(h=0, col="red")
runs.test(residuos)

#4. Normalidad de los residuos
ks.test(residuos,pnorm,mean(residuos),sd(residuos))

#SI PASA TODOS LOS SUPUESTOS YA TIENES UN MODELO VÁLIDO
#SI NO, DEBES SOLUCIONARLO
#no pasa el supuesto de No autocorrelación
#SOLUCIÓN

powerTransform(mpg)
mpg_T<-mpg^-0.35
#Probar si el (Y) es normal 
ks.test(mpg_T,pnorm,mean(mpg_T),sd(mpg_T))

#Estimo el nuevo modelo
mpg_T_simple <- lm(mpg_T ~ length) 
summary(mpg_T_simple)
plot(mpg_T ~ length)

#VALIDO SUPUESTOS
#1. Especificación
reset(mpg_T_simple)

#2. Homosedasticidad
#Goldfeld-Quandt Test
gqtest(mpg_T_simple)

#3. No autocorrelación: residuos independientes
residuos<-residuals(mpg_T_simple)
residuos
plot(residuos)+ abline(h=0, col="red")
runs.test(residuos)

#4. Normalidad de los residuos
ks.test(residuos,pnorm,mean(residuos),sd(residuos))

#ahora que pasa todos los supuestos si es un modelo válido
