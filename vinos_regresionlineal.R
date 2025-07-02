vinos<-read.csv(file="winequality-red.csv",header=TRUE)
plot(vinos)
str(vinos)

#Ajustamos ahora el modelo de regresión lineal múltiple a las n=1599 observaciones
#que tenemos de las variables, usando la funcion lm, y almacenando el resultado en 
#un objeto con nombre mod1
mod1<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides
         +free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,vinos)
mod1

#El resultado nos muestra los coeficientes estimados, de donde los valores estimados
#de la calidad de cada vino se obtienen a partir de la siguiente expresion lineal:

#Y=21.965+0.025x1-1.084x2-0.183x3+0.016x4-1.874x5+0.004x6-0.003x7-17.881x8
# -0.413x9+0.916x10+0.27.620x11

#Los coeficientes estimados de las covariables no son comparables entre sí, ya
#que no tienen todas las mismas unidades de medida. Sí podemos decir que entre 
#las variables que se miden en g/L, x1,...,x5 y x10 la covariable con mayor
#influencia en la calidad del vino es x5, chlorides, la concentracion de cloruro
# de sodio, ya que es la que tiene mayor coeficiente en valor absoluto.
#La que menos inlfuencia tiene sería x1, la acidez no volátil.


#Estudiamos ahora en qué medida las 11 percepciones de forma conjunta consiguen
#describir la calidad del vino, midiendo la bondad del ajuste.
#Obtengamos entonces el coeficiente de determinacion, R^2
#(porcentaje de la varianza de la v. de respuesta que queda xplicado por el 
#modelo ajustado)

summary(mod1)
mod3<-lm(quality~fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+density+pH,vinos)

summary(mod3)
#Elcoeficiente de regresion multiple es 0.3606
#El ajuste es un culo


#Estudiamos la influencia individual que cada una de las 11 percepciones
#consideradas tienen en la calidad del vino.
#Se trata de 11 problemas de contraste de hipótesis donde en cada caso,
#rechazar la hipótesis nula supondrá concluir que la percepcion corrrespondiente
# en cada caso tiene influencia significativa en la fidelidad
#Por el contrario, si no rechazamos la hipotesis nula, la conclusion es que
#podemos reducir el modelo eliminando dichas covariables.

#Observando el summary vemos que hay 5 variables que podrían rechazarse.


#Antes de reducir el modelo debemos realizar un análisis diagnóstico que nos
#permita validar o al menos descartar posibles violaciones de tales hipotesis


#HOMOCEDASTICIDAD: VARIANZA CONSTANTE.
res<-rstandard(mod1)
plot(mod1$fitted.values,res)
plot(vinos$fixed.acidity,res)
plot(vinos$volatile.acidity,res)
plot(vinos$citric.acid,res)
plot(vinos$residual.sugar,res)
plot(vinos$chlorides,res)
plot(vinos$free.sulfur.dioxide,res)
plot(vinos$density,res)
plot(vinos$pH,res)
plot(vinos$sulphates,res)
plot(vinos$quality,res)###




library("lmtest")
dwtest(mod1)

ks.test(res,pnorm)
qqnorm(res)
#la gráfica se acerca a un comportamiento lineal, luego podemos aceptar la 
#hipotesis de normalidad

mod2<-step(mod1)
mod2
summary(mod2)
