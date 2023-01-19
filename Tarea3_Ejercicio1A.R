rm(list = ls(all.names = TRUE))

library(Rfit)

data(quail)

is.factor(quail$treat)

#########################################I)
boxplot(ldl ~ treat, data = quail, col = "white", outline=TRUE, xlab="Tratamiento",
        ylab="LDL")
stripchart(ldl ~ treat, data = quail,
           method = "jitter",
           pch = 19,
           col = 2:5,
           vertical = TRUE,
           add = TRUE)

#Las medias delos tratamentos 1, 3 y 4 son similares en todos los grupos hay un  
#o dos individuos que al parecer no le funcionó el tratamiento por que sobresale en el
#stripchart, por otro lado se puede observar que el tratamiento dos parece tener
#mejores resultados,sin embargo, tiene mucha variabilidad.


##################################### II)

#A partir de la gráfica se observan que es evidente que las medias no se 
#encuentran a mitad de las cajas, y además como se mencionó arriba,hay observaciones
#muy extremas o atípicas que lo que harán es que las colas no sean igual, por lo que
#con el gráfico no se asume normalidad

#Ahora con el lilliefors test aplicado a cada grupo pues es una variable categórica

library(nortest)
lillie.test(quail$ldl[quail$treat=="1"]) #No se rechaza
lillie.test(quail$ldl[quail$treat=="2"]) #Se rechaza
lillie.test(quail$ldl[quail$treat=="3"]) #Se rechaza
lillie.test(quail$ldl[quail$treat=="4"]) #No se rechaza

#Se encontró evidencia en contra de la normalidad en el grupo 2 y 3
#Por lo tanto no se puede asumir normalidad en cada grupo


####################################III)

#Como no podemos asumir normalidad en cada grupo, debemos usar una prueba 
#la prueba de fligner para la homocedasticidad.

fligner.test(ldl ~ treat , data = quail)

#Obtuvimos que el p-value = 0.98076 >0.05. 
#Entonces no rechazamos Ho, es decir, la varianza es similar en los cuatro grupos

############################IV)

#Mediante la prueba Krukal Wallis contrastamos las hipótesis:
#H0:Los grupos tienen la misma mediana vs Ha: Al menos un grupo tiene distinta mediana
#Ya que si difieren las medianas se tendría evidencia en contra de que el LDL es igual
#en todos los grupos

#Distribución exacta sobre la estadística que es función de los rangos
library(PMCMRplus)
kruskalTest(ldl ~ treat, data = quail, dist="KruskalWallis")
kruskal.test(ldl ~ treat , data = quail)  #usando distribución asintótica

#Como obtuvimos que p-value en ambas es menor 0.1, entonces rechazamos la hipótesis nula Ho, 
#Por lo tanto, al menos un tratamiento es diferente en cuento a su mediana, y esto
#implica que los cuatro tratamientos no porporcionan los mismos valores de ldl

##################################V)

#Primero compararamos todos los pares para identificar grupos diferentes
comparativa <- kwAllPairsDunnTest(ldl ~ treat, data = quail)
summary(comparativa)
#sólo se identifica una diferencia significativa entre el tratamiento 2 y el 1

#Ahora comparemos al grupo 2 con el resto de los grupos tomandolo como referencia 
#para obtener información a profundidad y viendo el boxplot podemos ver que el
#grupo dos tiene reduce en promedio más los niveles de ldl que el resto

#Reasignamos los niveles haciendo que el grupo 2 sea la referencia.

quail$treat <- relevel(quail$treat, ref = "2")

#La hipotesis que contrastaremos es:
#H0:La mediana de los grupos 1, 3 y 4 menor o igual que la del grupo 2    vs
#Ha: La mediana de los grupos 1, 3 y 4 mayor que la del grupo 2

comp1 <- kwManyOneDunnTest(ldl ~ treat, data = quail, alternative = c("greater"))
summary(comp1)

#Como en los tres p-values < 0.1 se rechaza H0 y por lo tanto se tiene evidencia
#a favor de que el tratamiento dos reduce más los niveles de ldl que el resto de
#tratamientos
