rm(list = ls(all.names = TRUE))

x =c(1.1, 6.2, 6.09, 6.2, 8.6, 6.4, 0.09, 2.3, 6.66, 5.1, 41.9, 33, 17.7, 55.5, 
     18.25, 50.40, 17.9, 16.74, 12.0, 14.70)
y =c(-1.3, -1.2, -0.52, 2.0, 5.0, 1.,5 -3.89, -1.9, -0.62, 8.9, 4.1, 2, 1.6,
     5.4, 0.82, -0.42, 4.6, -0.25, 2.4, -0.12)

library(MVN)
#I)
cor.test(x,y,method = "pearson", alternative="two.sided")
#r = 0.2559792 (Coeficiente de Pearson)
#II)
cor.test(x,y,method = "kendall")
# tau = 0.2751323 (Coeficiente tau b de Kendall)
#III)
cor.test(x,y,method = "spearman")
#rho = 0.3788563  (Coeficiente de correlación de Spearman (rs))


plot(x,y)
mvn(data=cbind(x,y), mvnTest = "hz") #MVN: NO
mvn(data=cbind(x,y), mvnTest = "mardia") #MVN: NO

#Al parecer la v.a. X y Y son independientes puesto que en el gráfico 
#de dispersión no sepuede observar a simple vista una relación que tenga
#la variable X con Y, ya que se ven muy aleatorios y no se observan un patrón
#en las observaciones.

#Como no hay una asociación monótona (pues los datos crecen y decrecen) y tampoco 
#normalidad bivariada no podemos utilizar la interpetración que nos dan las
#pruebas de kendall, spearman o de pearson.

#Con la observación de los datos en la gráfica concluimos que la v.a. X es independiente
#con respecto a Y
