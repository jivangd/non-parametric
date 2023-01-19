rm(list = ls(all.names = TRUE))

Muestra= c(0.0023, 0.0150, 0.0298, 0.0337, 0.0729, 0.0943, 0.0950, 0.1080, 0.1180, 
           0.1300, 0.1500, 0.1592, 0.1617, 0.2016,0.2083, 0.2316, 0.2403, 0.2863,
           0.3427, 0.3766, 0.4384, 0.4715, 0.4895, 0.5544, 0.5575, 0.5910, 0.5960, 
           0.6224,0.6517, 0.6602, 0.7197, 0.7317, 0.7687, 0.8212, 0.9439, 1.1242,
           1.2681, 1.2885, 2.3626, 2.6055)
#¿los datos provienen de la distribución con función de densidad f(x) = 2e^−2x, x > 0?
#Notemos que si la distriución tiene esa función de densidad entonces X~exp(2)
#Entonces F(X ; 2)= 1-e^(−2x)

# Grafiquemos la Función de distribución teórica
# Con lambda = 2
xplot= seq(0, max(Muestra), .01)
yplot= 1- exp(-2*xplot)
plot(xplot, pexp(xplot, rate=2), type="l", ylim=c(0,1),xlab="x", ylab="Fx")

# Grafiquemos ahora la Función de distribución empírica
library(EnvStats)
ecdfPlot(Muestra, discrete=TRUE, add=TRUE)

#La pruea contrastará:
#H0 : los datos provienen de la distribución con función de densidad 
#f(x) = 2e^(−2x), x > 0 vs Ha : los datos no provienen de esa distribución
ks.test(Muestra, "pexp", 2)
#p-value >.05 no se rechaza H0. No hay evidencia en contra de H0.
#Entonces es pausible considerar que los datos vienen de una distribución con 
#función de densidad f(x) = 2e^−2x, x > 0
