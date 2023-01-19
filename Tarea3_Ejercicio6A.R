rm(list = ls(all.names = TRUE))

Muestra= c(0.0023, 0.0150, 0.0298, 0.0337, 0.0729, 0.0943, 0.0950, 0.1080, 0.1180, 
           0.1300, 0.1500, 0.1592, 0.1617, 0.2016,0.2083, 0.2316, 0.2403, 0.2863,
           0.3427, 0.3766, 0.4384, 0.4715, 0.4895, 0.5544, 0.5575, 0.5910, 0.5960, 
           0.6224,0.6517, 0.6602, 0.7197, 0.7317, 0.7687, 0.8212, 0.9439, 1.1242,
           1.2681, 1.2885, 2.3626, 2.6055)

#Por elcurso de inferencia estadística sabemos que el EMV del paramétro
#lambda donde X~Exp(lambda) es lambda_hat=1/ ( sum(x_i)/n )

lambda_hat=1/(sum(Muestra)/40)

# Grafiquemos la Función de distribución estimada propuesta
xplot= seq(0, 10, .01)
yplot= lambda_hat*exp(-lambda_hat*xplot)
yplotr= 1*exp(-1*xplot)

plot(xplot, yplot, type="l")
lines(xplot, yplotr, type="l", col="red")
hist(Muestra, add=TRUE, freq=FALSE)

## Prueba Ji-cuadrada
# Supongamos k=4 clases, donde las clases están determinadas por: 
#(0, 0.3],(0.3, 0.6],(0.6, 1.0],(1.0, ∞).

library(EnvStats)


gofTest(Muestra, test = "chisq", distribution = "exp", cut.points=c(0,.3,.6,1, Inf) )

#A mano

DataX=as.data.frame(Muestra)
k=4
Intervaloi=1:k
DataXkInt=as.data.frame(Intervaloi)
DataXkInt$aimenos1=c(0,0.3,0.6,1)
DataXkInt$ai=c(0.3,0.6,1,Inf)
library(tidyverse)
DataX$Intervaloi <- cut(DataX$Muestra, 
                        breaks=c(DataXkInt$aimenos1,Inf), 
                        labels=c("1","2","3", "4"))
DataXkInt2=DataX %>% group_by(Intervaloi) %>% summarise(Oi=n())
DataXkInt=merge(DataXkInt, DataXkInt2, by=c("Intervaloi"))
DataXkInt$Ei=sum(DataXkInt$Oi)*(pexp(DataXkInt$ai, rate=lambda_hat)-pexp(DataXkInt$aimenos1, rate=lambda_hat))
DataXkInt= DataXkInt%>% mutate(Xi=(Oi-Ei)^2/Ei)
(X=sum(DataXkInt$Xi))
p=1
(qchisq(.95, k-1-p,lower.tail = TRUE))
(pvalue=pchisq(X, k-1-p,lower.tail = FALSE))

#Como p-value = 0.777632 > 0.05 no se rechaza H0, es decir, es plausible asumir
#que las observaciones provienen de una distribución Exp(lambda)
