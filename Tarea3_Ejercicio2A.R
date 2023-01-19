rm(list = ls(all.names = TRUE))

Paciente=c(1:9)
x=c(1.83,3.06,1.3,0.5,1.62,2.48,1.68,1.88,1.55)
y=c(0.878,3.14,1.29,0.647,0.598,2.05,1.06,1.29,1.06)
(Data=as.data.frame(cbind(Paciente, x, y)))

####################I
#PRUEBA PARAMÉTRICA

# Se quiere ver si ha sido efectivo el tratamiento, es decir, si la media
#en x es mayor que la media de y.
#Lo que nos interesa lo ponemos en la alternativa.

# H0: M_x<=M_y vs H_a: M_x>M_y
# H0: M_w<=0 vs M_w>0
# H0: b0<=0 vs b0>0
#Pues w=x-y
t.test(Data$x,Data$y,paired = TRUE, alternative="greater")
# Como p-value < alpha=.05 se rechaza H0, es decir, se puede concluir que el
#tratamiento ha tenido éxito.
ggpaired(Data, cond1 = "x", cond2 = "y",
         color = "condition", line.color = "gray", line.size = 0.4)+
  stat_compare_means(method = "t.test", paired=TRUE, method.args = list(alternative = "less"))

###################II
#PRUEBA NO PARAMÉTRICA
#Tenemos las mismas hipótesis que se contratastaron en la prueba paramétrica
wilcox.test(Data$x,Data$y,paired = TRUE, alternative = c("greater"), exact = TRUE, correct = FALSE)
wilcox.test(Data$x,Data$y,paired = TRUE, alternative = c("greater"), exact = FALSE, correct = TRUE)
ggpaired(Data, cond1 = "x", cond2 = "y",
         color = "condition", line.color = "gray", line.size = 0.4)+
  stat_compare_means(method = "wilcox.test", paired=TRUE, method.args = list(alternative = "less"))
# Como p-value < alpha=.05 tanto en la exacta como en la aproximación a la distribución
#normal se rechaza H0, es decir, se puede concluir que el tratamiento ha tenido éxito.