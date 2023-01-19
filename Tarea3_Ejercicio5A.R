rm(list = ls(all.names = TRUE))


Frec=c(6,14,29,12,4,7,18,10)
GrupoEt=c("Asiatico", "Negro","Hispano", "Blanco","Asiatico", "Negro","Hispano", "Blanco")
Comportamiento=c(rep("Adecuado",4),rep("No Adecuado",4))
Datos=as.data.frame(cbind(Frec,GrupoEt,Comportamiento))
str(Datos)
Datos$Frec=as.numeric(as.character(Datos$Frec))
Datos$GrupoEt=factor(Datos$GrupoEt)
Datos$Comportamiento=factor(Datos$Comportamiento)
Datos
str(Datos)

#Contrastaremos las siguientes hipótesis:
#H0:GrupoEt y Comportamiento son independientes vs Ha: GrupoEt y Comportamiento
#no son independientes
library(MASS)
reg=loglm(Frec~GrupoEt+Comportamiento, data=Datos)
reg
#En la prueba del cociente de verosimilitud generalizado tenemos un p−value=
#0.8774178>0.01 y de acuerdo con la prueba de JiCuadrada tenemos un p−value=
#0.8772746>0.01. 
#Es decir, no se rechaza H0, por lo tanto, concluimos que GrupoEt y el Comportamiento
#son independientes.
#Así el comportamiento de los individuos después de salir de la cárcel no depende
#del grupo étnico
