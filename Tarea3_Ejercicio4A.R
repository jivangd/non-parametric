rm(list = ls(all.names = TRUE))

Data <- read.csv("Ejercicio4Ex3.csv")
str(Data)
Data$NivEdu=factor(Data$NivEdu)
levels(Data$NivEdu)
Data$FakeNews=factor(Data$FakeNews)
levels(Data$FakeNews)

#Para respondender a la pregunta:
#"¿Se puede decir que a mayor nivel educativo hay menor impacto por las Fake News?"
#Cambiaremos los niveles de menor a mayor de acuerdo al nivel de educación
#y al impacto de las FakeNews
Data$NivEdu <- factor(Data$NivEdu, levels = c("Primaria","Secundaria","Bachillerato",
                                              "Profesional"))
Data$FakeNews <- factor(Data$FakeNews, levels = c("Muy Poco","Poco","Regular","Mucho"))
levels(Data$NivEdu)
rank(Data$NivEdu)
levels(Data$FakeNews)
rank(Data$FakeNews)
#Lo que nos interesa ahora es tener evidencia a favor de que existe una relación monotona
#negativa ya que si lo hay o no respondería a la pregunta
mosaic(~NivEdu+FakeNews, data = Data, split = TRUE, shade = TRUE, legend = TRUE)
#Se logra oservar esta asociación monótona negativa

#Como ambas variables son ordinales solo podremos utilizar las pruebas
#spearman y kendall
cor.test(rank(Data$NivEdu),rank(Data$FakeNews),method = "kendall")
cor.test(rank(Data$NivEdu),rank(Data$FakeNews),method = "spearman")
#Como ambos p-vales <0.05 se rechaza H0, es decir, hay una relación monótona
#Y más aún tanto tao como rho son negativos; por lo tanto hay una relación 
#monótona decreciente, es decir, a mayor nivel educativo,menor impacto por la FakeNews.

      