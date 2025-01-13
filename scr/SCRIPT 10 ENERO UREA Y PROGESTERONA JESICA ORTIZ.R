#EJERCICIO UREA GRÁFICAS JESICA PAOLA ORTIZ ARIAS
#1 IMPORTAR DATOS
ej_6<-read.csv("Urea.csv",header=T) #no olvidar session
str(ej_6)#para ver su estructura
head(ej_6)#para mostrar los primeros elementos

#2 GRÁFICA DE DISPERSIÓN con PLOT,con línea de ajuste de Progesterona ~ Urea, sin tomar en cuenta el nivel de dosis
plot(ej_6$Progesterona,ej_6$Urea)
abline(lm(ej_6$Urea~ej_6$Progesterona),col = "blue")

#3 Obtener la media de la variable Urea para cada nivel de dosis 
media_urea<-tapply(ej_6$Urea, ej_6$Dosis,mean,na.rm=T)
print(media_urea)
#3.1Graficar media de urea por nivel de dosis en gráfica de barras
barplot(media_urea,
main = "Media de Urea por Nivel de Dosis", # Título
xlab = "Nivel de Dosis",                 # Nombre del eje X
ylab = "Media de Urea")#Nombre del eje Y
                
#4 Calcular la media, SE e intervalo de confianza por nivel de dosis:la media de progesterona (media ±95% intervalo de confianza) para cada nivel de dosis
library(dplyr)
progesterona_summary <-summarise(
  group_by(ej_6, Dosis),
    Media_Progesterona = mean(Progesterona, na.rm = TRUE),
    SE = sd(Progesterona, na.rm = TRUE) / sqrt(sum(!is.na(Progesterona))),
    IC_inf = Media_Progesterona - qt(0.975,df = sum(!is.na(Progesterona)) - 1) * SE,#Indice de confianza inferior y el qt indica: El cuantil del límite inferior está en el 2.5% (o 0.025 en términos de probabilidad acumulada).El cuantil del límite superior está en el 97.5% (o 0.975 en términos de probabilidad acumulada).
    IC_sup = Media_Progesterona + qt(0.975,df = sum(!is.na(Progesterona)) - 1) * SE
    )
#El resultado de qt(0.975, n() - 1) * SE es el margen de error, que se suma y se resta de la Media_Progesterona para obtener los límites inferior (IC_inf) y superior (IC_sup) del intervalo de confianza.
progesterona_summary
head(progesterona_summary)
ggplot(progesterona_summary, aes(x = as.factor(Dosis), y = Media_Progesterona))
str(progesterona_summary)

#4.1 Gráfica de barras con ggplot 
ggplot(progesterona_summary, aes(x = as.factor(Dosis), y = Media_Progesterona)) +
  geom_col() +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) + # Añadir barras de error
  labs(
    title = "Media de Progesterona por Nivel de Dosis (95% IC)",
    x = "Nivel de Dosis",
    y = "Media de Progesterona"
  ) 

#5 Hacer una grafica de dispersión Progesterona ~ Urea, tomando en cuenta el nivel de dosis
ggplot(ej_6, aes(x = Progesterona, y = Urea, color = as.factor(Dosis))) + 
  geom_point() +  # Con esto se añade los puntos de dispersión
  labs(
    title = "Dispersión de Progesterona vs. Urea por Nivel de Dosis",
    x = "Progesterona",
    y = "Urea",
    color = "Nivel de Dosis"
    )


      
       