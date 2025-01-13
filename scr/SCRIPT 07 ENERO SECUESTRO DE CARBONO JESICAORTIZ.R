#SCRIPT 1 PRACTICA (FACTOR) Y TASA DE SECUESTRO DE CARBONO (NUMERICO) JESICA PAOLA ORTIZ ARIAS
practica<-c("laboreo","fertilizacion", "composta")
Practica
Tasadesecuestro<-
  
  #DISTRIBUCIÓN NORMAL
labnorm<-rnorm(200,0.1,0.13)#numero de datos, media y desviación estandar
fertinorm<-rnorm(200,0.9,0.13)
comnorm<-rnorm(200,0.5,0.13)

practica<-("composta")
factor1<-rep(practica,each=200)
composta<-data.frame(factor1,comnorm)#el dataframe fue para unir los 200 datos de cada variable
colnames (composta) <- c("Practica", "Tasa")#se hizo colnames para nombrar las dos columnas

practica<-("fertilizacion")
factor2<-rep(practica,each=200)
fertilizacion<-data.frame(factor2,fertinorm)
colnames (fertilizacion) <- c("Practica", "Tasa")

practica<-("laboreo")
factor3<-rep(practica,each=200)
laboreo<-data.frame(factor3,labnorm)
colnames (laboreo) <- c("Practica", "Tasa")

resultado<-rbind(laboreo,fertilizacion,composta)
boxplot(resultado$Tasa~resultado$Practica)#PARA GRAFICAR EN CAJA DE BIGOTES
