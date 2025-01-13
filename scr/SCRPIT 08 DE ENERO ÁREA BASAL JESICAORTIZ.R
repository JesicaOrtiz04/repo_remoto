#EJERCICIO ÁREA BASAL JESICA PAOLA
ej_4<-read.csv("AreaBasal.csv",header=T)#para importar el archivo, primero abrir session, set working directory y choose directory y eliges la carpeta donde esta el archivo
str(ej_4)
head(ej_4)
#AQUI VAMOS A CALCULAR EL ÁREA BASAL DE UN ÁRBOL PROMEDIO DE LA CLASE DIAMETRAL
DAP<-c(2.3,4.5,5.5,6.0,6.4,6.8,7.7,7.9,8.1,9.0)#para generar vector llamado DAP
str(DAP) #str para conocer la estructura del elemento

abt<-3.14*(DAP^2/40000) #abt es area basal del árbol, aquí se establece la ecuación
#lo de abajo es para saber cómo hacerlo para el camino largo
prueba<-abt*ej_4[1,]
abtp1<-round(sum(prueba),digits=2)#area basal de la primera basal de la primera parcela
#de abt a abtp1 es el cuerpo del loop

#CAMINO CORTO
abtpv<-numeric(length(DAP)) #GENERAR VECTOR VACIO
abt<-3.14*(DAP^2/40000)
for(i in 1:length(DAP)){ #para generar loop
  prueba<-abt*ej_4[i,]´#el i es el indicador de la posición del loop para evaluar en este caso del 1 al 10, siempre va
  abtp1<-sum(prueba) #cuerpo de loop, se deben sumar, por eso va el sum
  abtpv[i]<-abtp1#aquí es para guardar en el vector vacio, siempre se debe hacer
}
#con i estoy indicando que va a tomar difrentes números
