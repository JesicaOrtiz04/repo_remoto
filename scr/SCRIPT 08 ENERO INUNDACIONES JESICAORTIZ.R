#EJERCICIOS DE INUNDACIONES JESICA PAOLA ORTIZ ARIAS
ej_5<-read.csv("CONTRERAS1.csv",
               header=TRUE,
               encoding = "UTF-8",
               check.names=FALSE) #ESTO SE HACE CUANDO NO ABRE EL ARCHIVO, LO DE UTF y encoding con minusculas
colnames(ej_5[,1])<-"año" #para modificar y que se vea la palabra año porque antes no se escribía bien, en este caso ya no lo use porque guarde la base de datos como csv utf8 


#Generar una nueva variable llamada TIPO a partir de la variable DICTAMEN con IFELSE

ej_5$Tipo<-ifelse(ej_5$DICTAMEN=="ATARJEA OBSTRUIDA","Obstruction",
ifelse(ej_5$DICTAMEN=="COLADERA OBSTRUIDA","Obstruction",
ifelse(ej_5$DICTAMEN=="FALTA DE INFRAESTRUCTURA","Sewage overflow",
ifelse(ej_5$DICTAMEN=="INSUFICIENCIA DE ATARJEA Y COLECTOR","Sewage overflow",
ifelse(ej_5$DICTAMEN=="INSUFICIENCIA DE GRIETA","Sewage overflow",
ifelse(ej_5$DICTAMEN=="RUPTURA DE TUBO DE AGUA POTABLE","Breaking off",
ifelse(ej_5$DICTAMEN=="INEXISTENTE AL MOMENTO DE LA INSPECCION [NO SE APRECIAN DIMENSIONES]","Lacking Info",
ifelse(ej_5$DICTAMEN=="INEXISTENTE AL MOMENTO DE LA INSPECCION","Lacking Info",
ifelse(ej_5$DICTAMEN=="NO SE OPERO CARCAMO DE BOMBEO","Pump Failure",
ifelse(ej_5$DICTAMEN=="HUNDIMIENTO DE CARPETA ASFALTICA","Sinking","Otros"))))))))))#AQUÍ BORRE RESTANTES PORQUE NO HAY UNA CATEGORÍA,Sólo se pone, "otros"
                                    
  #NOTAS
#SE PONEN AL FINAL TODOS LOS PARENTESIS PARA CERRAR CADA UN IFELSE, QUE COINCIDAN LOS PARENTESIS
#TODOS LOS NOMBRES VAN ENTRE COMILLAS
#EL ej_5 es el nombre con el que importe el excel (la base de datos)
#para cambiar el nombre ya sea porque me equivoque y eran mayusculas en lugar de minusculas, es con edit->replace and find y anotar en el cuadro la palabra como la tienes al nombre nuevo que quieres

#CREAR SUBGRUPOS
subgrupo1<-subset(ej_5,Año%in%c(2007,2008,2009)) #subset(nombre de tu archivo,nombre de la columna del año%in%c(año1,año2,año3))
subgrupo1
subgrupo2<-subset(ej_5,Año%in%c(2010,2011,2012, 2013,2014))
subgrupo2

#GENERAR UNA NUEVA VARIABLE FRECUENCIA
ej_5$Frecuencias=1       #PARA GENERAR UNA COLUMNA LLAMADA FRECUENCIAS Y QUE TENGA 1
#Para sumar frecuencias
Frecuencias<-aggregate(Frecuencias~Año+Tipo,data=ej_5,FUN=sum)
print(Frecuencias)#Para visualizar la tabla

#PARA GRAFICAR COMO LO HIZO DOCTORA YOSUNE
boxplot(Frecuencias$Año~Frecuencias$Tipo)#PARA GRAFICAR EN CAJA DE BIGOTES




