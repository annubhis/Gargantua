library(funciones)
library(ggplot2)
library(caTools)
set.seed(1234)

datos1<-read.table("C:/regresion/PPP1.txt")
datosmuestrales<-data.frame(datos1)
names(datos1)[1]<-"V1"
names(datos1)[2]<-"V2"
names(datos1)[3]<-"V3"

cambio<-is.na(datos1) # Pilla a los elementos NA
datos1[cambio] <-0 # transforma de NA a cero


names(datosmuestrales)[1]<-"V1"
names(datosmuestrales)[2]<-"V2"
names(datosmuestrales)[3]<-"V3"
#------------------------------------------------------------------------------------------------------------------------
datosmuestrales$V1<-factor(datosmuestrales$V1,levels = c(1,0), labels = c('Aceptado', 'Rechazado'))
print(ggplot(data = datosmuestrales, aes(x = datosmuestrales$V2, y = datosmuestrales$V3, col = V1)) + geom_point() +
          labs(x = 'x1', y = 'x2') + ggtitle('Conjunto entrenamiento') +
          theme(legend.title=element_blank()) +
          scale_colour_manual(name='', values=c('Aceptado'='black', 'Rechazado'='red')))
#-----------------------------------------------------------------------------------------


X=data.matrix(datos1)  #Se convierte un dataframe a matriz.
X<-X[,-1]  #Se elimina la columna primera.

y=matrix(datos1$V1,nrow =length(datos1$V1),ncol = 1)

x<-t(X)
m=length(y)
n=length(datos1)-1

parametros<-parametros_aleatorios(n,m,unidades_uno = 7,unidades_dos = 5)

iter<-2000

almacen_de_errores<-NULL
almacen_de_iteraciones<-NULL



for (i in 1:iter){

     lista1<-PHD(x,parametros,m)

     lista2<-PHDE(parametros,lista1,x,y)

     parametros<-parametros_actualizados(parametros,lista2,0.11)

     almacen_de_errores[i]=funcion_error(y,lista1[[3]],m)
     almacen_de_iteraciones[i]=i





}

informacion<-data.frame(almacen_de_iteraciones,almacen_de_errores)
names(informacion)<-c("iteraciones","costo")

u<-predicion1(parametros,x)


prediciones(u)



#---------------------------------

#Empieza el test del conjunto ciego.

datos2<-read.table("C:/regresion/PPP2.txt")
names(datos2)[1]<-"V1"
names(datos2)[2]<-"V2"
names(datos2)[3]<-"V3"
X_test=data.matrix(datos2)
X_test<-X_test[,-1]
x_test<-t(X_test)
m_test=length(datos2$V1)
y_test<-data.matrix(datos2$V1)

uu<-funcion_test(m_test,x_test,parametros)
cat("\n")
cat("Ahora hacemos prediciones en el conjunto test:")
cat("\n")
prediciones_test(uu,y_test)


print(qplot(informacion$iteraciones,informacion$costo,xlab = "Numero iteraciones",ylab="Costo"))


