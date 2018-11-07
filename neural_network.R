library(funciones)
library(ggplot2)
library(caTools)
set.seed(1234)

datos1<-read.table("C:/regresion/fotosalisi.txt")

names(datos1)[1]<-"V1"
names(datos1)[2]<-"V2"
names(datos1)[3]<-"V3"

cambio<-is.na(datos1) # Pilla a los elementos NA
datos1[cambio] <-0 # transforma de NA a cero



split <- sample.split(datos1$V1, SplitRatio = 0.80)
training_set <- subset(datos1, split == TRUE)
test_set <- subset(datos1, split == FALSE)

#------------------------------------------------------------------


X=data.matrix(training_set)  #Se convierte un dataframe a matriz.
X<-X[,-1]  #Se elimina la columna primera.

y=matrix(training_set$V1,nrow =length(training_set$V1),ncol = 1)

x<-t(X)
m=length(y)
p=length(training_set)-1


datosmuestrales1<-data.frame(training_set)
names(datosmuestrales1)[1]<-"V1"
names(datosmuestrales1)[2]<-"V2"
names(datosmuestrales1)[3]<-"V3"
datosmuestrales2<-data.frame(test_set)
names(datosmuestrales2)[1]<-"V1"
names(datosmuestrales2)[2]<-"V2"
names(datosmuestrales2)[3]<-"V3"


#-----------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
datosmuestrales1$V1<-factor(datosmuestrales1$V1,levels = c(1,0), labels = c('Aceptado', 'Rechazado'))
print(ggplot(data = datosmuestrales1, aes(x = datosmuestrales1$V2, y = datosmuestrales1$V3, col = V1)) + geom_point() +
           labs(x = 'x1', y = 'x2') + ggtitle('Conjunto entrenamiento') +
           theme(legend.title=element_blank()) +
           scale_colour_manual(name='', values=c('Aceptado'='black', 'Rechazado'='red')))
#-----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
datosmuestrales2$V1<-factor(datosmuestrales2$V1,levels = c(1,0), labels = c('Aceptado', 'Rechazado'))
print(ggplot(data = datosmuestrales2, aes(x = datosmuestrales2$V2, y = datosmuestrales2$V3, col = V1)) + geom_point() +
           labs(x = 'x1', y = 'x2') + ggtitle('Conjunto Test') +
           theme(legend.title=element_blank()) +
           scale_colour_manual(name='', values=c('Aceptado'='black', 'Rechazado'='red')))
#-----------------------------------------------------------------------------------------


numero_de_neuronas<-c(5,3,1)

L<-length(numero_de_neuronas)




#Ahora pasamos la lista W-b a la funcion de propagación hacia delante.
# Tiene que devolver todas las Ai o neuronas de activación.

iter<-20000


learning_rate<-runif(1,0,1)
almacen_de_errores<-NULL
almacen_de_iteraciones<-NULL
lamda<-0.11





     cat("iteraciòn del coeficiente de aprendizaje con valor k=",learning_rate)
     cat("\n")
     W_b<-neuronas_e_iniciacion(numero_de_neuronas,p,0.4)

for (i in 1:iter){
     Ai<-forward_propagation(x,W_b,m,L)
     derivadas<-back_propagation(W_b,Ai,x,y,L)
     W_b<-descenso_gradiente(W_b,derivadas,learning_rate,L)
     u<-predictor(x,W_b,m,L)
     almacen_de_errores[i]=funcion_L2(y,u,m,lamda,W_b,L)
     almacen_de_iteraciones[i]=i




}


     predicciones_futuras(u)
     informacion<-data.frame(almacen_de_iteraciones,almacen_de_errores)
     names(informacion)<-c("iteraciones","costo")
     print(qplot(informacion$iteraciones,informacion$costo,xlab = "Numero iteraciones",ylab="Costo",geom = "line"))


     cat("\n")

     #------------------------------------------------------------------


     Xtest=data.matrix(test_set)  #Se convierte un dataframe a matriz.
     Xtest<-Xtest[,-1]  #Se elimina la columna primera.

     ytest=matrix(test_set$V1,nrow =length(test_set$V1),ncol = 1)

     xtest<-t(Xtest)
     mtest=length(ytest)
     p=length(test_set)-1


     u<-predictor(xtest,W_b,m,L)

     predicciones_testeo(u,ytest)

     cat("\n")



     #-----------------------------------------------------------------






















