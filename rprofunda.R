#red con dos capas y una sola salida.


parametros_aleatorios<-function(n,m,unidades_uno,unidades_dos){
     set.seed(1)
     e<-0.01
     aleatorio1<-rnorm(unidades_uno*n)*e
     aleatorio2<-rnorm(unidades_uno*unidades_dos)*e
     aleatorio3<-rnorm(unidades_dos)*e
     W1<-matrix(aleatorio1,nrow = unidades_uno,ncol = n)
     W2<-matrix(aleatorio2,nrow = unidades_dos,ncol = unidades_uno)
     W3<-matrix(aleatorio3,nrow = 1,ncol = unidades_dos)
     b1<-matrix(0,unidades_uno,1)
     b2<-matrix(0,unidades_dos,1)
     b3<-matrix(0,1,1)

     #-----------
     b1<-c(b1)
     b2<-c(b2)
     b3<-c(b3)

     lista0<-list(W1,W2,W3,b1,b2,b3)
     return(lista0)



}

PHD<-function(X,parametros,m){

     W1<-parametros[[1]]
     W2<-parametros[[2]]
     W3<-parametros[[3]]
     b1<-parametros[[4]]
     b2<-parametros[[5]]
     b3<-parametros[[6]]
     #--------------------------------------------------


     Z1<-W1%*%X+b1
     A1<-tanh(Z1)

     Z2<-W2%*%A1+b2
     A2<-tanh(Z2)

     Z3=W3%*%A2+b3
     A3<-sigmoid(Z3)

     lista1<-list(A1,A2,A3)
     return(lista1)


}




PHDE<-function(parametros,lista1,X,Y){



     W1<-as.matrix(parametros[[1]])
     W2<-as.matrix(parametros[[2]])
     W3<-as.matrix(parametros[[3]])
     A1<-as.matrix(lista1[[1]])
     A2<-as.matrix(lista1[[2]])
     A3<-as.matrix(lista1[[3]])
     #------------------------------------
     dZ3<-A3-t(y)
     dW3<-1/m*dZ3%*%t(A2)
     db3<-1/m*rowSums(dZ3)
     dz2<-t(W3)%*%dZ3*((1-A2^2))
     dW2<-1/m*dz2%*%t(A1)
     db2<-1/m*rowSums(dz2)
     dZ1<-t(W2)%*%dz2*((1-A1^2))
     dW1<-1/m*dZ1%*%t(x)
     db1<-1/m*rowSums(dZ1)
     #----------------------------

    lista2<-list(dW1,dW2,dW3,db1,db2,db3)
     return(lista2)
}

parametros_actualizados<-function(parametros,lista2,learning_rate){



W1<-parametros[[1]]
W2<-parametros[[2]]
W3<-parametros[[3]]
b1<-parametros[[4]]
b2<-parametros[[5]]
b3<-parametros[[6]]

dW1 = lista2[[1]]
dW2 = lista2[[2]]
dW3 = lista2[[3]]
db1 = lista2[[4]]
db2 = lista2[[5]]
db3 = lista2[[6]]



W1 = W1-learning_rate*dW1
b1 = b1-learning_rate*db1
W2 = W2-learning_rate*dW2
b2 = b2-learning_rate*db2
W3 = W3-learning_rate*dW3
b3 = b3-learning_rate*db3


lista3<-list(W1,W2,W3,b1,b2,b3)
return(lista3)


}


predicion1<-function(parametros,X,m){

     cache = PHD(X,parametros,m)
     return(cache[[3]])

}



prediciones<-function(u){

     contador=0
     predictor<-NULL

     for (i in 1:length(y)){
          if (t(u)[i]>=0.5){
               predictor[i]=1
          }
          else{
               predictor[i]=0
          }

     }


     for (i in 1:length(y)){
          if (predictor[i]==y[i]){

               contador=contador+1
          }

     }
     nota<-"Este modelo tiene una efectividad del:"
     porcentage<-round(contador/length(y)*100,2)
     simbolo<-"% en el conjunto de entrenamiento."

     cat(nota,porcentage,simbolo)


}




funcion_test<-function(m_test,x,parametros){

     W1<-parametros[[1]]
     W2<-parametros[[2]]
     W3<-parametros[[3]]
     b1<-parametros[[4]]
     b2<-parametros[[5]]
     b3<-parametros[[6]]
     #--------------------------------------------------


     Z1<-W1%*%x+b1
     A1<-tanh(Z1)

     Z2<-W2%*%A1+b2
     A2<-tanh(Z2)

     Z3=W3%*%A2+b3
     A3<-sigmoid(Z3)


     return(A3)


}





prediciones_test<-function(u,y_test){
     contador=0
     predictor<-NULL

     for (i in 1:length(y_test)){
          if (t(u)[i]>=0.5){
               predictor[i]=1
          }
          else{
               predictor[i]=0
          }

     }
     for (i in 1:length(y_test)){
          if (predictor[i]==y_test[i]){

               contador=contador+1
          }

     }

     nota<-"Hemos acertado"
     acierto<-contador
     nota2<-"elementos de"
     numero<-length(y_test)
     nota3<-"del conjunto de prueba."

     cat(nota,acierto,nota2,numero,nota3,round(acierto/numero*100,3),"%")

}




funcion_error<-function(y,estimacion,m){
     error<- t(y)*log(estimacion)+t((1-y))*log(1-estimacion)
     cost<- -sum(error)/m

     return(cost)

}


sigmoid<-function(x){

     s<-1/(1+exp(-x))


}



