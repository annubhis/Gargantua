  #Red profunda general con L capas y una sola salida.
#Aprendiendo R.Yo soy matemático no ingeniero por las chapuzas que voy hacer...

neuronas_e_iniciacion<-function(x,p,e){


     #Crea los distintos pesos W y las bias b
     set.seed(1234)

     L<-length(x)
     W<-list(NULL)
     b<-list(NULL)
     aleatorios<-list(NULL)



     for (i in 1:L){


          if (i==1){

               aleatorios[[i]]=runif(x[i]*p,-1,1)*e
               W[[i]]=matrix(aleatorios[[i]],nrow = x[i],ncol = p)
               b[[i]]=matrix(0,nrow = x[i],ncol = 1)
               b[[i]]=c(b[[i]])

          }


          else {

               aleatorios[[i]]=rnorm(x[i]*x[i-1])*e
               W[[i]]=matrix(aleatorios[[i]],nrow = x[i],ncol = x[i-1])
               b[[i]]=matrix(0,nrow = x[i],ncol = 1)
               b[[i]]=c(b[[i]])
          }
          if (i==L){

               aleatorios[[i]]=rnorm(x[i]*x[i-1])*e
               W[[i]]=matrix(aleatorios[[i]],nrow = 1,ncol = x[i-1])

          }


     }

     lista0<-list(W,b)

}


#---------------------------------------------------------------


forward_propagation<-function(x,Wb,m,L){



     Z<-list(NULL)
     A<-list(NULL)

     for (i in 1:L){

          if (i==1){

               Z[[i]]=Wb[[1]][[i]]%*%x+Wb[[2]][[i]]
               A[[i]]=tanh(Z[[i]])

          }
          else {
               Z[[i]]=Wb[[1]][[i]]%*%A[[i-1]]+Wb[[2]][[i]]
               A[[i]]=tanh(Z[[i]])
          }

          if (i==L){

              A[[i]]=sigmoid(Z[[i]])
          }
}


     return(A)


}

#----------------------------------------------------


back_propagation<-function(Wb,Ai,x,y,L){

 dz<-list(NULL)
 dw<-list(NULL)
 db<-list(NULL)

 for (i in L:1){
      if (i==1){
           dz[[i]]=t(Wb[[1]][[i+1]])%*%dz[[i+1]]*(1-Ai[[i]]^2)
           dw[[i]]=1/m*dz[[i]]%*%t(x)
           db[[i]]=1/m*rowSums(dz[[i]])
      }
      else {
           if (i==L){

                dz[[i]]=Ai[[i]]-t(y)
                dw[[i]]=1/m*dz[[i]]%*%t(Ai[[i-1]])
                db[[i]]=1/m*rowSums(dz[[i]])
           }
           else{
                dz[[i]]=t(Wb[[1]][[i+1]])%*%dz[[i+1]]*(1-Ai[[i]]^2)
                dw[[i]]=1/m*dz[[i]]%*%t(Ai[[i-1]])
                db[[i]]=1/m*rowSums(dz[[i]])
           }
      }

}

 lista1<-list(dw,db,dz)
 return(lista1)


}

#-------------------------------------------------------




descenso_gradiente<-function(Wb,derivadas,learning_rate,L){



     w<-list(NULL)
     b<-list(NULL)

     for (i in 1:L){

        w[[i]]=  Wb[[1]][[i]]-learning_rate*derivadas[[1]][[i]]
        b[[i]]=  Wb[[2]][[i]]-learning_rate*derivadas[[2]][[i]]

     }

     lista2<-list(w,b)
     return(lista2)

}

#---------------------------------------------------


predictor<-function(x,W_b,m,L){

     cache = forward_propagation(x,W_b,m,L)
     return(cache[[L]])

}


#----------------------------------------------------



predicciones_futuras<-function(u){

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

#----------------------------------------


predicciones_testeo<-function(u,y_test){
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



#-----------------------------------------------


funcion_L2<-function(y,estimacion,m,lamda,W_b,L){

          cuadrados<-list(NULL)

          for (i in 1:L){
               cuadrados[[i]]=(W_b[[1]][[i]])*W_b[[1]][[i]]
               c<-c(cuadrados[[i]])
          }

          suma<-sum(c)
          error<- t(y)*log(estimacion)+t((1-y))*log(1-estimacion)
          a<-lamda/(2*m)*suma
          cost<- -sum(error)/m

          return(cost+a)

     }



l2_cost<-function(Y,estimacion){


     a<-mean((Y-estimacion)^2)
     return(a)
}

