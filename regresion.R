#Regresion Multiple

regresor<-function(datos){

        library("ggplot2", lib.loc="~/R/win-library/3.3")
        names(datos) <-c("V1","V2")
        cambio<-is.na(datos) # Pilla a los elementos NA
        datos[cambio] <-0 # transforma de NA a cero

        #---------------------------------------
        A=data.matrix(datos)
        A<-A[,-1]
        y=matrix(datos$V1,nrow = length(datos$V1),ncol = 1)
        m=length(y)
        n=length(datos)-1
        p<-ncol(datos)
        A<-cbind(1,A)
        X<-A
        Z<-solve(t(X)%*%X)
        theta=Z%*%t(X)%*%y
        estimacion<-X%*%theta
        if (p==2){


        dibujo<-qplot(x=datos$V2,y=datos$V1,xlab = "x",ylab = "y",main = " Recta de regresion")
        dibujo<-dibujo+geom_smooth(method = "lm",formula =y~ x )
        hist(A)
        print(dibujo)
        }



        for (i in 1:length(theta)){
                print("Los coeficientes son:")
                print((theta[i]))

        }



        residuo<-y-estimacion
        tabla<-data.frame(y,estimacion,residuo)
        names(tabla)<-c("Observacion","Estimacion","Residuo")
        View(tabla)
        dibujo2<-qplot(estimacion,residuo,main = "Residuos")
        print(dibujo2)
        H=X%*%Z%*%t(X)
        Hdiag<-diag(H)

        mediay<-mean(y)

        VE<-sum((estimacion-mediay)**2)
        VT=VE+sum(residuo**2)

}







sigmoid<-function(x){



     return(1/(1+exp(-x)))


}
