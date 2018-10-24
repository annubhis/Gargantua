from modulos.Matriz import Matrices
from modulos.Matrizesdearchivo import M
import numpy as np
from tabulate import tabulate
import matplotlib.pyplot as plt
import math
from io import open
import pandas as pan
from pandas.plotting import scatter_matrix
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns



def gra(itera,alpha):
    h1=np.zeros(p+1)
    A=np.matrix(h1)
    theta=np.transpose(A)
    
    
    for i in range(0,itera):
        theta=theta-alpha/n*t.XT*(t.X*theta-t.Y)
    
    
    for i in range(0,p+1):
        print(float(theta[i]))
        



print("\n Bienvenido al programa de regresion.\n")



print("-------------------")

print("-------------------")    

print("-------------------")
print("-------------------")



try:
   
    
    print("Datos por archivo o por consola?")
    elegir=int(input("\n Archivo --- pulsa 0, consola---pulsa 1 \n"))
    if elegir==0:
        t=M()
        n=t.muestra()
        p=(t.variables()-1)
        Z=t.XT*t.X
        invZ=Z.I
        beta=invZ*t.XT*t.Y
        estimacion=t.X*beta
        estimacionT=np.transpose(estimacion)
        H=t.X*invZ*t.XT
        e=t.Y-estimacion
        et=np.transpose(e)
        varianzaresidual=float(et*e/(n-p-1))
        desviacionresidual=math.sqrt(varianzaresidual)
        plt.hist(t.matri_x())
        plt.grid(True)
        plt.show()
        input("Pulsar para continuar...")
       
       
        print("-----------------------")
        print("Valores posibles  influyentes o atípicos de la matriz H")
        print("-----------------------")
        for i in range(0,n):
            if H[i,i]>0.6:
                print("posicion ", "(",i,")","(",i,")",H[i,i])

        print("-----------------------")
        print("-----------------------")
        print("-----------------------")
        
        print("-----------------------")
        print("-----------------------")
        sumay=0
    
       
        for i in range(0,n):
            sumay+=t.Y[i]

        media_y=sumay/n

        vc=[]
        for i in range(0,p+1):
            vc.append(varianzaresidual*invZ[i,i])
        
        print("-----------------")
        print("Los coeficientes de regresion son: ")
        print(tabulate(np.round(beta,2),headers=["C.regresion"]))
        print("-----------------")
        print("-----------------")
        print("Desviación típica de los regresores")
        print("-----------------")
        for i in range(1,p+1):
            print(np.round(math.sqrt(vc[i]),2))
        
        print("----------------")
        print("La varianza residual es: \n")
        print(np.round(varianzaresidual,2))
        
        a='C:/regresion/student.txt'
        b=open(a,'r')
        lista=b.readlines()
        b.close()
        d=[]

        for i in range(0,lista.__len__()):
            d.append(float(lista[i]))

        if n>20:
            tstudent=2.01
        else:
            tstudent=d[n-p-1]
        print("\n El valor que se fija de la t de Student sera: \n",tstudent)
        print("-----------------")
        print("-----------------")
        print("-----------------")
        TS=[]
        for i in range(0,p):
            TS.append(beta[i]/math.sqrt(vc[i]))
       
        
        for i in range(1,p):
            if math.fabs(TS[i])>tstudent:
                print("El coeficiente Beta",(i)," es significativo al 95%.")
        VNE=float(et*e)
        VT=float(t.YT*t.Y-n*media_y*media_y)
        VE=float(estimacionT*estimacion-n*media_y*media_y)

        F=VE/(p*varianzaresidual)
        f4=[["VE",VE,p,VE/p,F],["VNE",VNE,n-p-1,varianzaresidual],["VT",VT,n-1,VT/(n-1)]]
        print("\n Tabla ADEVA \n")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print(tabulate(f4,headers=['fuente','sc','gl','varianza','Contraste']))
        R2=VE/VT
        print("------------")
        print("------------")
        print("Correlacion")
        print("------------")
        print(np.round(R2,3))
        if p==1:
            beta0=float(beta[0])
            beta1=float(beta[1])
            x1=np.array(t.X[:,1])
            y1=np.array(t.Y)
            plt.title("Nube de puntos y regresión")
            plt.grid(True)
            
            
            plt.xlabel("Variable independiente")
            plt.ylabel("Observaciones") 
            plt.plot(x1,y1,'o',color='red')
            y2=beta0+beta1*x1
            #plt.plot(x1,[beta0+beta1*x3 for x3 in x1])
            plt.plot(x1,y2,color='black')
            plt.show()
        if p==2:
            fig=plt.figure()
            #ax = Axes3D(fig)
            sub=fig.add_subplot(1,1,1,projection="3d")
            x1=np.array(t.X[:,1])
            x2=np.array(t.X[:,2])
            z=np.array(t.Y[0])
            plt.xlabel("x1")
            plt.ylabel("x2")
            plt.title("Nube de puntos y plano de regresión")
            
            #sub.plot(x1,x2,z,'o',color='black')
            sub.scatter(x1 , x2 , z,  color='black')#Con este comando creas los puntos.
            x1,x2 = np.meshgrid(x1, x2) #comando para gráfica de 3D.
            beta0=float(beta[0])
            beta1=float(beta[1])
            beta2=float(beta[2])
            B=beta0+beta1*x1+beta2*x2
            surface=sub.plot_surface(x1,x2,B,color='turquoise')
            #ax.view_init(elev=30., azim=65)
            plt.show()
            
            
        
        
        
        residuos=[]
        Yst=[]
        for i in range(0,n):
            residuos.append(float(e[i]))
            Yst.append(float(estimacion[i]))
        xx=np.array(Yst)
        yy=np.array(residuos)
        plt.title('Residuos frente a estimaciones')
        plt.scatter(xx,yy, color='black')
        x=np.arange(-50,50,0.1)
        plt.xlabel('Estimaciones')
        plt.ylabel('Residuos')
        plt.grid(True)
        plt.show()
        input("Pulsar para continuar...")
        
        
        
        
        
        d1=[]
        d2=[]
        d3=[]
        d4=[]
        d5=[]
        for i in range(0,n):
            d1.append(float(t.Y[i]))
            d2.append(float(estimacion[i]))
            d3.append(float(e[i]))
            d4.append(float(H[i,i]))
            d5.append(float(1/H[i,i]))
        
        ri=[]
        for i in range(0,n):
            ri.append(float(e[i])/(desviacionresidual*math.sqrt(1-float(H[i,i]))))

        
        D=[]
        for i in range(0,n):
            D.append(float(ri[i])**2*float(H[i,i])/((p+1)*(1-float(H[i,i]))))
        
        
        nuevo=pan.DataFrame(
         { 'Observaciones':np.round(d1,2), 'Estimaciones':np.round(d2,2) ,'Residuos':np.round(d3,2),'h':np.round(d4,2)
          ,'1/h':np.round(d5,2),'r':np.round(ri,2),'D(i)':np.round(D,2)}, 
         columns=['Observaciones','Estimaciones','Residuos','h'
                  ,'1/h','r','D(i)'] )
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print(nuevo)
        
        
        X=t.matri_x()
        XT=np.transpose(X)
        vector_medias=1/n*XT*t.mt
        Xchurro=X-t.mt*np.transpose(vector_medias)
        S=1/n*np.transpose(Xchurro)*Xchurro
        r1=np.diagonal(S)
        r2=[]
        In=np.identity(p+1)
        for i in range(0,p+1):
            r2.append(np.round(math.sqrt(r1[i]),2))
        r3=r2*In
        D=np.matrix(r3)
        inD=D.I
        MCORR=inD*S*inD
        Mcorrel=np.round(MCORR,2)
        
        
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("El vector de medias es:\n")
        for i in range(0,p+1):
            print(float(vector_medias[i]))
        
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("La matriz de correlacion es:\n")
        print(Mcorrel)
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("La matriz de dispersión de datos es:\n")
        nuevo1=pan.DataFrame(t.matri_x())
        scatter_matrix(nuevo1)
        plt.show()
        if R2>=0.7:
            print("Parece una prueba significativa.\n")
            print("Vamos a confirmar los coeficientes con el método del gradiente.\n")
            print("-----------------")
            print("-----------------")
            print("-----------------")
            print("primera iteración\n")
            gra(500,0.01)
            print("-----------------")
            print("-----------------")
            print("-----------------")
            input("pulsar...")
            print("segunda iteración.\n")
            gra(30000,0.01)
            print("-----------------")
            print("-----------------")
            print("-----------------")
            input("pulsar...")
            print("tercera iteración.\n")
            gra(300000,0.01)
            print("-----------------")
            print("-----------------")
            print("-----------------")
            
            preguntatresa=input("Quieres probar con nuevos datos el algoritmo?")
            if preguntatresa=="si" or preguntatresa=="Si":
                datos_nuevos=M()
                nuevas_estimaciones=datos_nuevos.X*beta
                print("-----------------")
                print("-----------------")
                print("-----------------")
                print("Las estimaciones para estos datos no entrenados son:\n")
                for i in range(0,datos_nuevos.muestra()):
                    print(float(nuevas_estimaciones[i]))
           
            
            
            
            
            
            
            
            
        
        
    else:
        n=int(input("Tamaño de la muestra 'n' que vas a usar?\n"))
        p=int(input("Numero de variables 'm' independientes\n"))

        print("\n Introduce los datos en la matriz X \n")
        print("\n Recuerda;", n," filas por ",p+1," columnas \n")
        print("\n La primera columna de la matriz X siempre será de unos. \n")
        X=Matrices()
        print("\n Introduce los datos de la variable respuesta. \n")
        print("\n Dicho vector tiene dimensión (nx1). \n")
        Y=Matrices()
        print(tabulate(X.X,headers=["Matriz X"]))
        print("\n-------------------\n")
        print(tabulate(Y.X,headers=["Matriz Y"]))

        Z=X.XT*X.X
        invZ=Z.I
        beta=invZ*X.XT*Y.X
        estimacion=X.X*beta
        estimacionT=np.transpose(estimacion)
        H=X.X*invZ*X.XT
        e=Y.X-estimacion
        et=np.transpose(e)
        varianzaresidual=float(et*e/(n-p-1))
        desviacionresidual=math.sqrt(varianzaresidual)
    
        print("-----------------------")
        print("Valores posibles  influyentes o atípicos de la matriz H")
        print("-----------------------")
       
        
        for i in range(0,n):
            if H[i,i]>0.6:
                print("posicion ", "(",i,")","(",i,")",H[i,i])

        print("-----------------------")
        print("-----------------------")
        print("-----------------------")
        print("Las estimaciones son: ")
        print("-----------------------")
        print("-----------------------")
        sumay=0
       
        print(tabulate(estimacion,headers=["Estimaciones"]))
        
        for i in range(0,n):
            sumay+=Y.X[i]

        media_y=sumay/n

        vc=[]
        qq=(p+1)
        for i in range(0,qq):
            vc.append(varianzaresidual*invZ[i,i])
    

        print("-----------------")
        print("Los coeficientes de regresion son: ")
        print(tabulate(beta,headers=["C.regresion"]))
        print("-----------------")
        
        print("-----------------")

        print("Desviación típica de los regresores")
        print("-----------------")
        for i in range(0,p+1):
            print(math.sqrt(vc[i]))
        
        a='C:/regresion/student.txt'
        b=open(a,'r')
        lista=b.readlines()
        b.close()
        d=[]

        for i in range(0,lista.__len__()):
            d.append(float(lista[i]))


        tstudent=d[n-p-1]
        print("\n El valor que se fija de la t de Student sera: \n",tstudent)
        print("-----------------")
        print("-----------------")
        print("-----------------")
        TS=[]
        for i in range(0,p+1):
            TS.append(beta[i]/math.sqrt(vc[i]))
       
        
        for i in range(0,p+1):
            if math.fabs(TS[i])>tstudent:
                print("El coeficiente Beta",(i)," es significativo.")
        VNE=float(et*e)
        VT=float(Y.XT*Y.X-n*media_y*media_y)
        VE=float(estimacionT*estimacion-n*media_y*media_y)

        F=VE/(p*varianzaresidual)
        f4=[["VE",VE,p,VE/p,F],["VNE",VNE,n-p-1,varianzaresidual],["VT",VT,n-1,VT/(n-1)]]
        print("\n Tabla ADEVA \n")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print(tabulate(f4,headers=['fuente','sc','gl','varianza','Contraste']))
        R2=VE/VT
        print("------------")
        print("------------")
        print("------------")
        print("------------")
        print("Correlacion")
        print("------------")
        print(np.round(R2,3))
        if p==1:
            beta0=float(beta[0])
            beta1=float(beta[1])
            x1=np.array(X.X[:,1])
            y1=np.array(Y.X)
            plt.title("Nube de puntos y regresión")
            plt.grid(True)
            
            
            plt.xlabel("Variable independiente")
            plt.ylabel("Observaciones") 
            plt.plot(x1,y1,'o',color='red')
            y2=beta0+beta1*x1
            #plt.plot(x1,[beta0+beta1*x3 for x3 in x1])
            plt.plot(x1,y2,color='black')
            plt.show()
        if p==2:
            fig=plt.figure()
            #ax = Axes3D(fig)
            sub=fig.add_subplot(1,1,1,projection="3d")
            x1=np.array(X.X[:,1])
            x2=np.array(X.X[:,2])
            z=np.array(Y.X[0])
            plt.xlabel("x1")
            plt.ylabel("x2")
            plt.title("Nube de puntos y plano de regresión")
            
            #sub.plot(x1,x2,z,'o',color='black')
            sub.scatter(x1 , x2 , z,  color='black')#Con este comando creas los puntos.
            x1,x2 = np.meshgrid(x1, x2) #comando para gráfica de 3D.
            beta0=float(beta[0])
            beta1=float(beta[1])
            beta2=float(beta[2])
            B=beta0+beta1*x1+beta2*x2
            surface=sub.plot_surface(x1,x2,B,color='turquoise')
            #ax.view_init(elev=30., azim=65)
            plt.show()
            
        
        
        preguntatres=input("quieres ver gráficos de residuos?")
        if preguntatres=="si" or preguntatres=="Si":
            residuos=[]
            Yst=[]
            for i in range(0,n):
                residuos.append(float(e[i]))
                Yst.append(float(estimacion[i]))

            xx=np.array(Yst)
            yy=np.array(residuos)
            plt.title('Residuos frente a estimaciones')
            plt.scatter(xx,yy, color='black')
            x=np.arange(-50,50,0.1)
            plt.xlabel('Estimaciones')
            plt.ylabel('Residuos')
            plt.grid(True)
            plt.show()
        else:
            print("Gracias.")
        
        
        
        d1=[]
        d2=[]
        d3=[]
        d4=[]
        d5=[]
        for i in range(0,n):
            d1.append(float(Y.X[i]))
            d2.append(float(estimacion[i]))
            d3.append(float(e[i]))
            d4.append(float(H[i,i]))
            d5.append(float(1/H[i,i]))
        
        ri=[]
        for i in range(0,n):
            ri.append(float(e[i])/(desviacionresidual*math.sqrt(1-float(H[i,i]))))

        
        D=[]
        for i in range(0,n):
            D.append(float(ri[i])**2*float(H[i,i])/((p+1)*(1-float(H[i,i]))))
        
        
        nuevo=pan.DataFrame(
         { 'Observaciones':np.round(d1,2), 'Estimaciones':np.round(d2,2) ,'Residuos':np.round(d3,2),'h':np.round(d4,2)
          ,'1/h':np.round(d5,2),'r':np.round(ri,2),'D(i)':np.round(D,2)}, 
         columns=['Observaciones','Estimaciones','Residuos','h'
                  ,'1/h','r','D(i)'] )
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print(nuevo)
        
        """
        X=t.matri_x()
        XT=np.transpose(X)
        vector_medias=1/n*XT*t.mt
        Xchurro=X-t.mt*np.transpose(vector_medias)
        S=1/n*np.transpose(Xchurro)*Xchurro
        r1=np.diagonal(S)
        r2=[]
        In=np.identity(p+1)
        for i in range(0,p+1):
            r2.append(np.round(math.sqrt(r1[i]),2))
        r3=r2*In
        D=np.matrix(r3)
        inD=D.I
        MCORR=inD*S*inD
        Mcorrel=np.round(MCORR,2)
        
        
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("El vector de medias es:\n")
        for i in range(0,p+1):
            print(float(vector_medias[i]))
        
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("-----------------")
        print("La matriz de correlacion es:\n")
        print(Mcorrel)
        """


     
             
    input("Salir")    
        


except ValueError:
    print("Error del sistema.")
    


