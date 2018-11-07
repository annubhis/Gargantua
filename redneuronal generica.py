import numpy as np
import matplotlib.pyplot as plt
from modulos.Matrizesdearchivo import M
from time import time




#Red profunda con L capas y  una sola salida.


 

def sigmoid(x):
    a=1/(1+np.exp(-x))
    return a

class Neural_network:
   
    
    
    def __init__(self,neuronas,p,e):
        
        L=len(neuronas)
        self.W=[0]*L
        self.b=[0]*L
       
        
        
        for i in range (0,L):
            
            if i==0:
                self.W[i]= np.random.rand(neuronas[i],p)*e
                self.b[i]=np.zeros(neuronas[i])
            
            else:
               self.W[i]=np.random.rand(neuronas[i],neuronas[i-1])*e
               self.b[i]=np.zeros(neuronas[i])
               
            if i==(L-1):
             self.W[i]=np.random.rand(1,neuronas[i-1])*e
            
            
                
        
    def forward_propagation(self,X,W,b,L):
       
        
        Z=[0]*L
        A=[0]*L
        for i in range(0,L):
            if i==0:
                 Z[i]=W[i] @ X+b[0][0]
                 A[i]=np.tanh(Z[i])
            else:
                 Z[i]=W[i] @ A[i-1]+b[i][0] #Esto hay que mirarlo bien pues creo que está mal.
                 A[i]=np.tanh(Z[i])
            if i==(L-1):
                A[i]=sigmoid(Z[i])
                
        return A
    
    
    def back_propagation(self,W,b,A,x,y,L,m):
        
        dW=[0]*L
        dZ=[0]*L
        db=[0]*L
        for i in reversed (range(0,L)):
            if i==0:
                dZ[i]=np.multiply((np.transpose(W[i+1]) @ dZ[i+1]),(1-np.power(A[i],2)))
                dW[i]=1/m*dZ[i] @ np.transpose(x)
                db[i]=1/m*np.sum(dZ[i])
                
            elif i==(L-1):
                dZ[i]=A[i]-np.transpose(y)
                dW[i]=1/m*(dZ[i] @ np.transpose(A[i-1]))
                db[i]=1/m*np.sum(dZ[i])
              
            else:
                dZ[i]=np.multiply((np.transpose(W[i+1]) @ dZ[i+1]),(1-np.power(A[i],2)))
                dW[i]=1/m*(dZ[i] @ np.transpose(A[i-1]))
                db[i]=1/m*np.sum(dZ[i])
                
        
        total=[dW,db,dZ]
                
        return total
    
    def descenso_gradiente(self,W, b,derivadas,learning_rate,L):
        w1=[0]*L
        b1=[0]*L
        for i in range(0,L):
            w1[i]=  W[i]-learning_rate*derivadas[0][i]
            b1[i]=  b[i]-learning_rate*derivadas[1][i]
            
        lista2=[w1,b1]
        return lista2
                
    def predicciones(self,u,y,m):
        contador=0
        predictor=[0]*m
        U=np.transpose(u[L-1])
        for i in range (0,m):
            if (U[i,0]>=0.5):
                predictor[i]=1
          
            else:
               predictor[i]=0
        for i in range(0,m):
           if (predictor[i]==y[i]):
               contador=contador+1
          

     
        nota="Este modelo tiene una efectividad del:"
        porcentage=np.round(contador/len(y)*100,2)
        simbolo="% en el conjunto de entrenamiento."
        aciertos="Hemos acertado "
        elementos="elementos."
        print(nota,porcentage,simbolo)
        print("\n")
        print(aciertos,contador,elementos)
        
          

            
        
        

    
tiempo_inicial = time()         
x=M()
X=x.X
X=np.delete(X,0,1)
X=X.T
Y=np.array(x.Y)
#----------------------------------
x1=X.T[:,0]
y1=X.T[:,1]
plt.title("Nube de puntos")
plt.xlabel("Variable x1")
plt.ylabel("Variable x2") 
plt.plot(x1,y1,'o',color='red')
plt.grid(True)
plt.show()
#----------------------------------------------------------------
m=len(Y)
neuronas=[8,9,1] 
L=len(neuronas)
ob0=Neural_network(neuronas,2,0.21)
W=ob0.W
b=ob0.b
learning_rate=0.1

#--------------------------------------------------------------

for i in range(2500):
    A=ob0.forward_propagation(X,W,b,L)
    derivadas=ob0.back_propagation(W,b,A,X,Y,L,m)
    W1=ob0.descenso_gradiente(W, b,derivadas,learning_rate,L)
    W=W1[0]
    b=W1[1]
    tiempo_final = time() 

tiempo_ejecucion = tiempo_final - tiempo_inicial
u=ob0.forward_propagation(X,W,b,L)
U=np.transpose(u[L-1])
P=ob0.predicciones(u,Y,m)
print ('El tiempo de ejecucion del bucle fue:',np.round(tiempo_ejecucion,2),"segundos") #En segundos
































