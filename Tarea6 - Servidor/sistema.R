#Para la aplicación del algoritmo se considera un proceso de poisson de razón 5, la distribución de los 
#tiempos de permanencia en el servidor es U[0,5] y la distribución del servicio es una exponencial de
#parámetro 4

set.seed(1000)
NB=50 # número de simulaciones
j=1 #contador de simulaciones
run=c() # almacena el número de clientes atendidos en la k-ésima simulación
NB_lost=c() #almacena el número de clientes perdidos en la k-ésima simulación
while(j<NB){
  T=100 # tiempo para el cual vamos a estimar el número de clientes perdidos
  t=0 # Contador del tiempo T
  N=0 #número de clientes hasta el tiempo T
  l=0 #contador del número de clientes N
  lost=0 #contador del número de clientes perdidos hasta el tiempo T
  E=c() #alamacena los tiempos de entrar de cada cliente
  while(t<=T){
    N=l
    l=l+1
    t=t+rexp(1,5)
    if(t<=T){
      E[l]=t
    }
  }#Proceso de poisson 
  R=runif(N,min=0,max=5) #La longitud del tiempo de tolerancia del k-ésimo cliente
  L=pmin(E+R,T) #El momento de salir de cada cliente
  pending=matrix(1,ncol=1,nrow=N) # matriz que servirá como condicional a la hora de contar el número de clientes perdidos y 
  S_star=min(E*pending) #Momento inicial en que atienden al k-ésimo cliente
  while(as.numeric(min(as.character(L*pending)))<T){
    i=1
    S=rexp(1,4) #distribución del servicio
    if(S_star+S>T){#si la duración del servicio supera el tiempo establecido para analizar 
        L[which(as.character(E)==min(as.character(E*pending)))]=T #esto es para nunca lo considere en el while anterior
    }else{
        L[which(as.character(E)==min(as.character(E*pending)))]=S_star+S#Cambia el tiempo de salir del cliente en la posición dada por E
    }
    pending[1:sum(as.character(E)==min(as.character(E*pending))),]=T #Esto es para que no se considere en el while posterior
    while(i<N+1){
      if(as.character(pending[i,])=="1"){
        if(L[i]<min(S_star+S,T)){
        pending[i,]=NaN
        lost=lost+1#cuenta los clientes perdidos 
        }
      }
      i=i+1
    }
    S_star=max(S_star+S,as.numeric(min(as.character(L*pending))))
  }
  run[j]=sum(L-E)/T
  NB_lost[j]=lost
  j=j+1
}
mean(run) #valor promedio de los clientes que entran al servicio
mean(NB_lost) #valor promedio de los clientes perdidos
sd(run)
sd(NB_lost)
