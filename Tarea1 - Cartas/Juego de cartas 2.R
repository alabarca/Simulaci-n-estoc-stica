#CÓDIGO QUE SIMULA LA PROBABILIDAD DE QUE SALGAN 3 ACES Y DOS REYES EN UNA BARAJA DE 52 CARTAS INGLESAS

m =100000 # número de iteraciones
i=1 # contador
aces= numeric(m) # m-vector que alcenará el número de aces en cada iteración
reyes=numeric(m) # m-vector que alcenará el número de reyes en cada iteración
while(i<m+1){
  vec=c(1:52) # permutación de cartas. En este caso se toma P_j=j
  vec1=c() # vector que servirá para intercambiar las posiciones de "vec"
  k=52 #contador de las posiciones a ordenar
  j=1
  while (j==1){
    u=runif(1) #se genera aleatoriamente un valor con distribución uniforme [0,1]
    I=k%/%(1/u) + 1 #
    vec1[I]=vec[k] # se intercambian las posiciones
    vec[k]=vec[I]
    vec[I]=vec1[I]
    k=k-1 
    if (k==1){
      j=2 
    }
  }
  h=c(vec[1],vec[2],vec[3],vec[4],vec[5])
  #considera en cada iteración, cuántas veces salen tanto aces como reyes
  aces[i] = sum(h<5) 
  reyes[i] = sum(9<=h & h<14)
  i=i+1
}
mean(aces==3 & reyes==2) #probabilidad de que salgan 2 aces y 3 reyes

#Veamos el histograma correspondiente a la variable número de aces 
cut = (0:5) - .5
hist(aces, prob=T, breaks=cut,col="Wheat",main="Aces en una mano de Poker")
#Note que el histograma para el número de reyes debería ser muy parecido
hist(reyes, prob=T, breaks=cut,col="Wheat",main="Reyes en una mano de Poker")
# Más específicamente
#summary(as.factor(aces)) Contador de observaciones
summary(as.factor(aces))/m # Probabilidades simuladas
summary(as.factor(reyes))/m


