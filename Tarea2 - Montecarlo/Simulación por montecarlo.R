#Simulación de la integral int(e^(-(x^2)/2)) en el intervalo [-inf a inf] 
k=1
S=0
g=function(u){
  h=2*exp(-((1-(1/u))^2)/2)*(1/u^2) #Se contruye la función transformada en el intervalo [0,1]
return(h)
}
while(k<1000000){
  u=runif(1) # se generan uniformes independientes en [0,1]
  S=S+g(u) #las sucesivas iteraciones permiten aplicar la ley fuerte de los grandes números
  k=k+1
}
S/k #valor de la integral

#OTRO CÓDIGO ALTERNATIVO
k=10000
x=runif(k);
theta=mean(g(x)) # valor de la integral
