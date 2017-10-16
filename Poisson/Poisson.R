#Proceso de Poisson Homogéneo

PH=function(lambda,T){
t=0
i=0
S=0
S[1]=0
while(t<T){
  U=runif(1,0,1)
  t=t-(1/lambda)*log(U)
  i=i+1
  S[i+1]=t
  plot(S,c(0:(length(S)-1)),type='s')
}
}
PH(1,100)
