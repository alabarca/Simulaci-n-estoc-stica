#Distribución Gamma

gamma=function(x,alpha){
  sim=500
  u=runif(sim,0,1)
  x=c()
  f=c()
  i=1
  while(i<sim+1){
    x[i]=log(alpha*exp(1)/((1-u[i])*(exp(1)+alpha)))
    if(x[i]>0){
      if(x[i]<1){
        f[i]=alpha*exp(1)*(x[i]^(alpha-1))
      }else{
        f[i]=alpha*exp(-x[i]+1)/(exp(1)+alpha)
      }
    }
  i=i+1
  }
  plot(x,f)
}
gamma(2,4)
