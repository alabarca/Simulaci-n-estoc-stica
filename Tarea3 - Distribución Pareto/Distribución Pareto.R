#Distribuciòn Pareto

pareto=function(alpha,x_m){
  sim=500
  u=runif(sim,0,1)
  x=c()
  f=c()
  i=1
  while(i<sim+1){
    x[i]=x_m/((1-u[i])^(1/alpha))
    if(x[i]>x_m){
      f[i]=alpha*(x_m^(alpha))/(x[i]^(alpha+1))
    }else{
      f[i]=0
    }
    i=i+1
  }
  plot(x,f) 
}
pareto(1,1)
