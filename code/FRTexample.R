##### FRT test

# my function
DiM_function = function(Z,Y){
  DiM = mean(Y[which(Z==1)]) - mean(Y[which(Z==0)])
  return(DiM)
}

# simulating data and running an FRT
set.seed(1)
Yobs = rnorm(20)
Zobs = rbinom(20,1,0.5)
tau = 1 # additive treatment effect of 0.1
Yobs[Zobs==1] = Yobs[Zobs==1] + tau
boxplot(Yobs~Zobs)
Tobs = DiM_function(Zobs,Yobs)

Tprime = c()
for(ii in 1:1000){
  Zprime = rbinom(20,1,0.5)
  Tprime = c(Tprime,DiM_function(Zprime,Yobs)) # Yobs stays fixed because null hypothesis tells me so!
}
hist(Tprime)
abline(v=Tobs,col='orange',lwd=3)
FRTpvalue = mean(Tprime>=Tobs)

