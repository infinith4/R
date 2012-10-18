
mu=seq(0,1,length=1000)

k1=36/(8*sqrt(3)-9)
k2b=24/(16*sqrt(3)-27)

Ja=function(mu){
  k1*((1-mu)*exp(-2/5)*(4/5)*sqrt(5))^2+k2b*(mu+2*(1-mu)*exp(-2/5)*sqrt(1/5)-sqrt(1/2))^2
  
}

plot(mu,Ja(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,0.035),type="l")


