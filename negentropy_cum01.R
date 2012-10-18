#キュムラントによるネゲントロピーの近似

phi=function(x) dnorm(x,mean=0,sd=1)

p = function(x){
    mu*phi(x) + (1 - mu)*2*phi(2*(x - 1))
    
}

e2=function(x){
    (x^2)*p(x)
}

e3=function(x){
    (x^3)*p(x)
}

e4=function(x){
    (x^4)*p(x)
}

mu=0
k3=integrate(e3,-Inf,Inf)$val-3*(integrate(e2,-Inf,Inf)$val)*(1-mu)+2*(1-mu)^3
k4=integrate(e4,-Inf,Inf)$val-3*(integrate(e2,-Inf,Inf)$val)^2-4*(integrate(e3,-Inf,Inf)$val)*(1-mu)+12*(integrate(e2,-Inf,Inf)$val)*(1-mu)^2-6*(1-mu)^4


Jc=(1/12)*(k3)^2+(1/48)*(k4)^2

mu=0
Jc

mu=1
Jc

Jc

for(mu in 0:10){
    mu=mu*0.1
    k3=integrate(e3,-Inf,Inf)$val-3*(integrate(e2,-Inf,Inf)$val)*(1-mu)+2*(1-mu)^3
    k4=integrate(e4,-Inf,Inf)$val-3*(integrate(e2,-Inf,Inf)$val)^2-4*(integrate(e3,-Inf,Inf)$val)*(1-mu)+12*(integrate(e2,-Inf,Inf)$val)*(1-mu)^2-6*(1-mu)^4
    Jc=(1/12)*(k3)^2+(1/48)*(k4)^2
    plot(mu,Jc,xlim=c(0,1),ylim=c(0,0.04))
    par(new=T)
}    
    
    
    