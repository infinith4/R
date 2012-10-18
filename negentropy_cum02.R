#キュムラントによるネゲントロピーの近似

phi=function(x) dnorm(x,mean=0,sd=1)

mean=1-mu
var=((5-mu)/4)-(1-mu)^2

sqrt(var)
p = function(x){
    mu*phi(x) + (1 - mu)*2*phi(2*(x - 1))
}

pdf = function(x){
    p(sqrt(var)*x+mean)*(sqrt(var))
}

integrate(pdf,-Inf,Inf)


#integrate(p,-Inf,Inf)
mu=0.5
e1=function(x){
    x*pdf(x)
}

e2=function(x){
    (x^2)*pdf(x)
}

e3=function(x){
    (x^3)*pdf(x)
}

e4=function(x){
    (x^4)*pdf(x)
}

mu=0
mean=1-mu
var=((5-mu)/4)-(1-mu)^2

integrate(e1,-Inf,Inf)$val

integrate(e2,-Inf,Inf)$val

mu=0

mean=1-mu
var=((5-mu)/4)-(1-mu)^2
k3=integrate(e3,-Inf,Inf)$val
k4=integrate(e4,-Inf,Inf)$val-3

Jc=(1/12)*(k3)^2+(1/48)*(k4)^2
Jc


mu=0
Jc

mu=1
Jc

Jc

for(mu in 0:10){
    mu=mu*0.1
    mean=1-mu
    var=((5-mu)/4)-(1-mu)^2
    k3=integrate(e3,-Inf,Inf)$val
    k4=integrate(e4,-Inf,Inf)$val-3*(integrate(e2,-Inf,Inf)$val)^2
    Jc=(1/12)*(k3)^2+(1/48)*(k4)^2
    plot(mu,Jc,xlim=c(0,1),ylim=c(0,0.4))
    par(new=T)
}





