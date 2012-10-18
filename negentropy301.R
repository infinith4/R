#本に載ってるネゲントロピーの計算

p = function(x){
    mu*phi(x) + (1 - mu)*2*phi(2*(x - 1))
    
}

mean=1-mu
var=((5-mu)/4)-(1-mu)^2

pdf=function(x){
    p(sqrt(var)*x+mean)*(sqrt(var))
}


gauss=function(x) dnorm(x,mean=0,sd=1)
#gauss=function(x) dnorm(x,mean=1-mu,sd=sqrt((1+7*mu-4*mu^2)/4))
phi=function(x) dnorm(x,mean=0,sd=1)


mu=0
k1=36/(8*sqrt(3)-9)
k2a=1/(2-6/pi)

#g1:奇関数
g1=function(x){
    x*exp(-(x^2)/2)*p(x)
}
#g2a:偶関数
g2a=function(x){
    x*p(x)
}

integrate(g1,-Inf,Inf)$val

2*integrate(g2a,0,Inf)$val

h=function(v){
    v*gauss(v)
}
h1=function(v){
    v*exp(-(v^2)/2)*gauss(v)
}
Ja=function(mu){
    k1*(integrate(g1,-Inf,Inf)$val-integrate(h1,-Inf,Inf)$val)^2+k2a*(2*integrate(g2a,0,Inf)$val-2*integrate(h,0,Inf)$val)^2
}

mu=0
Ja(mu)
mu=1
Ja(mu)

for(mu in 0:100){
    mu = mu*0.01
    plot(mu,Ja(mu),xlab="mu",xlim=c(0,1),ylim=c(0,0.08),type="p",lwd=1)
    par(new=T)
    
}

###################################



#確かめ
#E(G1)=0?
mu=1
integrate(g1,-Inf,Inf)$val
h1=function(v){
    v*exp(-(v^2)/2)*gauss(v)
}

mu=0
integrate(g1,-Inf,Inf)$val-integrate(h1,-Inf,Inf)$val
integrate(h1,-Inf,Inf)$val

#mu=0,1で、
#2*integrate(g,0,Inf)$val-2*integrate(h,0,Inf)$val=0?

2*integrate(g2a,0,Inf)$val-sqrt(2/pi)

mu=1
g=function(x){
    x*p(x)
}

h=function(v){
    v*gauss(v)
}
2*integrate(g,0,Inf)$val-2*integrate(h,0,Inf)$val#mu=1で0

#mu=0
mu=0
gauss=function(x) dnorm(x,mean=1-mu,sd=sqrt(((10-9*mu)/9)-(1-mu)^2))

p = function(x){
    mu*3*phi(3*x) + (1 - mu)*3*phi(3*(x - 1))
    
}

g=function(x){
    x*p(x)
}

h=function(v){
    v*gauss(v)
}


2*integrate(g,0,Inf)$val-2*integrate(h,0,Inf)$val

##########################

for(mu in 0:100){
    mu = mu*0.01
    plot(mu,Ja(mu),xlab="mu",xlim=c(0,1),ylim=c(0,0.03),type="p",lwd=1)
    par(new=T)
    
}


