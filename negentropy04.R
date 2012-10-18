phi = function(xi) dnorm(xi)

integrate(phi,-Inf,Inf)

p = function(xi){
	mu*phi(xi) + (1 - mu)*2*phi(2*(xi - 1))

}

integrate(p,-Inf,Inf)

g1 = function(xi){
	xi*exp(-(xi^2)/2)
}

g2a = function(xi){
	abs(xi)
}

g2b = function(xi){
	exp(-(xi^2)/2)
}

ex11=function(xi) g1(xi)*phi(xi)
ex12=function(xi) g1(xi)*phi(2*(xi-1))


ex1=function(mu){

	mu*integrate(ex11,-Inf,Inf)$val+2*(1-mu)*integrate(ex12,-Inf,Inf)$val
}

ex21a=function(xi) g2a(xi)*phi(xi)
ex22a=function(xi) g2a(xi)*phi(2*(xi-1))

ex2a=function(mu){

	mu*integrate(ex21a,-Inf,Inf)$val+2*(1-mu)*integrate(ex22a,-Inf,Inf)$val
}

ex21b=function(xi) g2b(xi)*phi(xi)
ex22b=function(xi) g2b(xi)*phi(2*(xi-1))

ex2b=function(mu){

	mu*integrate(ex21b,-Inf,Inf)$val+2*(1-mu)*integrate(ex22b,-Inf,Inf)$val
}

k1=36/(8*sqrt(3)-9)

k2a=1/(2-6/pi)

k2b=24/(16*sqrt(3)-27)

Ja=function(mu){
	k1*(ex1(mu))^2+k2a*(ex2b(mu)-sqrt(2/pi))^2

}

Jb=function(mu){
	k1*(ex1(mu))^2+k2b*(ex2(mu)-sqrt(1/2))^2

}

Jb(0)

Jb(0.5)

Jb(1)


mu=seq(0,1,length=1000)

plot(mu,Ja(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,5),type="l")

par(new=T)

plot(mu,Jb(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,1),type="l")

plot(xi,phi(2*(xi-1)),xlab="xi",ylab="",xlim=c(-5,5),ylim=c(0,1),type="l")

xi=seq(-50,50,length=1000)

for( mu in 0:10){
	mu = mu*0.1
	plot(xi,p(xi),xlab="xi",ylab="pdf",xlim=c(-5,5),ylim=c(0,1),type="l",col=mu*10)
	par(new=T)
}


mu=1

f = function(xi){
	-p(xi)*log(p(xi))

}

f1 = function(xi){
	-phi(xi)*log(p(xi))

}

h=function(mu){
	integrate(f,-15,15)$val
}


J=function(mu){
	(1+(1/2)*(log(2*pi)))-h(mu)
}

for(mu in 0:1000){
	mu=mu*0.001
	plot(mu,J(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,1),type="p",cex=0.5,col="red")
	par(new=T)
}


k31=function(xi){
	xi^3*p(xi)
}

k3=function(mu){
	integrate(k31,-Inf,Inf)$val
}

k41=function(xi){
	xi^4*p(xi)
}

k4=function(mu){
	integrate(k41,-Inf,Inf)$val-3
}

Jk=function(mu){
	(1/12)*k3(mu)+(1/48)*(k4(mu))
}

par(new=T)

for(mu in 0:1000){
	mu=mu*0.001
	plot(mu,Jk(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,1),type="p" ,cex= 0.1,)
	par(new=T)
}






