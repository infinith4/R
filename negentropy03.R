phi = function(xi) dnorm(xi)

integrate(phi,-Inf,Inf)

p = function(xi){
	mu*phi(xi) + (1 - mu)*2*phi(2*(xi - 1))

}


integrate(p,-Inf,Inf)

g1 = function(xi){
	xi*exp(-xi^2/2)
}

g2 = function(xi){
	exp(-xi^2/2)
}

ex10=function(xi) g1(xi)*phi(xi)
ex11=function(xi) g1(xi)*phi(2*(xi-1))

ex1=function(mu){

	mu*integrate(ex10,-Inf,Inf)$val+2*(1-mu)*integrate(ex11,-Inf,Inf)$val
}

ex20=function(xi) g2(xi)*phi(xi)
ex21=function(xi) g2(xi)*phi(2*(xi-1))

ex2=function(mu){

	mu*integrate(ex20,-Inf,Inf)$val+2*(1-mu)*integrate(ex21,-Inf,Inf)$val
}

del10=function(xi){
	phi(xi)*(g1(xi))^2
}

del11=function(xi){
	phi(xi)*g1(xi)*xi
}

delta1=function(){
	integrate(del10,-Inf,Inf)$val-(integrate(del11,-Inf,Inf)$val)^2
}

delta1()

k1=1/(2*delta1())
k1

del20=function(xi){
	phi(xi)*(g2(xi))^2
}

del21=function(xi){
	phi(xi)*g2(xi)*xi
}

del22=function(xi){
	phi(xi)*g2(xi)
}

delta2=function(){
	integrate(del20,-Inf,Inf)$val-(integrate(del21,-Inf,Inf)$val)^2-(1/2)*(integrate(del22,-Inf,Inf)$val-integrate(del21,-Inf,Inf)$val)
}


k2=1/(2*delta2())
#k2=24/(16*sqrt(3)-27)

Jb=function(mu){
	k1*(ex0(mu))^2+k2*(ex1(mu)-sqrt(1/2))^2

}

Jb(1)

Jb(2)

mu=seq(0,1,length=1000)

plot(mu,Jb(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,5),type="l")

xi=seq(-50,50,length=1000)

for( mu in 0:10){
	mu = mu*0.1
	plot(xi,p(xi),xlab="xi",ylab="pdf",xlim=c(-5,5),ylim=c(0,1),type="l",col=mu*10)
	par(new=T)
}





















