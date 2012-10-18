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

e1=function(xi) g1(xi)*p(xi)


ex1=integrate(e1,-Inf,Inf)$val


e2=function(xi) g2(xi)*p(xi)

ex2=integrate(e2,-Inf,Inf)$val

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
k2

jb=function(){
	k1*(ex1)^2+k2*(ex2-sqrt(1/2))^2

}

mu=0.1
jb()

jb(2)


plot(mu,Jb(mu),xlab="mu",ylab="negentropy",xlim=c(0,1),ylim=c(0,5),type="l")






















