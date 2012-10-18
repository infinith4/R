#?}5.2?Ńl?Q???g???s?[???ő??ȂƂ?????mu=0.7?????肾??,(5.49)?̕??ςƕ??U?ƌv?Z??,
#???̕??ςƕ??U?����K?E?X???z?ƕ\??????,mu?𓮂?????(5.49)?̐}???K?E?X???z?Ɣ??r???Ăǂ̂??炢?Ⴂ?????邩?????B
#mu=0.7?ňႢ?????????邩?���???Ȃ??B

mu=0.5
phi = function(xi) dnorm(xi,mean=0,sd=1)

p = function(xi){
	mu*3*phi(3*xi) + (1 - mu)*3*phi(3*(xi - 1))

}

integrate(p,-Inf,Inf)


mu=0.5

mean=1-mu
var=(28-27*mu)/9

#v=function(xi){
#	((xi-(1-mu))^2)*p(xi)
#}
#integrate(v,-Inf,Inf)


Gauss = function(x){
	(1/sqrt(2*pi*(var)^2))*(exp(-((x-(mean))^2)/(2*var^2)))

}

integrate(Gauss,-Inf,Inf)



xi=seq(-50,50,length=1000)

#p??mu?ɂ??ē?????
for( mu in 0:10){
	mu = mu*0.1
	plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10,lwd=1)
	par(new=T)
}

mu=0.5
plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="o",col=mu*10,lwd=3)
par(new=T)
mu=0.5
mean=1-mu
var=(28-27*mu)/9
plot(xi,Gauss(xi),xlab="xi",ylab="",xlim=c(-3,3),ylim=c(0,0.8), pch = 1,type="b" ,cex= 0.3,axes = FALSE, lwd = 3,las = 1)



#Gauss?֐???mu?ɂ??ē?????
for( mu in 0:10){
	mu = mu*0.1
	mean=1-mu
	var=(28-27*mu)/9
	plot(xi,Gauss(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10,lwd=1)
	par(new=T)
}
mu=0.5
mean=1-mu
var=(28-27*mu)/9
plot(xi,Gauss(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="o",col=mu*10,lwd=2)

m
#########################











#par(new=T)
#plot(xi,p(xi),xlab="xi",ylab="pdf",xlim=c(-3,3),ylim=c(0,0.6),type="l",col=2)

par(new=T)




#mu=0.10
#plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10)

mu=0.7
plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10)

for( mu in 70:80){
	mu = mu*0.01
	plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.5),type="l",col=mu*100)
	par(new=T)
}

