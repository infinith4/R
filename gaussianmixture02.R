#図5.2でネゲントロピーが最大なところはmu=0.7あたりだが,(5.49)の平均と分散と計算し,
#その平均と分散を持つガウス分布と表示して,muを動かして(5.49)の図がガウス分布と比較してどのくらい違いがあるか見る。
#mu=0.7で違いが見られるかもしれない。

mu=0.7
phi = function(xi) dnorm(xi,mean=0,sd=1)
p = function(xi){
	mu*phi(xi) + (1 - mu)*2*phi(2*(xi - 1))

}



mu=0.5

mean=1-mu

var=(1+7*mu-4*mu^2)/4
mu=0.1
var

#v=function(xi){
#	((xi-(1-mu))^2)*p(xi)
#}
#integrate(v,-Inf,Inf)


Gauss = function(x){
	(1/sqrt(2*pi*(var)^2))*(exp(-((x-(mean))^2)/(2*var^2)))

}

integrate(Gauss,-Inf,Inf)



xi=seq(-50,50,length=1000)

#pをmuについて動かす
for( mu in 0:10){
	mu = mu*0.1
	plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10,lwd=1)
	par(new=T)
}
mu=0.7
plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="o",col=mu*10,lwd=2)
par(new=T)
plot(xi,Gauss(xi),xlab="xi",ylab="",xlim=c(-3,3),ylim=c(0,0.8), pch = 1,type="b" ,cex= 0.3,axes = FALSE, lwd = 3,las = 1)



#Gauss関数をmuについて動かす
for( mu in 0:10){
	mu = mu*0.1
	mean=1-mu
	var=(1+7*mu-4*mu^2)/4
	plot(xi,Gauss(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,1),type="l",col=mu*10,lwd=1)
	par(new=T)
}

mu=0.7
mean=1-mu
var=(1+7*mu-4*mu^2)/4
plot(xi,Gauss(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,1),type="l",col=mu*10,lwd=3)


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

