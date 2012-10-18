#図5.3でネゲントロピーが最大なところはa=3あたりだが,(5.50)の平均と分散と計算し,
#その平均と分散を持つガウス分布と表示して,aを動かして(5.50)の図がガウス分布と比較してどのくらい違いがあるか見る。
#a=0.7で違いが見られるかもしれない。

#一般化ガウス族(GGF)
p=function(xi){
	(1/(2*a^(1/a-1)*gamma(1/a)))*exp(-abs(xi)^a/a)
}

a=2

mean=(a^(1/a))*(gamma(2/a)/gamma(1/a))
var=(a^(2/a))*(gamma(3/a)/gamma(1/a))-((a^(1/a))*(gamma(2/a)/gamma(1/a)))^2

Gauss = function(x){
	(1/sqrt(2*pi*(var)^2))*(exp(-((x-(mean))^2)/(2*var^2)))

}


xi=seq(-50,50,length=1000)

default.par <- par()
mai <- par()$mai
mai[4] <- mai[1]
par(mai = mai)

for( a in 20:1000){
	a = a*0.1
	plot(xi,p(xi),xlab="xi",xlim=c(-4,4),ylim=c(0,1),type="l",col=a*10)
	par(new=T)
}

a=1000
par(new=T)
mean=(a^(1/a))*(gamma(2/a)/gamma(1/a))
var=(a^(2/a))*(gamma(3/a)/gamma(1/a))-((a^(1/a))*(gamma(2/a)/gamma(1/a)))^2
plot(xi,Gauss(xi),xlab="xi",ylab="",xlim=c(-4,4),ylim=c(0,4.5), axes = FALSE,type="l")
axis(4)
mtext("Gauss", side = 4, line = 3)
par(default.par)


integrate(Gauss,-Inf,Inf)

for( a in 20:1000){
	a = a*0.1
	mean=(a^(1/a))*(gamma(2/a)/gamma(1/a))
	var=(a^(2/a))*(gamma(3/a)/gamma(1/a))-((a^(1/a))*(gamma(2/a)/gamma(1/a)))^2
	plot(xi,Gauss(xi),xlab="xi",ylab="Gauss",xlim=c(-4,4),ylim=c(0,5),type="l" ,col=a*10)
	par(new=T)
}








mu=0.10
plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.8),type="l",col=mu*10)

mu=0.7
for( mu in 70:80){
	mu = mu*0.01
	plot(xi,p(xi),xlab="xi",xlim=c(-3,3),ylim=c(0,0.5),type="l",col=mu*100)
	par(new=T)
}




