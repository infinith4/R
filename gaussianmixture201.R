#図5.3でネゲントロピーが最大なところはa=3あたりだが,(5.50)の平均と分散と計算し,
#その平均と分散を持つガウス分布と表示して,aを動かして(5.50)の図がガウス分布と比較してどのくらい違いがあるか見る。
#a=0.7で違いが見られるかもしれない。

#一般化ガウス族(GGF)
p=function(xi){
	(1/(2*a^(1/a-1)*gamma(1/a)))*exp(-abs(xi)^a/a)
}


a=2

#この定義がaに依存していないのでダメ。
mean=(a^(1/a))*(gamma(2/a)/gamma(1/a))

var=(a^(2/a))*(gamma(3/a)/gamma(1/a))-((a^(1/a))*(gamma(2/a)/gamma(1/a)))^2

GaussianMixture = function(x){
	(1/sqrt(2*pi*var^2))*(exp(-(x-mean)^2/2))

}

xi=seq(-50,50,length=1000)

plot(xi,GaussianMixture(xi),xlab="xi",ylab="GaussianMixture",xlim=c(-5,5),ylim=c(0,3),type="l" ,cex= 0.1,)


par(new=T)


for( a in 20:1000){
	a = a*0.1
	plot(xi,p(xi),xlab="xi",xlim=c(-4,4),ylim=c(0,0.5),type="l",col=a*10)
	par(new=T)
}



for( a in 20:100){
	a = a*0.1
	plot(xi,GaussianMixture(xi),xlab="xi",ylab="GaussianMixture",xlim=c(-5,5),ylim=c(0,10),type="l" ,col=a*10)
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




