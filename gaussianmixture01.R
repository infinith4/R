#図5.2でネゲントロピーが最大なところはmu=0.7あたりだが,(5.49)の平均と分散と計算し,
#その平均と分散を持つガウス分布と表示して,muを動かして(5.49)の図がガウス分布と比較してどのくらい違いがあるか見る。
#mu=0.7で違いが見られるかもしれない。

p = function(xi){
	mu*phi(xi) + (1 - mu)*2*phi(2*(xi - 1))

}



mu=0.7

mean=1-mu

var=-((mu-7/8)^2-(7/8)^2)

GaussianMixture = function(mu){
	(1/sqrt(2*pi*var^2))*(exp(-(x-mean)^2/2))

}

xi=seq(-50,50,length=1000)

plot(xi,GaussianMixture(xi),xlab="xi",ylab="negentropy",xlim=c(-10,10),ylim=c(0,0.6),type="l" ,cex= 0.1,)




xi=seq(-50,50,length=1000)

phi = function(xi) dnorm(xi)

par(new=T)
plot(xi,p(xi),xlab="xi",ylab="pdf",xlim=c(-10,10),ylim=c(0,0.6),type="l",col=2)

par(new=T)


for( mu in 0:10){
	mu = mu*0.1
	plot(xi,p(xi),xlab="xi",ylab="pdf",type="l",col=mu*10)
	par(new=T)
}



