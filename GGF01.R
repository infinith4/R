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

for( a in 5:20){
    a = a*0.1
    plot(xi,p(xi),xlab="xi",xlim=c(-4,4),ylim=c(0,1),type="l",col=a*10)
    par(new=T)
}

a=5
par(new=T)
mean=(a^(1/a))*(gamma(2/a)/gamma(1/a))
var=(a^(2/a))*(gamma(3/a)/gamma(1/a))-((a^(1/a))*(gamma(2/a)/gamma(1/a)))^2
plot(xi,Gauss(xi),xlab="xi",ylab="",xlim=c(-4,4),ylim=c(0,4.5), axes = FALSE,type="l")
axis(4)
mtext("Gauss", side = 4, line = 3)
par(default.par)


#確率1
integrate(Gauss,-Inf,Inf)