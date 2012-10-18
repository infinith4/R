
p=function(xi){
	(1/(2*a^(1/a-1)*gamma(1/a)))*exp(-abs(xi)^a/a)
}



k31=function(xi){
	(xi^3)*p(xi)
}

k3=function(mu){
	integrate(k31,-Inf,Inf)$val
}

k41=function(xi){
	(xi^4)*p(xi)
}

k4=function(a){
	integrate(k41,-Inf,Inf)$val-3
}

Jk=function(a){
	(1/12)*k3(a)^2+(1/48)*(k4(a))^2
}

#par(new=T)

for(a in 2001:3000){
	a=a*0.001
	plot(a,Jk(a),xlab="a",ylab="negentropy",xlim=c(2,3),ylim=c(0,0.015),type="p" ,cex= 0.1,)
	par(new=T)
}

########################


g1 = function(xi){
	xi*exp(-(xi^2)/2)
}

g2a = function(xi){
	abs(xi)
}

g2b = function(xi){
	exp(-(xi^2)/2)
}

ex11=function(xi) g1(xi)*p(xi)

ex21a=function(xi) g2a(xi)*p(xi)

ex21b=function(xi) g2b(xi)*p(xi)

k1=36/(8*sqrt(3)-9)

k2a=1/(2-6/pi)

k2b=24/(16*sqrt(3)-27)
a=1
Ja=function(a){
	k1*(integrate(ex11,-Inf,Inf)$val)^2+k2a*(integrate(ex21a,-Inf,Inf)$val-sqrt(2/pi))^2

}

Ja(0)

Ja(1)
a=1

Jb=function(a){
	k1*(integrate(ex11,-Inf,Inf)$val)^2+k2b*(integrate(ex21b,-Inf,Inf)$val-sqrt(1/2))^2

}

Jb(0)

Jb(1)


for(a in 2001:3000){
	a=a*0.001
	plot(a,Ja(a),xlab="a",ylab="negentropy",xlim=c(2,3),ylim=c(0,0.015),type="p" ,cex= 0.1,)
	par(new=T)
}

par(new=T)

for(a in 2001:3000){
	a=a*0.001
	plot(a,Jb(a),xlab="a",ylab="negentropy",xlim=c(2,3),ylim=c(0,0.015),type="p" ,cex= 0.1,)
	par(new=T)
}


########################

for(a in 500:2000){
	a=a*0.001
	plot(a,Jk(a),xlab="a",ylab="negentropy",xlim=c(0.5,2),ylim=c(0,0.7),type="p" ,cex= 0.1,)
	par(new=T)
}

par(new=T)

for(a in 500:2000){
	a=a*0.001
	plot(a,Ja(a),xlab="a",ylab="negentropy",xlim=c(0.5,2),ylim=c(0,0.7),type="p" ,cex= 0.1,)
	par(new=T)
}

par(new=T)

for(a in 500:2000){
	a=a*0.001
	plot(a,Jb(a),xlab="a",ylab="negentropy",xlim=c(0.5,2),ylim=c(0,0.7),type="p" ,cex= 0.1,)
	par(new=T)
}



