bimodal <- function(d)
{
  x <- seq(-4, 4, 0.05)
  plot(x,dnorm(x, mean=0, sd=0.8), type="n")
  curve(dnorm(x, mean=-d, sd=0.25)+dnorm(x, mean=d, sd=0.25), type="l",add=T)
}

integrate(bimodal,-Inf,Inf)