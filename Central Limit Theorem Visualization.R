#
CLT_binom <- function(n, size, prob, sampleSize)
{ 
	x <- rbinom(n, size, prob)

	#in class
 	y <- sqrt(size) * (x/size - prob) / sqrt(prob * (1- prob))
	hist(y , col='light green',
	     main = substitute(paste("Binomial distribution, n = ", v1, ", p = ", v2) , list(v1= size, v2=prob)),
		xlab = expression(paste(sqrt(n), ' ', frac(bar(x[n])-mu, sigma)))
		)
}

# Visualizing CLT using exponential distribution
CLT_exp <- function(repeats, sampleSize, lambda)
{
	par( mfrow=c(1,2))
	clt <- NULL
	set.seed(1648)
	for(i in 1:repeats)
	{
		clt<- c(clt, mean(rexp(sampleSize, lambda)))
	}
	hist(clt , col='light blue',
	     main = substitute(paste("Emp: ", mu , " = ",  v1, ", ", sigma, " = ", v2, " Theo: ", mu, " = ", v3, ", " , sigma, " = ", v4) , 
				     list(v1= round(mean(clt)), v2=round(sd(clt),2), v3=round(1/lambda), v4=round(1/lambda/sqrt(sampleSize), 2))),
	     xlab = substitute(paste(bar(X[n]),  " [Exp(", lambda, " = " , v2, , ") ,n = ", v1, "]" ), list (v1=sampleSize, v2=lambda)))

	#Normalize 
	clt <- sqrt(sampleSize) * (clt - (1/lambda)) / (1/lambda)
	
	hist(clt , col='light yellow',
	     main = substitute(paste("CLT in normaized form : ", mu , " = ",  v1, ", ", sigma, " = ", v2) , 
				     list(v1= round(mean(clt)), v2=round(sd(clt),2))),
	     xlab = expression(paste(sqrt(n), "  ", frac(bar(X[n])-mu, sigma))))


}
	
CLT_exp(1000, 50, 0.2)