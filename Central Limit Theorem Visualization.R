#
CLT <- function(n, size, prob, sampleSize)
{ 
	par(new=FALSE, mfrow=c(1,2))

	x <- rbinom(n, size, prob)

	#in class
 	y <- sqrt(size) * (x/size - prob) / sqrt(prob * (1- prob))
	hist(y , col='light green',
	     main = substitute(paste("Bimonial distribution, n = ", v1, ", p = ", v2) , list(v1= size, v2=prob)),
		xlab = expression(paste(sqrt(n), ' ', frac(bar(x[n])-mu, sigma)))
		)

	#my thoughts
	for(i in 1:n)
	{
		y[i] <- mean(sample(x, sampleSize, replace=TRUE))
	}

	hist(y , col='light blue',
	     main = substitute(paste("Mean of means of samples = ",  v1, ", sigma(x) = ", v2) , list(v1= round(mean(y)), v2=sd(y))),
		xlab = substitute(paste('mean of a sample of ', v1), list (v1=sampleSize)))

}
	
CLT(5000, 50, 0.4, 60)