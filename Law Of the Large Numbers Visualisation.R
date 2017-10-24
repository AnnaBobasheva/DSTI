#
LLN <- function(n, lambda, k)
{ 
	par(new=FALSE)
	M = matrix(data= 0, ncol=n, nrow=k)
	for ( i in 1:k)
		{
			x <- rexp(n, lambda)
			x_mean <- cumsum(x) / (1:n)
			M[i,] <- x_mean
		}
	ma <- max(M)
	for ( i in 1:k)
		{
			plot((1:n), M[i,], type='l', col=palette()[i], xlim= c(1,n), ylim=c(0, ma), ylab="", xlab="")	
			par(new=TRUE)
		}
	title <- expression(paste(" ", lambda, "= "))
	plot(c(0,n), c(1/lambda, 1/lambda),
	     type='l', col='black', lwd=2, 
	     xlim= c(0,n), ylim=c(0, ma), 
 	     main = substitute(paste(a, " exponential distributions, ", lambda, "=", v " ,"), list(a= k, v=lambda)),
	     xlab='number of trials', ylab='Average')
}
	
LLN(5000, 2, 5)