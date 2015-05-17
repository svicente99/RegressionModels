## ----------------------------------------
## Coursera - Data Science Specialization
## Regression Models
##
## Quiz 1 - answers code
## 
## date...: May, 10th 2015
## author.: Sergio Vicente
## twitter: @svicente99
## ----------------------------------------

library(manipulate)

q01_mse <- function(mu) {
	x <- c(0.18, -1.54, 0.42, 0.95)
	w <- c(2, 1, 3, 1)
	hist(x, col="blue", breaks=200)
	lines(c(mu, mu), c(0, 150), col="red", lwd=5)
	mse <- mean(w*(x - mu)^2)
	text(63, 150, paste("mu = ", mu))
	text(63, 140, paste("MSE = ", round(mse, 2)))
	return(mse)	
}

q01 <- function() {
	v <- c(0.0025, 1.077, 0.300, 0.1471)
	min <- 999999
	mu_answer <- 0
	for(mu in v) {
		min_i <- q01_mse(mu)
		cat("mu = ",mu," - mse = ",min_i,"\n")
		if(min_i < min) {
			min <- min_i; mu_answer <- mu
		}
	}
	cat("menor 'mse' = ",min," para o mu = ",mu_answer,"\n")
}

q02 <- function() {
	x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
	y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
	# Fit a regression line in which the intercept has been
	# forced to be zero and display the line on the scattter plot.
	plot(x,y)
	# fitZeroInt <- lm(y ~ -1+x)
	# ou...
	# http://stackoverflow.com/questions/18941205/how-to-force-a-regression-through-the-origin-r
	fitZeroInt <- lm(y ~ 0 + x)
	abline(fitZeroInt, lty=2)
	print(summary( fitZeroInt ))
	print(paste("The answer been asked is:", round(fitZeroInt$coefficients[1],4)))
}

q03 <- function() {
	fit <- lm(mpg ~ wt, data=mtcars)
	print(summary(fit))
	print("The slope coefficient is:")
	print(fit$coefficients[2])
}

q04 <- function() {
	corXY = 0.5
	sigmaY = 4 # by instance...
	sigmaX <- sigmaY / 2
	beta1 <- corXY * sigmaY / sigmaX
	print(paste("The answer is:",beta1))
}

q05 <- function() {
	# According to Video Lecture 01_04.pdf
	# If you had to predict a son's normalized height
	# it would be: Cor(X,Y) * X(i)
	# Thus...
	corXY = 0.4
	Xi = 1.5
	print(paste("The answer of this question is:", corXY*Xi))
}	

q05_demo <- function(n=20) {
	mt <- mtcars
	x1 <- mean(mt$wt)   #x
	s1 <- sd(mt$wt)
	wtNorm <- (mt$wt-x1)/s1
	x2 <- mean(mt$mpg)  #y
	s2 <- mean(mt$mpg)
	mpgNorm <- (mt$mpg-x2)/s2
	
	print(head(wtNorm,5))
	print(head(mpgNorm,5))
	Xprev <- wtNorm[n]
	fit <- lm(mpgNorm ~ wtNorm)
	print(summary(fit))
	beta0 <- fit$coefficients[1]
	beta1 <- fit$coefficients[2]
	print(beta0)
	print(beta1)
	
	plot(wtNorm, mpgNorm)
	abline(fit, lty=2)
	
	Zpred <- beta0 + (beta1 * Xprev)
	print(paste("Zpred =",round(Zpred,5)))
	Ypred <- Zpred*s1 + x1
	print(paste("Ypred =",round(Ypred,5)))
	rho <- round(cov(mpgNorm,wtNorm,use="all.obs",method="pearson"),7)
	print(paste("rho =",rho))

	print(paste("Normalized value to predict:",round(Xprev,5)))
	print("Result using quick formula: rho*Xi (same value is obtained)")
	print(round(rho*Xprev,5))
	
}

q06 <- function() {
	x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
	xn <- (x - mean(x))/sd(x)
	print(head(xn))
	print("So, the first measurement value, after normalized, is:")
	print(round(xn[1],4))
}

q07 <- function() {
	x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
	y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
	fit <- lm(y ~ x)
	summary(fit)
}

q09_mse <- function(mu) {
	x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
	hist(x, col="blue", breaks=200)
	lines(c(mu, mu), c(0, 150), col="red", lwd=5)
	mse <- mean((x - mu)^2)
	text(63, 150, paste("mu = ", mu))
	text(63, 140, paste("MSE = ", round(mse, 2)))
	return(mse)	
}

q08 <- function() {
	x <- c(0,0,0,0,0)
	y <- c(0,0,0,0,0)
	fit <- lm(y ~ x)
	print(summary(fit))
	print(paste("So,the intercept is: ",fit$coefficients[1]))
}
q09 <- function() {
	v <- c(0.573, 0.8, 0.36, 0.44)
	min <- 999999
	mu_answer <- 0
	for(mu in v) {
		min_i <- q09_mse(mu)
		cat("mu = ",mu," - mse = ",min_i,"\n")
		if(min_i < min) {
			min <- min_i; mu_answer <- mu
		}
	}
	cat("menor 'mse' = ",min," para o mu = ",mu_answer,"\n")
}

q10 <- function() {
	fit1 <- lm(mpg ~ wt, data=mtcars)
	print(summary(fit1))
	fit2 <- lm(wt ~ mpg, data=mtcars)
	print(summary(fit2))
	print("Be 'beta1' the slope of fit1 and 'gama1', slope of fit2")
	print("The ratio beta1/gama1:")
	print(fit1$coefficients[2] / fit2$coefficients[2])
	print("Var(Y)/Var(X)")
	print(var(mtcars$mpg)/var(mtcars$wt))
}
