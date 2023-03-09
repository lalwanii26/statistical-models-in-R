load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names
dat = tmp
dat = as.data.frame(scale(dat)) # Normalizing the (numerical) variables
#attach(as.data.frame(dat))

# Ridge regression
require(MASS)
fit0 = lm(mpg ~ hp + wt + len + wd, data = dat)
round(coef(fit0), 3)
fit = lm.ridge(mpg ~ hp + wt + len + wd, data = dat, lambda=seq(0, 20, len=100))
select(fit) # provides 3 different estimates for lambda

# Plot of the coefficients vs lambda
plot(fit) 

# A more detailed plot
matplot(fit$lambda, t(fit$coef), type='l', lwd=2, xlab='Lambda', ylab='Coefficients', col=1:4)
abline(h=0, lty=3)
title('Ridge Regression')
legend(x='bottomright', c('hp', 'wt', 'len', 'wd'), lty=1:4, lwd=2, col=1:4, bg='white') 

# LASSO
require(lars)
X <- as.matrix(dat[,c('hp', 'wt', 'len', 'wd')])
y <- dat$mpg 
fit = lars(x=X, y=y, normalize=FALSE, intercept=FALSE)
plot(fit, lty=1:4, col=1:4, lwd=2)
legend('bottomleft', c('hp', 'wt', 'len', 'wd'), lty=1:4, lwd=2, col=1:4, bg='white') 


## Simulated example 
X = matrix(runif(100*10), 100, 10)
X = scale(X)
y = X %*% c(8,5,3.5,2,1,0,0,0,0,0) + rnorm(100)

fit = lm.ridge(y ~ X, lambda=0:1000)
plot(fit)
title("Ridge Regression")

fit.lasso = lars(X, y)
plot(fit.lasso, lty=rep(1,10), col=rainbow(10))
summary(fit.lasso)
