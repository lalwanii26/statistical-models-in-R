require(car)

# an example of inaccurate model
x1 = runif(100,-1,1)
x2 = runif(100,-1,1)
x3 = runif(100,-1,1)
y = 2 + x1^2 + x2 + x3 + rnorm(100,sd=.1)
ex.lm = lm(y ~ x1 + x2 + x3) # we are not fitting the correct model
pairs(y ~ x1 + x2 + x3, pch=16, cex=1.5) # only a hint of curvature on the scatterplots
plot(ex.lm, which=1, pch=16) # the residuals vs fitted values plot does not show much, so we look at partial residual plots
residualPlots(ex.lm) # Residual plots 
# all the partial residual plots show a clear curvature in x1

# an example of heteroscedastic model
x1 = runif(1000)
x2 = runif(1000)
x3 = runif(1000)
y = 2 + x1 + x2 + x3 + 3*x2*rnorm(1000, sd=.1)
ex.lm = lm(y ~ x1 + x2 + x3) 
pairs(y ~ x1 + x2 + x3) # not much is visible
residualPlots(ex.lm) # clear fan shape on the 2nd plot

# an example with various kinds of outliers
x = runif(100)
y = 1 + 3*x + 0.3*rnorm(100)
x[1] = 2; y[1] = 7 # outlier in predictor
x[2] = 0.5; y[2] = 5 # outlier in response
x[3] = -0.5; y[3] = 2 # influential point
plot(x, y, pch=16, cex=1)
fit = lm(y ~ x)
par(mfrow=c(1,3))
plot(hatvalues(fit), type='h', main="hat values", lwd=3)
plot(abs(rstudent(fit)), type='h', main="externally studentized residuals", lwd=3)
plot(fit, which=4, lwd=3) # Cook's distances

# variance stabilizing transformation
x = runif(300)
y = 1 + 10*x + 0.1*(1 + 10*x)*rnorm(300)
par(mfrow=c(1,2))
plot(x, y)
plot(x, log(y)) # does not seem to help much

# weighted least squares
n = 1e2
x = runif(n)
y = 10*x + 0.3*10*x*rnorm(n) # more noise to better appreciate the difference below
fit0 = lm(y ~ x) # OLS
fit1 = lm(y ~ x, weights=x^{-2}) # WLS
summary(fit0)
summary(fit1) # the residual standard error is the proportionality constant that relates the sigma_i to w_i; R^2 and R_a^2 are the weighted versions
coef(fit0)
coef(fit1) 


## Real data manipulation ##

load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names

dat = tmp
#attach(dat)

plot(dat, pch=16)
pairs(mpg ~ ., data = dat)

# let's fit a simple linear model via least-squares
fit = lm(mpg ~ ., data = dat)
summary(fit)

# checking model accuracy and heteroscedasticity via residual plots
par(mfrow=c(1,1))
plot(fit, which=1, pch=16) # shows a hint of curvature
residualPlots(fit) # shows a hint of curvature in the plot vs disp

# checking normality via q-q plot 
plot(fit, which=2, cex=1, pch=16)

# checking for outliers in predictor via hatvalues
plot(hatvalues(fit), type='h', col="blue", ylab="Hat Values", main="Hat values")
p = 4; n = 387
abline(h = 2*(p+1)/n, lty=2) # threshold for suspects (not visible on this plot)

# checking outliers in response via externally studentized residuals
plot(abs(rstudent(fit)), type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, n-p-2), lty=2) # threshold for suspects

# Cook's distances
plot(fit, which=4, col="blue", lwd=2)
abline(h = 1, lty=2) # threshold for suspects (not visible on this plot)

# DFBETAS
par(mfrow=c(2,3))
for (j in 1:5){
	plot(abs(dfbetas(fit)[,j]), col=4, type='h', ylab='DFBETAS')
	abline(h = 2/sqrt(n), lty=2) # threshold for suspects
	}

# DFFITS	
par(mfrow=c(1,1))
plot(abs(dffits(fit)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt(p/n), lty=2) # threshold for suspects

# checking for multicolinearity via pairwise correlations b/w predictors
round( cor(dat[, -2]) , 2) # rounded to 2 digits

require(ellipse)
plotcorr(cor(dat[, -2]))

# checking for multicolinearity via variance inflation factors (VIF)
plot(vif(fit), type='h', col=4, lwd=3)
abline(h = 10, lty=2) # threshold for suspects 

# checking for multicolinearity via condition indices
C = cor(dat[, -2]) # correlation matrix for the predictors
L = eigen(C) # eigenvalues  
K = max(L$val)/L$val # condition indices
plot(K, type='h', col=4, lwd=3)
abline(h = 1000, lty=2) # threshold for suspects 
