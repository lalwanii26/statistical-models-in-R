load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names
dat = tmp
attach(dat)

fit = lm(mpg ~ ., data = dat)
summary(fit)

# checking model accuracy via residual plots
require(car)
crp(fit) # shows a hint of curvature in the plot vs disp

# stepwise search with AIC 
step(lm(mpg ~ 1), scope = mpg ~ hp+wt+len+wd, direction="forward") $ call
step(fit, direction="backward") $ call
step(fit, direction="both") $ call

# stepwise search with BIC
n = length(mpg)
step(lm(mpg ~ 1), scope = mpg ~ hp+wt+len+wd, direction="forward", k=log(n)) $ call
step(fit, direction="backward", k=log(n)) $ call
step(fit, direction="both", k=log(n)) $ call

# best subset selection using Mallow's Cp
require(leaps)
X = model.matrix(fit)[,-1]
L = leaps(x = X, y = mpg)
print(L)
ind = which.min(L$Cp)
L$which[ind,]  # best model


## Simulated example
n = 200
X = matrix(runif(n*10), n, 10)
y = X %*% c(5,4,3,2,1,0,0,0,0,0) + 0.1*rnorm(n)
dat = data.frame(X, y)
attach(dat)
fit = lm(y ~ ., data = dat)

# stepwise search with AIC 
step(lm(y ~ 1), scope = y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, direction="forward") $ call
step(fit, direction="backward") $ call
step(fit, direction="both") $ call

require(leaps)
L = leaps(X, y)
ind = which.min(L$Cp)
L$which[ind,]  # best model

