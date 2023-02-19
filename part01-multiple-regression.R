load("datasets/04cars.rda") # loads a data frame called "dat"
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names

dat = tmp
str(dat)
head(dat)
#attach(dat) # creates a variables for each column

# all pairwise scatterplots
plot(dat, pch = 16) 
pairs(mpg ~ ., data = dat, pch = 16) # same except for the order of the variables

# correlation plot
require(ellipse)
plotcorr(cor(dat))

# let's fit a simple linear model via least-squares (intercept included by default)
fit = lm(mpg ~ ., data = dat)

# alternatively
fit = lm(mpg ~ hp + wt + len + wd)

summary(fit)

# lots of correlations so that most variables are not significant when the others are in the model

# CIs for the coefficients
confint(fit)

# predicting with a 95% confidence interval
# let's do so for the 2004 Peugeot 407
# http://www.auto-data.net/en/?f=showCar&car_id=5394  
new = data.frame(hp=116, wt=3086, len=184, wd=71)
predict(fit, new, interval='confidence') # predicts mean
predict(fit, new, interval='prediction') # predicts new response
# actual value is mpg = 39

# predict() is a generic function; to learn about how it works with lm objects, do as follows
?predict.lm

# Explaining the (adjusted) R-squared
fit0 = lm(mpg ~ hp, data = dat)
summary(fit0)
1 - var(residuals(fit0))/var(dat$mpg) # same as R-squared
par(mfrow=c(1,2)) # subdivides the graphical window
plot(mpg ~ hp, data = dat, pch = 16)
abline(fit0, col = 'red', lwd = 2)
boxplot(dat$mpg - mean(dat$mpg), residuals(fit0), names=c('mpg (centered)', 'residuals'))

# comparing the model with only hp and wt to the model above
fit0 = lm(mpg ~ hp + wt, data = dat) 
anova(fit0, fit) 

