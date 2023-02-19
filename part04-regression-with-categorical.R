load("datasets/04cars.version2.rda")
tmp = dat[,c(2,3,7,8,10,11)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("type", "drive", "cyl", "hp","mpg","wt") # abbreviate names

# most cars have either 4, 6, or 8 cylinders
with(tmp, table(cyl))

# we remove the other ones for the sake of presenting the material
dat = subset(tmp, (cyl==4)|(cyl==6)|(cyl==8))

## mpg on cyl as numeric
dat$cyl = as.numeric(dat$cyl)
summary(dat$cyl)
plot(mpg ~ cyl, data = dat, pch = 16)
fit = lm(mpg ~ cyl, data = dat)
summary(fit)
abline(fit, col="red", lwd = 2)

# diagnostic plots
par(mfrow=c(1,3))
plot(fit, which = c(1,2,4))

## mpg on cyl as categorical (factor)
dat$cyl = as.factor(dat$cyl)
summary(dat$cyl)
par(mfrow=c(1,1))
boxplot(mpg ~ cyl, data = dat, ylab='MILES PER GALLON', xlab='NUMBER OF CYLINDERS', lwd=2, pch = 16)
fit = lm(mpg ~ cyl, data = dat)
summary(fit)
anova(fit)

## mpg on drive and cyl w/o interactions
dat$drive = as.factor(dat$drive)
fit = lm(mpg ~ drive + cyl, data = dat)
summary(fit)
anova(fit)

# compare with
fit0 = lm(mpg ~ 1, data = dat); fit1 = lm(mpg ~ drive, data = dat); anova(fit0, fit1)
fit0 = lm(mpg ~ 1, data = dat); fit1 = lm(mpg ~ cyl, data = dat); anova(fit0, fit1)
fit0 = lm(mpg ~ drive, data = dat); fit1 = lm(mpg ~ drive + cyl, data = dat); anova(fit0, fit1)

# compare with
fit = lm(mpg ~ cyl + drive, data = dat)
summary(fit)
anova(fit)

## mpg on drive and cyl w/ interactions
interaction.plot(dat$cyl, dat$drive, dat$mpg, col=2:4, lwd=2, cex.axis=1, cex.lab=1)
fit = lm(mpg ~ drive * cyl, data = dat)
summary(fit)
anova(fit)

## mpg on wt and drive w/ interactions
pairs(mpg ~ wt + drive, data = dat, pch = 16)
ind = (dat$drive==0)
plot(mpg[ind] ~ wt[ind], data = dat, col=2, pch=2)
fit = lm(mpg[ind] ~ wt[ind], data = dat)
abline(fit, col=2, lwd=2)  
ind = (dat$drive==1)
points(mpg[ind] ~ wt[ind], data = dat, col=3, pch=3)
fit = lm(mpg[ind] ~ wt[ind], data = dat)
abline(fit, col=3, lwd=2)  
ind = (dat$drive==2)
points(mpg[ind] ~ wt[ind], col=4, pch=4)
fit = lm(mpg[ind] ~ wt[ind], data = dat)
abline(fit, col=4, lwd=2)  

# Instead of fitting a model to each group defined by drive (as we just did), we now fit the model with wt and drive.  The only thing that really changes is our estimate for the error variance, which is here assumed to be the same across group.
fit = lm(mpg ~ wt * drive, data = dat)
summary(fit)
anova(fit)
