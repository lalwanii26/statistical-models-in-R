load("datasets/alcoholtobacco.rda") 
#attach(alcoholtobacco)
# Average weekly expenditure on tobacco (British pounds) and average weekly expenditure on alcohol (British pounds) for households in n = 11 regions in the United Kingdom.

plot(Tobacco ~ Alcohol, data = alcoholtobacco, pch=16, xlim=c(3,7), ylim=c(2,5), cex=1)
text(Tobacco ~ Alcohol, data = alcoholtobacco, Region, pos=1, cex=1)

fit.ls = lm(Tobacco ~ Alcohol, data = alcoholtobacco)
abline(fit.ls, col='red', lwd=2)
summary(fit.ls)

# various diagnostics for detecting influential points
library(car)
residualPlot(fit.ls)
plot(residuals(fit.ls) ~ Alcohol, data = alcoholtobacco, pch=16); abline(h=0)
I = influence.measures(fit.ls)
print(I)
I$is.inf

# we remove the obvious outlier
fit.outlier = lm(Tobacco[-11] ~ Alcohol[-11], data = alcoholtobacco)
summary(fit.outlier)

# the fit is very different
plot(Tobacco ~ Alcohol, data = alcoholtobacco, pch=16, xlim=c(3,7), ylim=c(2,5))
abline(fit.ls, lwd=2)
abline(fit.outlier, lwd=2, lty=3)
legend('bottomright', c('LS','LS no outlier'), lwd=3, lty=c(1,2))

require(quantreg)

# L1 regression
fit.l1 = rq(Tobacco ~ Alcohol, data = alcoholtobacco)
abline(fit.l1, lwd=3, col='blue')
legend('bottomright', c('LS','LS no outlier', 'L1'), col=c(1, 1, 'blue'), lwd=3, lty=c(1,2,1), bg='white')


# Psi functions for various M-estimators
require(MASS)
x = seq(-100, 100, 0.1)
par(mfrow=c(2,3))
plot(x, psi.huber(x), lwd=3, col='red', type='l', xlab="", ylab="", main="Huber's Weight")
plot(x, psi.hampel(x), lwd=3, col='red', type='l', xlab="", ylab="", main="Hampel's Weight")
plot(x, psi.bisquare(x), lwd=3, col='red', type='l', xlab="", ylab="", main="Tukey's Weight")

# fitting M-estimators
fit.huber = rlm(Tobacco ~ Alcohol, data = alcoholtobacco, maxit=50)
summary(fit.huber)
plot(fit.huber$w, ylab="", main="Huber weights", type='h', lwd=3, col="orange")

fit.hampel = rlm(Tobacco ~ Alcohol, data = alcoholtobacco, maxit=50, psi = psi.hampel)
summary(fit.hampel)
plot(fit.hampel$w, ylab="", main="Hampel weights", type='h', lwd=3, col="purple")

fit.tukey = rlm(Tobacco ~ Alcohol, data = alcoholtobacco, maxit=50, psi = psi.bisquare)
summary(fit.tukey)
plot(fit.tukey$w, ylab="", main="Tukey weights", type='h', lwd=3, col="red")

dev.set(2)	
abline(fit.huber, col="orange", lwd=3)
abline(fit.hampel, col="purple", lwd=3)
abline(fit.tukey, col="red", lwd=3)
legend('bottomright', c('LS','LS no outlier','L1','Huber','Hampel','Tukey'), col=c('black','black','blue','orange','purple','red'), lwd=3, lty=c(1,2,1,1,1,1), bg='white')


# high breakdown point methods
fit.lms = lmsreg(Tobacco ~ Alcohol, data = alcoholtobacco)
fit.lts = ltsreg(Tobacco ~ Alcohol, data = alcoholtobacco)

plot(Tobacco ~ Alcohol, data = alcoholtobacco, pch=16, xlim=c(3,7), ylim=c(2,5), cex=1)
abline(fit.ls, lwd=3)
abline(fit.outlier, lwd=3, lty=2)
abline(fit.lms, lwd=3, col="green")
abline(fit.lts, lwd=3, col="maroon")
legend('bottomright', c('Least Squares','LS no outlier','Least Median','Trimmed Mean'), col=c('black','black','green','maroon'), lwd=3, lty=c(1,2,1,1))

