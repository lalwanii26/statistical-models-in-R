load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names
dat = tmp
#attach(as.data.frame(dat))

# The usual linear fit
plot(mpg ~ hp, data = dat, pch = 16)
fit <- lm(mpg ~ hp, data = dat)
abline(fit, lwd = 2, col = 'red')
summary(fit)
confint(fit)
abline(a = confint(fit)[1,1], b = confint(fit)[2,2], col = 'red', lwd = 2, lty = 2)
abline(a = confint(fit)[1,2], b = confint(fit)[2,1], col = 'red', lwd = 2, lty = 2)

# Nonparametric bootstrap
B = 1000
n = nrow(dat)
coef.boot <- matrix(0, B, 2)
for (b in 1:B) {
  indices = sample(seq(1,n), replace=T)
  m.boot = lm(mpg ~ hp, data = dat[indices,])
  coef.boot[b,] = m.boot$coefficients
}
head(coef.boot)
sd.boot <- apply(coef.boot, 2, sd)
sd.boot

hist(coef.boot[,2], 20);
abline(v=coef(fit)[2], col='black', lwd=2)        # Original estimate
abline(v=confint(fit)[2,1], col='black', lwd=2)   # Original CI
abline(v=confint(fit)[2,2], col='black', lwd=2)   # Original CI

abline(v=mean(coef.boot[,2]), col='blue', lwd=2)  # Bootstrap estimate
abline(v=quantile(coef.boot[,2], probs = c(0.025, 0.975)), col='blue', lwd=2)  # Quantile CI

confint.boot <- coef(fit)[2] + c(-1,1)*sd.boot[2]*qt(0.025, df=n-1)
abline(v=confint.boot[1], col='red', lwd=2)   # Boostrap-t CI
abline(v=confint.boot[2], col='red', lwd=2)   # Boostrap-t CI


#-------------------------------------------------------------------------------
# Bootstrap simulation

# Gamma median
(theta = qgamma(0.5, shape=3))

# Gamma simulation
B = 1000
n = 150
y.b = matrix(0, B, n)
for(b in 1:B) y.b[b,] = rgamma(n, shape=3)
hist(y.b[,1])
curve(B*dgamma(x, shape=3), add=T, col='orange')

ymed = apply(y.b, 1, median)
hist(ymed, 20);
abline(v=theta, col='black', lty=2, lwd=2)
abline(v=mean(ymed), col='blue', lwd=2)
abline(v=quantile(ymed, probs = c(0.025, 0.975)), col='red', lwd=2)
mean(ymed); sd(ymed)
quantile(ymed, probs = c(0.025, 0.975))

# Single gamma sample
n = 150
y = rgamma(n, shape=3)
median(y)
hist(y)
abline(v=theta, col='black', lty=2, lwd=2)
abline(v=median(y), col='blue', lwd=2)
curve(n*dgamma(x, shape=3), add=T, col='orange')

# Gamma bootstrap
B = 1000
y.b = matrix(0, B, n)
for(b in 1:B) y.b[b,] = sample(y, n, replace = T)
ymed = apply(y.b, 1, median)
hist(ymed, 20);
abline(v=theta, col='black', lty=2, lwd=2)
abline(v=mean(ymed), col='blue', lwd=2)
abline(v=quantile(ymed, probs = c(0.025, 0.975)), col='red', lwd=2)
abline(v=mean(ymed) + sd(ymed)*qt(0.025, df=n-1), col='red', lwd=2, lty=2)
abline(v=mean(ymed) - sd(ymed)*qt(0.025, df=n-1), col='red', lwd=2, lty=2)

# Bootstrap estimate of SE
mean(ymed); sd(ymed)

# Boostrap CI
quantile(ymed, probs = c(0.025, 0.975))
c(mean(ymed) + sd(ymed)*qt(0.025, df=n-1), mean(ymed) - sd(ymed)*qt(0.025, df=n-1))



