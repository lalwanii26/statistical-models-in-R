require(car)
load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] 
tmp = tmp[complete.cases(tmp),]
tmp = as.data.frame(tmp)
names(tmp) = c("hp", "mpg", "wt", "len", "wd")
dat = tmp
#attach(dat)

# let's focus on mpg ~ hp
# we fit a line
fit = lm(mpg ~ hp, data = dat)
summary(fit)
plot(mpg ~ hp, data = dat, pch = 16, cex = 1)
abline(fit, col = "blue", lwd = 3)

# fitting a line in log-log scale
hist(dat$mpg, 20)
hist(log(dat$mpg), 20)
hist(dat$hp, 20)
hist(log(dat$hp), 20)
plot(log(mpg) ~ hp, data = dat, pch = 16)
plot(mpg ~ log(hp), data = dat, pch = 16)
plot(log(mpg) ~ log(hp), data = dat, pch = 16)

logfit = lm(log(mpg) ~ log(hp), data = dat)
summary(logfit)
plot(log(mpg) ~ log(hp), data = dat, pch = 16)
abline(logfit, col = "red", lwd = 3)

# comparing the two fits visually
logfit = lm(mpg ~ hp, data=log(dat))
pts = seq(0, 600, len=100) # covers the range of hp
val = predict(logfit, data.frame(hp = log(pts)))
plot(mpg ~ hp, data = dat, pch = 16, cex = 1)
abline(fit, col = "blue", lwd = 3)
lines(pts, exp(val), col="red", lwd = 3)

# fitting polynomial of degree 2
fit = lm(mpg ~ hp + I(hp^2), data = dat)
summary(fit)
# same as
fit = lm(mpg ~ poly(hp, 2, raw = TRUE))
summary(fit)
val = predict(fit, data.frame(hp = pts))
#plot(hp, mpg, pch = 16)
lines(pts, val, col="green", lwd = 3)

# R-squared for polynomial models of increasing degree
dmax = 30
R0 = rep(NA, dmax)
for (d in 1:dmax){
	fit = lm(mpg ~ poly(hp, d, raw = TRUE), data = dat)
	S = summary(fit)
	# print(S) 
	R0[d] = S$adj.r.squared
	}
plot(1:dmax, R0, type="o", xlab = "Degree", ylab = "R-squared", main = "R-squared for polynomial models", lwd = 3, col = "blue", pch = 5)

# comparing polynomial models visually
plot(hp, mpg, pch = 16)
for (d in 1:10){
	fit = lm(mpg ~ poly(hp, d, raw = TRUE))
	val = predict(fit, data.frame(hp = pts))
	lines(pts, val, col=rainbow(10)[d], lwd = 3)
	}

# piecewise constant fit
plot(mpg ~ hp, data = dat, pch = 16, main="Piecewise constant fit", cex = 1)
K = quantile(dat$hp, seq(0, 1, len = 6), type=1)
pts = rep(0,10)
val = rep(0,10)
for (j in 1:5){
	I = (K[j] < dat$hp)&(dat$hp <= K[j+1])
	fit = lm(mpg[I] ~ 1, data = dat)
	pts[2*j-1] = K[j]
	pts[2*j] = K[j+1]
	val[2*j-1] = coef(fit)
	val[2*j] = coef(fit)
	}
lines(pts, val, col="red", lwd = 3)

# piecewise linear fit
plot(mpg ~ hp, data = dat, pch = 16, main="Piecewise linear fit", cex = 1)
K = quantile(dat$hp, seq(0, 1, len = 6), type=1)
pts = rep(0,10)
val = rep(0,10)
for (j in 1:5){
	I = (K[j] < dat$hp)&(dat$hp <= K[j+1])
	fit = lm(mpg[I] ~ hp[I], data = dat)
	pts[2*j-1] = K[j]
	pts[2*j] = K[j+1]
	val[2*j-1] = coef(fit)%*%c(1,K[j])
	val[2*j] = coef(fit)%*%c(1,K[j+1])
	}
lines(pts, val, col="green", lwd = 3)

# splines fit of degree 1 (linear)
require(splines)
plot(mpg ~ hp, data = dat, pch = 16, main="Linear spline fit", cex = 1)
K = quantile(dat$hp, c(.2,.4,.6,.8), type=1)
fit = lm(mpg ~ bs(hp, degree=1, knots=K), data = dat)
pts = seq(0, 600, len=100)
val = predict(fit, data.frame(hp = pts), data = dat)
lines(pts, val, col="red", lwd = 3)

# splines fit of degree 3 (cubic)
K = quantile(dat$hp, c(.2,.4,.6,.8), type=1)
plot(mpg ~ hp, data = dat, pch = 16, main="Cubic spline fit", cex = 1)
fit = lm(mpg ~ bs(hp,degree=3,knots=K), data = dat)
val = predict(fit, data.frame(hp = pts))
lines(pts, val, col="blue", lwd = 3)

# B-spline basis for the same knots
K = quantile(hp, c(.2,.4,.6,.8), type=1)
B1 = bs(pts, degree=1, knots=K, intercept=TRUE)
B3 = bs(pts, degree=3, knots=K, intercept=TRUE)
par(mfrow=c(1,3))
matplot(pts, B1, type="l", lwd = 3, ylab="", xlab="", main="Linear B-Splines")
matplot(pts, B3, type="l", lwd = 3, ylab="", xlab="", main="Cubic B-Splines")

# natural splines for the same knots
N = ns(pts, knots = K[2:3], intercept = TRUE, Boundary.knots = K[c(1,4)])
matplot(pts, N, type="l", lwd = 3, ylab="", xlab="", main="Natural Splines")

# smoothing splines fit
par(mfrow=c(1,1))
plot(hp, mpg, pch = 16, main="Smoothing splines fit", cex = 1)
fit = smooth.spline(hp, mpg)
lines(fit, col="red", lwd = 3)
# smoother look if the model is evaluated at more locations
fitted = predict(fit, pts) 
lines(fitted, col="green", lwd = 3)
# controlling the degrees of freedom by hand
fit = smooth.spline(hp, mpg, df=10)
fitted = predict(fit, pts) 
lines(fitted, col="blue", lwd = 3)

# polynomial model of degree 2 in hp and wt
fit = lm(mpg ~ hp, data = dat)
summary(fit)
plot(mpg ~ hp, data = dat, pch = 16, main="Fit with no interaction", cex = 1)
val = predict(fit)
points(dat$hp, val, col="red", lwd = 3)

fit = lm(mpg ~ hp + wt, data = dat)
summary(fit)
val = predict(fit)
points(dat$hp, val, col="green", lwd = 3)

fit = lm(mpg ~ hp*wt + I(hp^2) + I(wt^2), data = dat)
#fit = lm(mpg ~ hp*wt, data = dat)
summary(fit)
val = predict(fit)
points(dat$hp, val, col="blue", lwd = 3)

# spline model of degree 1 + interactions in hp and wt
fit = lm(mpg ~ bs(hp, degree=1, df=6) * bs(wt, degree=1, df=6), data = dat)
summary(fit)

# MARS algorithm for fitting multi-dimensional splines
require(mda)
mars.fit = mars(cbind(hp, wt), mpg)

#-------------------------------------------------------------------------------
# Mauna Loa CO2 data

CO2 <- read.table('ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt')
CO2 <- CO2[,3:4]
names(CO2) = c('year', 'ppm')

# Linear model
plot(ppm ~ year, data=CO2, pch=20); grid(lty='solid')
abline(lm(ppm ~ year, data=CO2), col='red', lwd=2)

# Log transform
CO2 = transform(CO2, lppm = log(ppm))
plot(lppm ~ year, data=CO2, pch=16); grid(lty='solid')
abline(lm(lppm ~ year, data=CO2), col='red', lwd=2)

# Quadratic model
plot(ppm ~ year, data=CO2, pch=16); grid(lty='solid')
m2 <- lm(ppm ~ year + I(year^2), data=CO2)
lines(predict(m2) ~ year, data=CO2, col='red', lwd=2)
summary(m2)
CO2 <- transform(CO2, pastyear = year - 2023)
m2 <- lm(ppm ~ pastyear + I(pastyear^2), data=CO2)
summary(m2)
summary(lm(ppm ~ poly(pastyear, 2, raw=T), data=CO2))

# Residuals
plot(CO2$year, residuals(m2), type = 'l')
abline(h=0)
K <- quantile(CO2$pastyear, seq(0,1,by=0.1), type=1)
B1 <- bs(CO2$pastyear, knots=K, degree=3, intercept=TRUE)
matplot(B1, type='l')
mrs <- lm(residuals(m2) ~ B1)
plot(CO2$year, residuals(m2), type = 'l')
lines(CO2$year, predict(mrs), col='blue')
plot(residuals(mrs) ~ pastyear, data = CO2, type = 'l')
abline(h=0)

# Seasonal effect
nyears <- ceiling(nrow(CO2)/12)
seasonal <- array(residuals(mrs), dim = c(12,nyears))
matplot(seasonal, type='l', col = 'gray')
seasonal.mean <- rowMeans(seasonal)
lines(seasonal.mean, lwd = 2, col = 'blue')

# Complete fit
plot(ppm ~ year, data=CO2, pch=20); grid(lty='solid')
lines(CO2$year, predict(m2) + predict(mrs) + array(seasonal.mean, dim = c(nrow(CO2),1)), col = 'red')
      