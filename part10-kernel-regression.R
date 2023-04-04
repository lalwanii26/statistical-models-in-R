f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)

n = 1e3
x = runif(n)
x = sort(x)

y = f(x) + rnorm(n)

plot(x, y, col = "grey", pch=16)
t = seq(0, 1, len=100)
lines(t, f(t), lwd=2)

# Smoothing splines
require(splines)
fit = smooth.spline(x, y)
lines(fit, col="red", lwd = 3)
fit$df 	# degrees of freedom

# Kernel smoothing
h = 0.10
fit = ksmooth(x, y, 'normal', bandwidth = h)
lines(x, fit$y, col = 4, lwd=3)
# degrees of freedom not provided...

# Local linear regression (loess)
require(MASS)
h = 0.25
fit = loess(y ~ x, span = h)
val = predict(fit, data.frame(x = t))
lines(t, val, col = 'blue', lwd=2)
fit$tr		# degrees of freedom
	
