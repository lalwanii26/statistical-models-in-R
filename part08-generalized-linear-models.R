load('datasets/AircraftDamage.rda')
dat = AircraftDamage
head(dat)
dat = dat[,-1]

pairs(dat, pch = 20)

# Linear regression
fit.ols = lm(y ~ ., data = dat)
summary(fit.ols)
require(car)
residualPlots(fit.ols)
fitted.values(fit.ols)

# Poisson regression
fit.pois = glm(y ~ ., data = dat, family=poisson)
summary(fit.pois)
fitted.values(fit.pois)

# some diagnostic plots
require(car)
residualPlots(fit.pois)

plot(fit.pois, which = 4)

# analysis of deviance
anova(fit.pois, dispersion=1, test='LRT')

# forward AIC selection
require(MASS)
stepAIC(fit.pois)


load('datasets/EducationByAge.rda')
dat = edbyage
print(dat)
addmargins(as.matrix(dat))

# barplots
barplot(as.matrix(dat), xlab="Degree", legend=TRUE, args.legend=list(x = "bottom", horiz = TRUE, inset = 1.2, cex = 0.8))
barplot(t(as.matrix(dat)), xlab="Age", legend=TRUE, args.legend=list(x = "bottom", horiz = TRUE, inset = 1.2, cex = 0.8))

# chi-square test of independence
chisq.test(dat)

# analysis of deviance
y = matrix(as.matrix(dat), 20, 1)
Deg = gl(4, 5, labels = names(dat))
Age = gl(5, 1, labels = row.names(dat), length = 20)
print(cbind(Age, Deg, y))
fit0 = glm(y ~ Deg + Age, family=poisson) # w/o interactions
fit1 = glm(y ~ Deg*Age, family=poisson) # w/ interactions
anova(fit0, fit1, dispersion=1, test='LRT')
AIC(fit0, fit1) # the full model has much smaller AIC


load('datasets/cleveland.rda')
dat = cleveland
str(dat)

fit = glm(cond ~ ., data = dat[, -15], family = binomial(link = "logit"))
summary(fit)
plot(fit, which = 4)
residualPlots(fit)
anova(fit, dispersion=1, test='LRT')

# stepwise AIC selection
require(MASS)
stepAIC(fit)

# glm() does not seem to handle multinomial regression.  Below are two options.

require(nnet)
fit = multinom(condplus ~ ., data = dat[, -14])
summary(fit)
stepAIC(fit)

require(VGAM)
fit = vglm(condplus ~ ., data = dat[, -14], family=multinomial())
summary(fit)
