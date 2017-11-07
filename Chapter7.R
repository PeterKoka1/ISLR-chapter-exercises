library(ISLR)
attach(Wage)
library(boot)
set.seed(123)

val.errors <- rep(NA, 15)
for (i in 1:15) {
  fit.i <- glm(wage ~ poly(age, i), data = Wage)
  cv.i <- cv.glm(Wage, fit.i, K=10)$delta[1]
  val.errors[i] = cv.i    
}
val.errors
par(mfrow=c(1,1))
title("10 Fold Cross Validation on Polynomial Regression")
x.range <- seq(1:length(val.errors))
plot(x.range, val.errors, xlab = "Degrees", ylab = "Test MSE", type = "l")
which.min(val.errors) # 6 deg model -> lowest test MSE
points(6, val.errors[6], pch = 20, cex = 2, col = "red")

# ANOVA testing
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
fit.6 <- lm(wage ~ poly(age, 6), data = Wage)
fit.7 <- lm(wage ~ poly(age, 7), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7)
# deg 4 has 0.05 significance, higher order polynomials don't offer much
# may even make sense to use cubic instead (p-value of 0.0017)

title("Wage vs. Age Cubic Polynomial Regression")
plot(wage ~ age, data = Wage, col = "gray")
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
fit2 <- lm(wage ~ poly(age, 4, raw=TRUE), data = Wage)
coef(summary(fit2))
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(summary(fit2a))

# creating grid values for age at which we want predictions
agelims <- range(age) # 18 80
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim = agelims, cex = 0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands,lwd = 1, col="red", lty = 3)

# finding proper degree polynomial from 1 to 5
fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age,2), data=Wage)
fit.3 <- lm(wage~poly(age,3), data=Wage)
fit.4 <- lm(wage~poly(age,4), data=Wage)
fit.5 <- lm(wage~poly(age,5), data=Wage)
# anova() performs an analysis of variance (ANOVA, using an F-test) in order to test the null
# that the a model M1 is sufficient to explain the data against the alternative that a more 
# complex model M2 is required. 
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# also could have done coef(summary(fit.5)) because poly() creates orthogonal polynomials
coef(summary(fit.5)) # again, shows degree-4 polynomial is best fit

# Now, we predict whether or not an individual earns more than $250,000 per year
# Proceed the same way as before, except first create the appropriate response vector and then apply
# the glm() function using family = "binomial"
fit <- glm(I(wage>250) ~ poly(age,4), data = Wage, family = "binomial")
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
# since we fit model using logit, the standard errors are also in log form
# to evaluate natural confidence intervals for Pr(Y=1|X), we use transformations with e
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage>250), xlim=agelims, type = "n", ylim=c(0,.15))
points(jitter(age), I((wage>250)/5), cex = 0.5, pch = "|",
       col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "red", lty = 3)

# now for step functions, we use cut() function
table(cut(age,4))
fit <- lm(wage ~ cut(age,4), data = Wage)
coef(summary(fit))
# the age < 33.5 is left out so the intercept coefficient is the average salary 
# for those under 33.5 years of age



###: SPLINES

library(splines)
# the bs() function generates an entire matrix of basis functions for splines with the 
# specified set of knots. By default, cubic splines are produced
fit <- lm(wage ~ bs(age, knots = c(25,40,60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se=TRUE)
par(mfrow=c(1,1))
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
# matlines(age.grid, pred$fit + 2*pred$se, lwd = 1, col = "red", lty = 3)
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")
# having specified 3 knots at 25, 40, and 60, we have K + 4 = 7 degrees of freedom

# we can specify degrees in spline (rather than 3 which is default)
fit <- lm(wage ~ bs(age, knots = c(25,40,60), degree = 6), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")

# in order to fit a NATURAL spline, we use ns()
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age=age.grid), se=TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)
# can also specify number of knots as with bs function

# in order to fit a SMOOTHING spline, we use smooth.spline()
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv=TRUE)
fit2$df # 6.8
lines(fit, col="red", lwd = 2)
lines(fit2, col="blue", lwd = 2)
legend("topright", legend=c("16 DF", "6.8 DF"),
       col=c("red","blue"), lty=1, lwd=2, cex=.8)


###: GAMS

# fit a GAM to predict wage using natrual spline functions of year and age, treating education
# as a qualitative predictor
gam1 <- lm(wage ~ ns(year, df = 4) + ns(age, df = 5) + education, data = Wage)
# to fit components than cannot be expressed in terms of basis functions, we use gam library
# the s() function in gam is used to indicate a smoothing spline
library(gam)
gam.m3 <- gam(wage ~ s(year, df=4) + s(age, df=5) + education, data = Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
# plot() understands this is an object of class gam() (it will understand the lm() with gam as well)
# but use plot.gam()

# we notice year looks rather linear here. We can now use a series of ANOVA tests in order to determine
# which of these three models is best: a GAM that excludes year (M1), a GAM that uses a linear
# function of year (M1), a GAM that uses a spline function of year (M3)

gam.m1 <- gam(wage ~ s(age, df = 5) + education, data = Wage)
gam.m2 <- gam(wage ~ s(age, df = 5) + year + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
# we find that linear model is best for year
summary(gam.m3)

# make predictions for gam objects, just like from lm objects
preds <- predict(gam.m2, newdata = Wage)

# in order to fit a logistic regression GAM, we use the I() function in constructing the
# binary response variable, and set family = "binomial"
gam.lr <- gam(I(wage>250) ~ year + s(age, df = 5) + education, data = Wage,
              family = "binomial")
par(mfrow=c(1,3))
plot(gam.lr, se=TRUE, col="green")
# clear to see there are no high earners in the <HS category, therefore we can remove highschool 
gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df=5) + education, family = "binomial",
                data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se=TRUE, col = "green")