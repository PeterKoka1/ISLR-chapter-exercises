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

set.seed(1)
cut.cv.error <- rep(NA, 10)
for (i in 2:length(cut.cv.error)) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cut.cv.error[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
xrange <- seq(from = 2, to = 10)
testMSE <- cut.cv.error[!(is.na(cut.cv.error))]
plot(xrange, testMSE, xlab = "Number of Cuts", ylab = "Test MSE", type = "l")
which.min(cut.cv.error)
points(8, cut.cv.error[8], col = "red", pch = 20, cex = 2)
title("Step Function - Number of Cuts 10-Fold CV")

plot(wage ~ age, data = Wage, col = "gray")
age.lims <- range(age)
age.grid <- seq(from = age.lims[1], to = age.lims[2])
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)
title("Step Function with Cutpoints = C = 8")

attach(Auto)
pairs(Auto)
# wage looks like it can be modelled by displacement, horsepower and weight

# Polynomial Regresion
cv.errors <- rep(NA, 15)
for (i in 1:length(cv.errors)) {
  fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.errors[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
xrange <- seq(from = 1, to = length(cv.errors))
cv.errors
plot(xrange, cv.errors, type = "l")
which.min(cv.errors)
points(6, cv.errors[6], col = "red", pch = 20, cex = 2)

poly.fit <- glm(mpg ~ poly(horsepower, 6), data = Auto)
horselims <- range(horsepower)
horse.grid <- seq(from = horselims[1], to = horselims[2])
poly.pred <- predict(poly.fit, newdata = list(horsepower = horse.grid))

# Step Functions
cv.errors <- rep(NA, 20)
for (i in 2:length(cv.errors)) {
  Auto$horsepower.cut <- cut(Auto$horsepower, i)
  fit <- glm(mpg ~ horsepower.cut, data = Auto)
  cv.errors[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
xrange <- seq(from = 2, to = length(cv.errors))
plot(xrange, cv.errors[!(is.na(cv.errors))], 
     xlab = "Number of Cuts", ylab = "Test MSE", type = "l")
which.min(cv.errors)
points(15, cv.errors[15], col = "red", pch = 20, cex = 2)

step.fit <- glm(mpg ~ cut(horsepower, 15), data = Auto)
step.pred <- predict(step.fit, newdata = list(horsepower = horse.grid))

# Spline Functions
library(splines)
cv.errors <- rep(NA, 15)
for (i in 1:length(cv.errors)) {
  spline.fit <- glm(mpg ~ ns(horsepower, df = i), data = Auto)
  cv.errors[i] <- cv.glm(Auto, spline.fit, K = 10)$delta[1]
}
xrange <- seq(from = 1, to = 15)
plot(xrange, cv.errors, xlab = "Degrees of Freedom", ylab = "Test MSE", type = "l")
which.min(cv.errors)
points(10, cv.errors[10], col = "red", pch = 20, cex = 2)

ns.spline.fit <- lm(mpg ~ ns(horsepower, df = 10), data = Auto)
spline.pred <- predict(ns.spline.fit, newdata = list(horsepower = horse.grid))

par(mfrow=c(1,3))
plot(mpg ~ horsepower, data = Auto, col = "darkgrey", main = "Polynomial Regression")
lines(horse.grid, poly.pred, col = "red", lwd = 2)
plot(mpg ~ horsepower, data = Auto, col = "darkgrey", main = "Step Function")
lines(horse.grid, step.pred, col = "blue", lwd = 2)
plot(mpg ~ horsepower, data = Auto, col = "darkgrey", main = "Natural Spline w/ 10 Degrees of Freedom")
lines(horse.grid, spline.pred, col = "red", lwd = 2)

attach(Boston)
set.seed(123)
fit <- glm(nox ~ poly(dis, 3), data = Boston)
dislim <- range(dis)
dis.grid <- seq(from = dislim[1], to = dislim[2], by = 0.01)
preds <- predict(fit, newdata = list(dis = dis.grid))
par(mfrow=c(1,1))
plot(nox ~ dis, data = Boston, col = "darkgray", 
     main = "Distance vs. Nitrogen Oxides Concentration")
lines(dis.grid, preds, col = "red", lwd = 3)
