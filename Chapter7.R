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

par(mfrow = c(1,1))
fit <- lm(wage ~ poly(age, 3), data = Wage)
plot(wage ~ age, data = Wage, col = "gray")
title("Wage vs. Age Cubic Polynomial Regression")
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
lines(age.grid, preds$fit, lwd = 4, col = "blue")
matlines(age.grid, se.bands, lwd = 2, col = "blue", lty = 3)

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

SSE <- rep(NA, 10)
par(mfrow=c(2,5))
for (i in 1:length(errors)) {
  fit <- lm(nox ~ poly(dis, i), data = Boston)
  SSEi <- sum(fit$residuals^2)
  SSE[i] <- SSEi
  
  plot(nox ~ dis, data = Boston, col = "darkgray", 
       pch = 20)
  title(sprintf("Polynomial Fit With Degree %d", i), pch = 0.5)
  preds <- predict(fit, newdata = list(dis = dis.grid), se = TRUE)
  lines(dis.grid, preds$fit, lwd = 3, col = "red")
  se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
  matlines(dis.grid, se.bands, lwd = 2, col = "red", lty = 2)
}
par(mfrow=c(1,1))
plot(1:length(SSE), SSE, type = "l", 
     xlab = "Number of Degrees", ylab = "Sum of Squared Errors")
title("Polynomial Fits with Various Degrees on Distance vs. Nitrogen Oxide Concentration",
      pch = 0.2)
which.min(SSE)
points(10, SSE[10], col = "red", pch = 20, cex = 2)
# obviously, as we get more flexible, train SSE decreases monotonically


k.fold.iters <- function() {

  iter.errors <- rep(NA, 50)
  ret.val.errors <- rep(0, 10)
  for (i in 1:length(iter.errors)) {
    set.seed(i)
    val.errors <- rep(NA, 10)
    for (j in 1:length(val.errors)) {
      fit <- glm(nox ~ poly(dis, j), data = Boston)
      cv.error <- cv.glm(data = Boston, fit, K=10)$delta[1]
      val.errors[j] = cv.error
      ret.val.errors[j] = ret.val.errors[j] + cv.error
    }
    min <- which.min(val.errors)
    iter.errors[i] = min
  }
  
  print(sd(iter.errors))
  
  ret.val.errors = ret.val.errors / length(iter.errors)
  return(c(mean(iter.errors), ret.val.errors))
}

mean.k.fold <- k.fold.iters()

cross.validation <- function() {

  k.fold.reps <- k.fold.iters()
  
  min <- trunc(k.fold.reps[1])
  val.errors <- k.fold.reps[2:length(mean.k.fold)]
  dis.range <- range(dis)
  if (dis.range[2] > 50) {
    grid.dis <- seq(from = dis.range[1], to = dis.range[2])
  } else {
    grid.dis <- seq(from = dis.range[1], to = dis.range[2], by = 0.5)
  }
  fit <- lm(nox ~ poly(dis, min), data = Boston)
  preds <- predict(fit, newdata = list(dis = grid.dis), se = TRUE)
  
  par(mfrow=c(1,2))
  plot(1:length(val.errors), val.errors, type = "l",
       xlab = "Test MSE", ylab = "Number of Degrees")
  title(sprintf("10-Fold Cross-Validation with d = 1-%d",range(val.errors)[2]))
  points(min, val.errors[min], col = "navyblue", pch = 20, cex = 2)
  
  plot(nox ~ dis, data = Boston, col = "gray48", pch = 1, 
       xlab = "Distance", ylab = "Nitrogen Oxide Concentration")
  title(sprintf("Cross-Validated %d-Degree Polynomial Regression", min))
  lines(grid.dis, preds$fit, col = "navyblue", lwd = 3)
  se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
  matlines(grid.dis, se.bands, col = "skyblue1", lty = 2, lwd = 2)
  labels <- c("Linear Regression", "Squared Regression", 
              "Cubic Regression", "Quadratic Regression")
  for (i in 5:length(val.errors)) {
    ith.deg <- sprintf("%dth Degree Regression", i)
    label.i <- cbind(labels, ith.deg)
    labels <- label.i
  }
  plot.labels <- labels[1:length(val.errors)]
  reg.type.plot <- ""
  for (i in 1:length(val.errors)) {
    if ( i == min ) {
      reg.type.plot <- plot.labels[i]
    }
  }
  legend("topright", legend=c(reg.type.plot, "95% Confidence Interval"),
         col=c("navyblue","skyblue"), lty=c(1,2), lwd=c(3,2), seg.len = 2, cex=0.8)
}

cross.validation()

fit <- lm(nox ~ bs(dis, degree = 4), data = Boston)
summary(fit)
pred <- predict(fit, newdata = list(dis = grid.dis))
par(mfrow=c(1,1))
plot(nox ~ dis, data = Boston, col = "darkgray", pch = 20)
lines(grid.dis, pred, col = "navyblue", lwd = 3)

val.errors <- rep(NA, 12)
for (i in 1:length(val.errors)) {
  fit <- lm(nox ~ bs(dis, df = i+3), data = Boston)
  val.errors[i] = sum(fit$residuals^2)
}
plot(1:length(val.errors), val.errors, type = "l")
points(which.min(val.errors), val.errors[which.min(val.errors)], 
       col = "red", pch = 20, cex = 2)

val.errors <- rep(NA, 12)
for (i in 1:length(val.errors)) {
  fit <- glm(nox ~ bs(dis, df = i+3), data = Boston)
  cv.error <- cv.glm(data = Boston, fit, K=10)$delta[1]
  val.errors[i] = cv.error
}
plot(1:length(val.errors), val.errors, type = "l")
min <- which.min(val.errors)
points(min, val.errors[min], col = "red", pch = 20, cex = 2) # 5 degrees of freedom

library(leaps)
set.seed(123)
attach(College)
train <- sample(1:dim(College)[1], dim(College)[1] / 2)
test <- !(train)
length(names(College))
reg.fwd <- regsubsets(Outstate ~., data = College[train, ], nvmax = 
                        length(names(College)) - 1,
                      method = "forward")
regfit.sum <- summary(reg.fwd)
par(mfrow=c(1,3))
plot(regfit.sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Squared",
     type = "l")
abline(h = adr2.max - sd(regfit.sum$adjr2), lty = 2, col = "red")
points(adjr2.max, regfit.sum$adjr2[adjr2.max], col = "red", pch = 20, cex = 1.5)
plot(regfit.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic.min <- which.min(regfit.sum$bic)
points(bic.min, regfit.sum$bic[bic.min], col = "red", pch = 20, cex = 1.5)
plot(regfit.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp.min <- which.min(regfit.sum$cp)
points(cp.min, regfit.sum$cp[cp.min], col = "red", pch = 20, cex = 1.5)
mins <- c(adjr2.max, bic.min, cp.min)

reg.fwd <- regsubsets(Outstate ~., data = College, nvmax = length(names(College)) - 1,
                      method = "forward")
co <- coef(reg.fwd, id = 11)
