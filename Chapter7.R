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
ar + s(age, df=5) + education, family = "binomial",
                data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se=TRUE, col = "green")
