set.seed(123)
X <- rnorm(100)
e <- rnorm(100)
B0 = 1.5
B1 = 0.7
B2 = 1.2
B3 = 0.9
y <- B0 + B1*X + B2 * X^2 + B3 * X^3 + e
data <- data.frame(y = y, x = X)
attach(data)
regfit.best <- regsubsets(y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7)
                           + I(X^8) + I(X^9) + I(X^10),
                           data = data, nvmax = 10)
reg.summary <- summary(regfit.best)
par(mfrow=c(2,2))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(3, reg.summary$cp[3], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(3, reg.summary$bic[3], col="red", cex=2, pch=20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Squared", type = "l")
which.max(reg.summary$adjr2)
points(7, reg.summary$adjr2[7], col="red", cex=2, pch=20)
plot(regfit.best, scaled = "adjr2")
# Cp best fit was using 3 variables, BIC also 3 variables, adjr2 was 7
# note that the change in r^2 using 3 variables and higher (until ~ 9) is minimal, so we can assume
# model with fewer will be sufficient
coef(regfit.best, 3)

regfit.fwd <- regsubsets(y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7)
                         + I(X^8) + I(X^9) + I(X^10),
                         data = data, nvmax = 10, method = "forward")
fwd.summary <- summary(regfit.fwd)
par(mfrow=c(2,2))
plot(fwd.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(fwd.summary$cp)
points(3, fwd.summary$cp[3], col="red", cex=2, pch=20)
plot(fwd.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(fwd.summary$bic)
points(3, fwd.summary$bic[3], col="red", cex=2, pch=20)
plot(fwd.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R Squared", type="l")
which.max(fwd.summary$adjr2)
points(4, fwd.summary$adjr2[4], col="red", cex=2, pch=20)
# best model is the 3-var model
coef(regfit.fwd, id=3)

regfit.bwk <- regsubsets(y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7)
                         + I(X^8) + I(X^9) + I(X^10),
                         data = data, nvmax = 10, method = "backward")

bwk.summary <- summary(regfit.bwk)
par(mfrow=c(2,2))
plot(bwk.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(bwk.summary$cp)
points(4, bwk.summary$cp[4], col="red", cex=2, pch=20)
plot(bwk.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(bwk.summary$bic)
points(4, bwk.summary$bic[4], col="red", cex=2, pch=20)
plot(fwd.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R Squared", type="l")
which.max(fwd.summary$adjr2)
points(4, fwd.summary$adjr2[4], col="red", cex=2, pch=20)
# best model is the 4-var model
coef(regfit.bwk, id=4)

library(glmnet)
x <- model.matrix(y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7)
                  + I(X^8) + I(X^9) + I(X^10), data = data)[, -1]
grid <- 10^seq(from = 10, to = -2, length = 100)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid, standardize = TRUE)
dim(coef(lasso.mod)) # 11 x 100
dev.off()
par(mfrow=c(1,1))
plot(lasso.mod)
set.seed(1)
cv.lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)
min.lambda <- cv.lasso$lambda.min
lasso <- glmnet(x, y, alpha = 1)
predict(lasso, s = min.lambda, type = "coefficients")[1:11,]
# X, X^2, X^3, X^4, and X^6 as variables (along with intercept)

B7 <- 0.2
y <- B0 + B7 * X^7 + e
data <- data.frame(x = X, y = y)
attach(data)
regfit.best <- regsubsets(y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7)
                          + I(X^8) + I(X^9) + I(X^10),
                          data = data, nvmax = 10)
reg.sum <- summary(regfit.best)
dev.off()
par(mfrow=c(2,2))
plot(reg.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.sum$cp) #1
points(1, reg.sum$cp[1], col = "red", cex = 2, pch = 20)
plot(reg.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.sum$bic)
points(1, reg.sum$bic[1], col = "red", cex = 2, pch = 20)
plot(reg.sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Squared", type = "l")
which.max(reg.sum$adjr2)
points(6, reg.sum$adjr2[6], col = "red", cex = 2, pch = 20)
# Cp picked the 1 var model, bic picked the 1 var model, adjR^2 picked the 4-var model
plot(regfit.best, scaled = "adjr2")
coef(regfit.best, id = 1)
coef(regfit.best, id = 4)

lasso.mod <- glmnet(x, y, lambda = grid, alpha = 1, standardize = TRUE)
par(mfrow=c(1,1))
plot(lasso.mod)
cv.lasso <- cv.glmnet(x, y, alpha = 1)
lambda.min <- cv.lasso$lambda.min
lasso.mod.opt <- glmnet(x, y, lambda = lambda.min, alpha = 1, standardize = TRUE)
predict(lasso.mod.opt, s = lambda.min, type = "coefficients")[1:11,]
# select model with X^2, X^5. and X^7

College
names(College)
dim(College)[1]
set.seed(1)
train <- sample(1:dim(College)[1], dim(College)[1] /2)
test <- -train
Col.train <- College[train, ]
Col.test <- College[test, ]
attach(College)
lm.fit <- lm(Apps ~ ., data = Col.train)
lm.pred <- predict(lm.fit, Col.test)
mean((lm.pred - Col.test$Apps)^2) # 1108531 MSE

x.train <- model.matrix(Apps ~., data = Col.train)
x.test <- model.matrix(Apps ~., data = Col.test)
grid <- 10 ^ seq(7, -4, length = 100)
ridge.mod <- glmnet(x.train, Col.train$Apps, alpha = 0, lambda = grid, 
                    standardize = TRUE, thresh = 1e-12)
ridge.cv <- cv.glmnet(x.train, Col.train$Apps, alpha = 0, lambda = grid,
                      thresh = 1e-12)
lambda.min <- ridge.cv$lambda.min
ridge.pred <- predict(ridge.mod, s = lambda.min, newx = x.test)
mean((ridge.pred - Col.test$Apps)^2) # 1108509 minimal reduction in test MSE

lasso.mod <- glmnet(x.train, Col.train$Apps, alpha = 1, lambda = grid, 
                    standardize = TRUE, thresh = 1e-12)
lasso.cv <- cv.glmnet(x.train, Col.train$Apps, alpha = 1, lambda = grid,
                      thresh = 1e-12)
lambda.min <- lasso.cv$lambda.min
lasso.pred <- predict(lasso.mod, s = lambda.min, newx = x.test)
mean((lasso.pred - Col.test$Apps)^2) # 1019367 slightly more reduction in test MSE
predict(lasso.mod, s = lambda.min, type = "coefficients")
# 17 preds used

library(pls)
pcr.fit <- pcr(Apps ~., data = Col.train, scale = TRUE,
               validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, newdata = Col.test, ncomp = 16)
mean((pcr.pred - Col.test$Apps)^2) # 1166897

pls.fit <- plsr(Apps ~., data = Col.train, scale = TRUE,
               validation = "CV")
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, newdata = Col.test, ncomp = 8)
mean((pls.pred - Col.test$Apps)^2) # 1132021

Apps.avg <- mean(Col.test$Apps)
1 - mean((lm.pred - Col.test$Apps)^2) / mean((Apps.avg - Col.test$Apps)^2)
1 - mean((ridge.pred - Col.test$Apps)^2) / mean((Apps.avg - Col.test$Apps)^2)
1 - mean((lasso.pred - Col.test$Apps)^2) / mean((Apps.avg - Col.test$Apps)^2)
1 - mean((pcr.pred - Col.test$Apps)^2) / mean((Apps.avg - Col.test$Apps)^2)
1 - mean((pls.pred - Col.test$Apps)^2) / mean((Apps.avg - Col.test$Apps)^2)
# all methods have near identical test R^2 with our basic least squares model with the highest

X <- matrix(data = rnorm(1000*20, mean = 0, sd = 1), nrow = 1000, ncol = 20)
e <- rnorm(1000)
B = rnorm(20)
B[2] = 0
B[7] = 0
B[11] = 0
B[13] = 0
B[14] = 0
B[20] = 0
y <- X %*% B + e
train <- sample(1:100, 100)
test <- -train
data <- data.frame(y = y, x = X)
data.train <- data[train, ]
data.test <- data[test, ]
attach(data.train)
regfit.best <- regsubsets(y ~., data = data.train, nvmax = 20)
regfit.sum <- summary(regfit.best)
par(mfrow=c(1,1))
plot(regfit.sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R Squared", type = "l")
which.max(regfit.sum$adjr2)
points(17, regfit.sum$adjr2[17], col = "red", cex = 2, pch = 20)
train.mat <- model.matrix(y ~., data = data.train, nvmax = 20)
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(regfit.best, id = i)
  pred = train.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((data.train$y - pred)^2)
}
val.errors
which.min(val.errors) # as expected, train MSE includes all variables
plot(val.errors, xlab = "Number of Variables", ylab = "Training MSE", type = "o")

attach(data.test)
test.mat <- model.matrix(y ~., data = data.test, nvmax = 20)
for (i in 1:20) {
  coefi <- coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((data.test$y - pred)^2)
}
which.min(val.errors) # 16 variable model
plot(val.errors, xlab = "Number of Variables", ylab = "Test MSE", type = "o")
# USE OF STANDARD ERROR RULE
sd(val.errors)
sdError <- sd(val.errors)^2 / length(val.errors) #0.5332
# we can use 14 variable model

attach(data)
regfit.best <- regsubsets(y ~., data = data, nvmax = 20)
coef(regfit.best, 14)
# coefficient B13 (although set to 0) was used, rest unused