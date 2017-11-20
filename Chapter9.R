library(e1071)
set.seed(1)
x <- matrix(rnorm(200)^2 + 1 * 2, ncol = 2)
split <- sample(100, 50)
X1 <- x[, 1]
X2 <- x[, 2]
X2[split] <- X2[split] + 2
X1[-split] <- X1[-split] - 2
plot(X1[split], X2[split], col = "darkblue", xlab = "Predictor X1", ylab = "Predictor X2",
     xlim = c(-1,11), ylim = c(1.5,14))
points(X1[-split], X2[-split], col = "red")
x <- cbind(X1, X2)
y <- rep(1, 100)
y[split] <- -1
dat <- data.frame(x = x, y = as.factor(y))
attach(dat)
train <- sample(100, 50)

svmfit <- svm(y ~., data = dat[train, ], kernel = "linear", cost = 10,
              scale = FALSE)
plot(svmfit, dat[train, ])
table(predict = predict(svmfit, dat[train, ]), truth = dat[train, ]$y)
# 3 errors on training data

svmfit2 <- svm(y ~., data = dat[train, ], kernel = "polynomial", d = 2,
               cost = 10, scale = FALSE)
plot(svmfit2, dat[train, ])
table(predict = predict(svmfit2, dat[train, ]), truth = dat[train, ]$y)
# 0 errors

svmfit3 <- svm(y ~., data = dat[train, ], kernel = "radial", cost = 10,
               gamma = 3, scale = FALSE)
plot(svmfit3, dat[train, ])
table(predict = predict(svmfit3, dat[train, ]), truth = dat[train, ]$y)
# 0 errors

table(predict = predict(svmfit, dat[-train, ]), truth = dat[-train, ]$y)
# 3 misclassified on test data for linear kernel
table(predict = predict(svmfit2, dat[-train, ]), truth = dat[-train, ]$y)
# 3 misclassified on test data for polynomial kernel
table(predict = predict(svmfit3, dat[-train, ]), truth = dat[-train, ]$y)
# 3 misclassified on test data for radial kernel

# Using non-linear transformation of the features with log reg
set.seed(123)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)
plot(x1, x2, col = (10-y), xlab = "X1", ylab = "X2")
dat <- data.frame(x1, x2, y);attach(dat)
log.fit <- glm(y ~., data = dat, family = "binomial")
summary(log.fit) 
probs <- predict(log.fit, data = dat, type = "response")
preds <- rep(0, length(y))
preds[probs > 0.5] = 1
plot(dat[preds == 1, "x1"], dat[preds == 1, "x2"], col = "darkblue")
points(dat[preds == 0, "x1"], dat[preds == 0, "x2"], col = "red")

log.fit <- glm(y ~ poly(x2, 3) + poly(x1, 2), data = dat, family = "binomial")
summary(log.fit)
probs <- predict(log.fit, data = dat, type = "response")
preds <- rep(0, length(y))
preds[probs > 0.5] = 1
plot(dat[preds == 1, "x1"], dat[preds == 1, "x2"], col = "darkblue")
points(dat[preds == 0, "x1"], dat[preds == 0, "x2"], col = "red")

svm.fit <- svm(as.factor(y) ~., data = dat, kernel = "linear", cost = 0.1)
preds <- predict(svm.fit, data = dat)
plot(svm.fit, dat) # all observations classified to one class

svm.rad <- svm(as.factor(y) ~., data = dat, kernel = "radial", gamma = 1, cost = 0.1)
preds <- predict(svm.rad, data = dat)
par(mfrow=c(1,1))
plot(svm.rad, dat)
plot(dat[preds == 1, "x1"], dat[preds == 1, "x2"], col = "darkblue")
points(dat[preds == 0, "x1"], dat[preds == 0, "x2"], col = "red")

library(ISLR)
rm(Auto)
mil <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpg_1 <- as.factor(mil)
attach(Auto)
tune.out <- tune(svm, mpg_1 ~., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000)))
summary(tune.out) # cost of 1 performed best
tune.out.rad <- tune(svm, mpg_1 ~., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 100), 
                                                                                gamma = c(0.1, 0.5, 1, 5)))
summary(tune.out.rad) # cost of 10 and gamma 0.1 performed best
tune.out.poly <- tune(svm, mpg_1 ~., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000),
                                                                                     d = c(2, 3, 4, 5)))
summary(tune.out.poly) # cost of 1000, degree of 2

svm.lin <- svm(mpg_1 ~., data = Auto, kernel = "linear", cost = 1)
svm.rad <- svm(mpg_1 ~., data = Auto, kernel = "radial", gamma = 1, cost = 1)
svm.poly <- svm(mpg_1 ~., data = Auto, kernel = "polynomial", cost = 1000, d = 3)

par(mfrow=c(1,1))
plot(svm.lin, data = Auto, mpg ~ displacement)
plot(svm.lin, data = Auto, mpg ~ cylinders)
plot(svm.lin, data = Auto, mpg ~ acceleration)

plot(svm.rad, data = Auto, mpg ~ displacement)
plot(svm.rad, data = Auto, mpg ~ cylinders)
plot(svm.rad, data = Auto, mpg ~ acceleration)

plot(svm.poly, data = Auto, mpg ~ displacement)
plot(svm.poly, data = Auto, mpg ~ cylinders)
plot(svm.poly, data = Auto, mpg ~ acceleration)
