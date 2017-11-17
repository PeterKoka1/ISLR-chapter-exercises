library(MASS)
library(randomForest)
attach(Boston)
set.seed(123)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
B.train <- Boston[train, ]
B.test <- Boston[-train, ]

m.tree.vals <- c(sqrt(ncol(Boston)), ncol(Boston) - 1, ncol(Boston) / 2)
n.tree.vals <- c(300, 400, 500)
colors <- c("blue","red","green")
par(mfrow=c(1,3))
for (i in 1:3) {
  n.tree <- n.tree.vals[i]
  rforest <- randomForest(medv ~., data = Boston, subset = train, mtry = m.tree.vals[1], ntree = n.tree)
  mse.rf <- rforest$mse
  plot(1:n.tree, mse.rf, col = colors[1], type = "l", xlab = sprintf("Size %d forest", n.tree), ylab = "Test MSE",
       ylim = c(10,35))
  rforest2 <- randomForest(medv ~., data = Boston, subset = train,
                           mtry = m.tree.vals[2], ntree = n.tree)
  rforest3 <- randomForest(medv ~., data = Boston, subset = train,
                           mtry = m.tree.vals[3], ntree = n.tree)
  mse.rf2 <- rforest2$mse
  mse.rf3 <- rforest3$mse
  lines(1:n.tree, mse.rf2, col = colors[2], type = "l",
        xlab = sprintf("Size %d forest", n.tree), ylab = "Test MSE")
  lines(1:n.tree, mse.rf3, col = colors[3], type = "l",
        xlab = sprintf("Size %d forest", n.tree), ylab = "Test MSE")
  legend("topright", legend=c("m = sqrt(p)", "m = p-1", "m = p/2"), col=c("blue","red","green"), 
         lty=c(1,1,1), lwd=c(3,2), seg.len = 2, cex=0.8)
}

library(ISLR)
library(tree)
set.seed(123)
attach(Carseats)
train <- sample(nrow(Carseats), nrow(Carseats) / 2)
Car.train <- Carseats[train, ]
Car.test <- Carseats[-train, ]
par(mfrow=c(1,1))
Car.tree <- tree(Sales ~., data = Car.train)
plot(Car.tree);text(Car.tree, pretty = 0)
tree.preds <- predict(Car.tree, newdata = Car.test)
mean((tree.preds - Car.test$Sales)^2) # 4.427724 MSE

cv.Carseats <- cv.tree(Car.tree)
plot(cv.Carseats$size, cv.Carseats$dev, type = "l", col = "darkblue")
points(which.min(cv.Carseats$dev), cv.Carseats$dev[which.min(cv.Carseats$dev)],
       col = "black", pch = 20, cex = 1.4) # 8
prune.Carseats <- prune.tree(Car.tree, best = 8)
prune.pred <- predict(prune.Carseats, newdata = Car.test)
mean((prune.pred - Car.test$Sales)^2) # 5.314472 MSE. This is an increase in MSE

length(names(Carseats))
bag.Carseats <- randomForest(Sales ~., data = Car.train, mtry = ncol(Carseats) - 1,
                             ntree = 500, importance = TRUE)
yhat.bag <- predict(bag.Carseats, newdata = Car.test)
mean((yhat.bag - Car.test$Sales)^2) # 2.4796 MSE. Decrease with bagged model
importance(bag.Carseats)
# Price, ShelveLoc, then Age are most important predictors

MSE.tests <- rep(NA, 10)
for (i in 1:length(MSE.tests)) {
  Carseats.forest <- randomForest(Sales ~., data = Car.train, mtry = i,
                                  ntree = 500, importance = TRUE)
  yhat.forest <- predict(Carseats.forest, newdata = Car.test)
  mse.i <- mean((yhat.forest - Car.test$Sales)^2)
  MSE.tests[i] = mse.i
}
which.min(MSE.tests) # minimum test MSE is with 10 preds

attach(OJ)
train <- sample(nrow(OJ), 800)
OJ.train <- OJ[train, ]
OJ.test <- OJ[-train, ]

OJ.tree <- tree(Purchase ~., data = OJ.train)
summary(OJ.tree) # num.terminal nodes is 9 and training error rate of 16.25%
plot(OJ.tree);text(OJ.tree, pretty = 0)
OJ.tree # terminal node 15. Split criterion is PriceDiff > 0.31, number of observations 
# is 93, deviance is 0, 100% are classified as CH

tree.preds <- predict(OJ.tree, newdata = OJ.test, type = "class")
table(tree.preds, OJ.test$Purchase)
1 - mean(tree.preds == OJ.test$Purchase) # 17.78% error rate
set.seed(123)
cv.OJ <- cv.tree(OJ.tree, FUN = prune.misclass)
plot(cv.OJ$size, cv.OJ$dev, type = "l", col = "darkblue", xlab = "Number of Predictors",
     ylab = "Deviance")
# seems like strict cut-off at 2 in terms of decrease in deviance
prune.OJ <- prune.tree(OJ.tree, best = 2)
plot(prune.OJ); text(prune.OJ, pretty = 0)
summary(prune.OJ) # 19.38%
summary(OJ.tree) # 16.25%
yhat <- predict(prune.OJ, newdata = OJ.test, type = "class")
1 - mean((yhat == OJ.test$Purchase)^2) # 18.148% error rate

attach(Hitters)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)
train <- Hitters[1:200, ]
test <- Hitters[201:nrow(Hitters), ]

library(gbm)
set.seed(123)
lambda <- seq(from = 0.01, to = 0.5, by = 0.01)
error.train <- rep(NA, length(lambda))
for (i in 1:length(lambda)) {
  shrink <- lambda[i]
  boost.Hitters <- gbm(Salary ~., data = train, distribution = "gaussian",
                       n.trees = 1000, interaction.depth = 4, shrinkage = shrink)
  preds <- predict(boost.Hitters, data = train, n.trees = 1000)
  train.mse <- mean((preds - train$Salary)^2)
  error.train[i] = train.mse
}
par(mfrow=c(1,1))
plot(lambda, error.train, type = "b", col = "darkblue")
error.test <- rep(NA, length(lambda))
for (i in 1:length(lambda)) {
  shrink <- lambda[i]
  boost.Hitters <- gbm(Salary ~., data = train, distribution = "gaussian",
                       n.trees = 1000, interaction.depth = 4, shrinkage = shrink)
  preds <- predict(boost.Hitters, newdata = test, n.trees = 1000)
  test.mse <- mean((preds - test$Salary)^2)
  error.test[i] = test.mse
}
par(mfrow=c(1,1))
plot(lambda, error.test, type = "b", col = "darkblue")
error.test[which.min(error.test)] # 0.265 MSE
lambda[which.min(error.test)] # 0.04 lambda value


lm.fit <- lm(Salary ~., data = train)
lm.preds <- predict(lm.fit, newdata = test)
mean((lm.preds - test$Salary)^2) # 0.4917

boost.Hitters.optimal <- gbm(Salary ~., data = Hitters, distribution = "gaussian",
                             n.trees = 1000, interaction.depth = 4,
                             shrinkage = lambda[which.min(error.test)])
summary(boost.Hitters.optimal) # CAtBat is most important variable

set.seed(123)
bag.Hitters <- randomForest(Salary ~., data = train, mtry = ncol(train) - 1,
                            ntree = 1000)
yhat.bag <- predict(bag.Hitters, newdata = test)
mean((yhat.bag - test$Salary)^2) # 0.2311 MSE 
