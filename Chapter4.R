################
## CHAPTER 4 ###
################

library(ISLR)
summary(Weekly)
attach(Weekly)
cor(Weekly[,-9])
pairs(Weekly[-9])
library(ggplot2);library(GGally)
ggpairs(Weekly[,-9])
# no obvious patterns among predictors other than volume vs. year (R^2 ~ 0.842)
# ability to handle larger market orders, HFT, etc.
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Weekly,
               family = binomial)
summary(glm.fit)
summary(glm.fit)$coef[,4]
# the predictor Lag 2 seems to be statistically significant with a p-value of 0.0296
# with a positive coefficient, this suggests that if the market rose the last 2 days,
# it is likely that it will rise on the third day
par(mfrow=c(2,2))
plot(glm.fit)

glm.probs <- predict(glm.fit, Weekly, type="response")
glm.pred <- rep("Down",length(Volume))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction)

mean(glm.pred == Direction) # 56.106 overall accuracy
557 / (557 + 48) # predicts direction of Up correctly at 92.06% 
54 / (54 + 430) # predicts direction of Down correctly at 11.15%
# model predicts poorly when assessing negative returns but well for positive

train <- (Year < 2009)
test <- Weekly[(!train),]
test.Direction <- Direction[!train]
glm.fit <- glm(Direction ~ Lag2, data = Weekly, 
               family = binomial, subset = train)
glm.probs <- predict(glm.fit, test, type = "response")
glm.preds <- rep("Down", length(test.Direction))
glm.preds[glm.probs > 0.5] = "Up"
table(glm.preds, test.Direction)

mean(glm.preds == test.Direction) # 0.625 overall model accuracy
56 / (56 + 5) # correct predictions on test data - 91.80% for market increase
9 / (34 + 9) # correct predictions on test data - 20.93% for market decrease

par(mfrow=c(1,1))
plot(density(Lag2)) # density test shows predictor is approx Gaussian

library(MASS)
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
plot(lda.fit)
lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class

table(lda.class, test.Direction) 
mean(lda.class==test.Direction) # 0.625 overall model accuracy
56 / (56 + 5) # correct predictions on test data - 91.80% on market increase
9 / (9 + 34) # correct predictions on test data - 20.93% on market decrease

qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <- predict(qda.fit, test)$class
mean(qda.pred == test.Direction) # overall accuracy of 58.65%
table(qda.pred,test.Direction)

set.seed(123)
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.y <- Direction[train]
test.y <- Direction[!train]

knn.fit <- knn(train = train.X, test = test.X, cl = train.y, k=1)
table(knn.fit, test.y)
mean(knn.fit == test.y) # 50% accuracy
31 / (31 + 30) # 50.82% on market increase
21 / (21 + 22) # 48.83% on market decrease

# log reg and lda provide same accuracy

knn.fit <- knn(train = train.X, test = test.X, cl = train.y, k=15)
mean(knn.fit == test.y) # knn with k = 15 offered best results for knn with decreasing accuracy after 15

pairs(Weekly[,-9])
ggpairs(Weekly[-9])

plot(hatvalues(glm.fit))
which.max(hatvalues(glm.fit)) 

glm.fit_poly<-glm(Direction ~ Lag2 + I(Lag2^2), data = Weekly,
                  family = binomial, subset = train)
glm.fit_int<-glm(Direction ~ Lag2:Lag1, data = Weekly,
                  family = binomial, subset = train)
glm.probs <- predict(glm.fit_poly, test, type = "response")
glm.preds <- rep("Down",length(glm.probs))
glm.preds[glm.probs > 0.5] = "Up"
mean(glm.preds == test.Direction)
glm.probs <- predict(glm.fit_int, test, type = "response")
glm.preds <- rep("Down",length(glm.probs))
glm.preds[glm.probs > 0.5] = "Up"
mean(glm.preds == test.Direction)

?Auto
summary(Auto)
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] = 1
auto_df <- data.frame(Auto,mpg01)

names(auto_df)
ggpairs(auto_df[,-9])
# strongest cor for mpg01 is with cylinders, displacement, and weight
par(mfrow=c(1,1))
attach(auto_df)
boxplot(displacement ~ mpg01, data = auto_df, main = "MPG01 vs. Displacement")
boxplot(mpg ~ mpg01, data = auto_df, main = "MPG01 vs. MPG")
boxplot(cylinders ~ mpg01, data = auto_df, main = "MPG01 vs. Cylinders")
boxplot(weight ~ mpg01, data = auto_df, main = "MPG01 vs. Weight")
boxplot(horsepower ~ mpg01, data = auto_df, main = "MPG01 vs. Horsepower")
# focus on displacement, cylinders, horesepower, and weight

attach(auto_df)
train <- createDataPartition(mpg01, list=FALSE)
train <- (Year %% 2 == 0)
Auto.train <- auto_df[train, ]
Auto.test <- auto_df[-train, ]
mpg01.test <- mpg01[-train]
lda.fit <- lda(mpg01 ~ displacement + cylinders + horsepower + weight,
               data = auto_df, subset = train)

lda.pred <- predict(lda.fit, Auto.test)$class
mean(lda.pred != mpg01.test) # 9.6938% overall model error rate on test data
table(lda.pred,mpg01.test)

qda.fit <- qda(mpg01 ~ displacement + cylinders + horsepower + weight,
                data = auto_df, subset = train)
qda.pred <- predict(qda.fit, Auto.test)$class
mean(qda.pred != mpg01.test) # 8.163% overall model error rate on test data

glm.fit <- glm(mpg01 ~ displacement + cylinders + horsepower + weight,
               data = auto_df, subset = train, family = "binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit, Auto.test, type = "response")
glm.preds <- rep(0, length(glm.probs))
glm.preds[glm.probs > 0.5] = 1
mean(glm.preds != mpg01.test) # 10.71% error rate

train.X <- cbind(displacement,cylinders,horsepower,weight)[train,]
test.X <- cbind(displacement,cylinders,horsepower,weight)[-train,] 
train.y <- mpg01[train]
knn.fit <- knn(train.X, test.X, train.y, k = 3)
table(knn.fit,mpg01.test)
mean(knn.fit != mpg01.test) # 18.87% error rate for k = 3 
# k = 3 performs best

Power <- function() {
  print(2^3)
}
Power()

Power2 <- function(a,b) {
  print(a^b)
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3 <- function(a,b) {
  result <- a^b
  return(result)
}
Power3(4,5)

par(mfrow=c(1,1))
x_axis <- 1:10
plot(x_axis, Power3(x_axis, 2), log = "xy", 
     xlab = "Log of x", ylab = "Log of x squared", main = "Log of X Squared vs Log X")
PlotPower <- function(x, power) {
  plot(x, Power3(x, power), xlab = "X", ylab = "X Squared", main = "X vs. X to Some Power")
}
PlotPower(1:10, 3)