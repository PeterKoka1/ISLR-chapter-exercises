library(ISLR)
set.seed(1)
glm.fit <- glm(Default$default ~ Default$income + Default$balance, data=Default,
               family="binomial")
summary(glm.fit)
glm.probs <- predict(glm.fit, data=Default, type="response")
glm.preds <- rep("No",dim(Default)[1])
glm.preds[glm.probs>0.5] <- "Yes"
mean(glm.preds!=Default$default) #2.63% error rate

attach(Default)
train <- sample(dim(Default)[1],dim(Default)[1]*(1/2))
Default.log.reg <- glm(default~income+balance, data=Default, family="binomial",
                       subset=train)
summary(Default.log.reg)
Default.probs <- predict(Default.log.reg, newdata=Default[-train,], type="response")
Default.preds <- rep("No", length(Default.probs))
Default.preds[Default.probs>.5] = "Yes"
mean(Default.preds != Default[-train,]$default)
# trial 1: 2.36% error rate
# trial 2: 2.38% error rate
# trial 3: 2.82% error rate
# Error rates vary in each test/train split

Default.log.reg <- glm(default~income+balance+student, data=Default, family="binomial",
                       subset=train)
Default.probs <- predict(Default.log.reg, newdata=Default[-train,], type="response")
Default.preds <- rep("No",length(Default.probs))
Default.preds[Default.probs>.5] = "Yes"
mean(Default.preds != Default[-train, ]$default)
# 2.82% error rate
# no noticable reduction in error rate from addition of student in model

set.seed(1)
Default.log.reg <- glm(default~income+balance, data=Default, family="binomial")
coef(summary(Default.log.reg))[,2]
#  (Intercept)       income      balance 
# 4.347564e-01 4.985167e-06 2.273731e-04 

boot.fn <- function(Default, index) {
  return(coef(glm(default~income+balance, data=default, 
                          subset=index, family="binomial")))
}
boot.fn(Default, (1:dim(Default)[1]))
boot.fn(Default, sample(dim(Default)[1], dim(Default)[1], replace=TRUE))

library(boot)
boot(Default, boot.fn, R=1000)
#  (Intercept)        income       balance
# 4.250851e-01  4.597920e-06  2.271145e-04
# nearly identical

library(ISLR)
set.seed(1)
attach(Weekly)
Weekly.fit <- glm(Direction~Lag1+Lag2, data=Weekly, family="binomial")
Weekly.fit <- glm(Direction~Lag1+Lag2, data=Weekly[-1,], family="binomial")
Weekly.probs <- predict(Weekly.fit, newdata=Weekly[1, ], type="response")
length(Weekly.probs)
Weekly.preds <- "No"
Weekly.preds[Weekly.probs>.5] <- "Yes"
mean(Weekly.preds == Weekly[1,]$Direction) # wrong classification

###: Computing LOOCV manually
errors <- rep(1, dim(Weekly)[1])
n <- dim(Weekly)[1]
for (i in 1:n) {
  glm.fit <- glm(Direction~Lag1+Lag2, data=Weekly[-i, ], family="binomial")
  glm.prob <- predict(glm.fit, newdata=Weekly[i, ], type="response")
  glm.pred <- "Down"
  glm.pred[glm.prob > .5] <- "Up"
  if (glm.pred == Weekly[i, ]$Direction){
    errors[i] <- 0
  }
}
CV.n <- (1 / n) * sum(errors)
CV.n # ~ 45% error rate

set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100) # n = 100, p = 2
# Y = X - 2X^2 + error
plot(x, y) # curviature in relationship between x and y

loocv.list <- rep(0, 4)
set.seed(1)
Data <- data.frame(x,y)
glm.fit1 <- glm(y ~ x, data=Data)
glm.fit2 <- glm(y ~ poly(x, 2), data=Data)
glm.fit3 <- glm(y ~ poly(x, 3), data=Data)
glm.fit4 <- glm(y ~ poly(x, 4), data=Data)
loocv.list[1] <- cv.glm(data=Data, glmfit=glm.fit1)$delta[1]
loocv.list[2] <- cv.glm(data=Data, glmfit=glm.fit2)$delta[1]
loocv.list[3] <- cv.glm(data=Data, glmfit=glm.fit3)$delta[1]
loocv.list[4] <- cv.glm(data=Data, glmfit=glm.fit4)$delta[1]
loocv.list # 2rd order polynomial fit seems to provide best results

set.seed(2)
loocv.list <- rep(0, 4)
Data <- data.frame(x,y)
glm.fit1 <- glm(y ~ x, data=Data)
glm.fit2 <- glm(y ~ poly(x, 2), data=Data)
glm.fit3 <- glm(y ~ poly(x, 3), data=Data)
glm.fit4 <- glm(y ~ poly(x, 4), data=Data)
loocv.list[1] <- cv.glm(data=Data, glmfit=glm.fit1)$delta[1]
loocv.list[2] <- cv.glm(data=Data, glmfit=glm.fit2)$delta[1]
loocv.list[3] <- cv.glm(data=Data, glmfit=glm.fit3)$delta[1]
loocv.list[4] <- cv.glm(data=Data, glmfit=glm.fit4)$delta[1]
loocv.list # 2nd order polynomial fit seems to provide best results again
# LOOCV iterates through ALL observations, so results make sense to be identical
# the quadratic fit had the smallest error which makes sense 
summary(glm.fit4) # poly(x, 4)2 had significance of 0 as expected due to shape of scatter

library(MASS)
attach(Boston)
n <- length(Boston$medv)
mu.medv <- (1 / n) * sum(Boston$medv)
mu.medv
std.err.medv <- sd(medv) / sqrt(n)
boot.fn <- function(data, index) {
  return(mean(data[index]))
}
boot(medv, boot.fn, R=1000)
std.err.medv
# 0.4005969 (Boot) vs. 0.4088611 (manual computation)
conf.int.low <- (mu.medv - (2 * std.err.medv)) 
conf.int.high <- (mu.medv + (2 * std.err.medv)) 
conf.int <- c(conf.int.low, conf.int.high) # 21.71508  23.23053
t.test(Boston$medv) # 21.72953 to 23.33608

med.medv <- median(medv) # 21.2
boot.fn <- function(data, index) {
  return(median(data[index]))
}
boot(medv, boot.fn, R=1000) # std.error of 0.3797
?quantile

tenth.tile <- quantile(medv, probs=.10) # 12.75
boot.fn <- function(data, index) {
  return(quantile(data[index], probs=.10))
}
boot(medv, boot.fn, R=1000) # std.error of 0.4945
