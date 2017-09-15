################
## CHAPTER 3 ###
################

###: Auto dataset
library(ISLR)
data("Auto")
lm.fit <- lm(formula = mpg ~ horsepower, data=Auto)
attach(Auto)
summary(lm.fit)

###: testing for Ho : Bi = 0 for all i
###: p-value within *** (0) range, low enough to reject
###: R^2 val = 0.6049 indicating approx. 60% of variation in mpg is explained by horsepower

predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98), interval = "prediction") # wider to include errors

plot(horsepower, mpg)
abline(lm.fit, lwd=3, col="red")

###: heteroscedacity test
plot(predict(lm.fit),rstudent(lm.fit))
###: leverage test
plot(hatvalues(lm.fit))

# overall tests:
par(mfrow=c(2,2))
plot(lm.fit)

###: Residuals vs. fitted -> indicates trend in residuals (i.e. non-linearity in model)
###: Residuals vs. leverage -> indicates some high leverage points 

###: scatterplot matrix with all points

pairs(Auto)
cor(Auto[,-9])
lm.fit2 <- lm(formula = mpg ~., data = Auto[,-9])
summary(lm.fit2)

###: High f-stat, low p-value indicate relationship between predictors and response

###: Displacement, Weight, Year, and Origin have noticable relationship with mpg

###: R^2 of approx 0.82 indicates approx 82% of variablility is explained by predictors

###: The coefficient for the year suggests that with an increase in year 
###: follows an increase in mpg

par(mfrow=c(2,2))
plot(lm.fit2)

###: Residuals vs. Fitted -> indicates somewhat non-linear shape
###: Residuals vs. Leverage -> indicates a few very high leverage points
which.max(hatvalues(lm.fit2)) # point 14 has high leverage val (also seen on Res. vs. Levg. plot)

###: finding interaction effects
par(mfrow=c(1,1))
pairs(Auto) # displacement*weight, displacement*horsepower

summary(lm(formula = mpg ~ displacement*weight, data = Auto[,-9])) 
# high f-stat, low p-val
summary(lm(formula = mpg ~ displacement*horsepower, data = Auto[,-9])) 
# high f-stat, low p-val
summary(lm(formula = mpg ~ displacement*horsepower + displacement*weight, data = Auto[,-9]))
###: when predicted together, displacement:horsepower is significant, while displacement:weight is not

###: transformations: log(X), sqrt(X), X^2
plot(Auto[,-9])
lm.fit3 <- lm(formula = mpg ~ log(year) + sqrt(displacement) + I(weight^2), data = Auto[,-9])
lm.fit3_compare <- lm(formula = mpg ~ year + displacement + weight, data = Auto[,-9])
summary(lm.fit3)
summary(lm.fit3_compare)
###: comparison shows that transformations create more significant model 
###: displacement significance in linear model is approx = 1
###: displacement significance in transformed model is approx = 0

library(ISLR)
data("Carseats")
lm.fit <- lm(formula = Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)
?Carseats

###: Price -> 1 unit increase in Price results in decrease of 54.4459 units in Sales
###: UrbanYes *qualitative var* -> if store is in urban location, results in 21.19 decrease in Sales
###: USYes *qualitative var* -> if store is in US, results in 1200.573 unit increase in Sales

# Sales = 13.043469 - 0.054459 * Price - 0.0219 * UrbanYes + 1.200573 * USYes
# Reject Predictor at UrbanYes (near 1 P-val)

lm.fit2 <- lm(formula = Sales ~ Price + US, data = Carseats)
summary(lm.fit2)

###: f-stat is higher at 62.43 on fit without Urban, R^2 is only slightly higher @0.2354 vs 0.2335

confint(lm.fit2, level = 0.95)
plot(lm.fit)
###: Residuals vs. Leverage indicate some resids between (-2,2)

set.seed(1) # taking arb integer arg
x <- rnorm(100) # 100 random vals
y <- 2*x + rnorm(100) 
lm.fit3 <- lm(formula = y ~ x + 0)
summary(lm.fit3) 
# coeff: 1 unit increase in y results in 1.9939 increase in x
# Standard Error of coeff: 0.1065
# p-value: approx = 0
# f-stat: 350.7
# t-value = 18.73
lm.fit4 <- lm(formula = x ~ y + 0)
summary(lm.fit4)
# coeff: 1 unit increase in y results in 0.3911 increase in x
# Standard Error of coeff: 0.02089
# p-value: approx = 0
# f-stat: 350.7
# t-value = 18.73

###: proof t-stat

n <- length(x)
t.stat <- sqrt(n-1) * sum(x*y) / sqrt( sum(x^2) * sum(y^2) - (sum(x*y)^2))
as.numeric(t.stat) # = 18.7259

summary(lm(formula = y ~ x)) # t-val = 18.556
summary(lm(formula = x ~ y)) # t-val = 18.56

set.seed(1)
x <- 1:100
sum(x^2) # 338350
y <- 2*x + rnorm(100, sd = 0.1) 
sum(y^2) # 1353606

summary(lm(formula = y ~ x + 0))
summary(lm(formula = x ~ y + 0))

set.seed(1)
x <- rnorm(100) # feature X
eps <- rnorm(100, mean = 0, sd = sqrt(0.25))
y <- -1 + 0.5*x + eps # Bo = -1, B1 = 1/2
print(length(y)) # length = 100
par(mfrow=c(1,1))
plot(x,y)
###: shows linear positive trend with additional noise from error term (eps)
lm.fit5 <- lm(formula = y ~ x)
summary(lm.fit5)
###: comparison of pred(Bo) vs act(Bo) and pred(B1) vs act(B1)
###: -0.975 vs -1 and 0.55311 vs 0.50
abline(lm.fit5, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("Least square", "Regression"), col = c("blue","red"), lwd = c(2,2))
lm.fit6 <- lm(formula = y ~ x + I(x^2))
summary(lm.fit6) # worse fit (trivial with given scatter shape) even though R^2 slightly increases

eps <- rnorm(100, mean = 0, sd = sqrt(3))
y <- -1 + 0.5*x + eps
lm.fit7 <- lm(formula = y ~ x)
plot(x,y) # lesser degree of pattern (greater error term with eps)
confint(lm.fit7)

###: Collinearity

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100) # 2, 2, and 0.3
###: corr between x1 and x2
cor(x1,x2) # 0.8351212
plot(x1,x2)
lm.fit8 <- lm(formula = y ~ x1 + x2)
summary(lm.fit8)
###: results display we can reject Ho for B1 but cannot for Bo

lm.fit9 <- lm(formula = y ~ x1)
summary(lm.fit9)
###: with just x1, we can reject the null at 5% significance
lm.fit10 <- lm(formula = y ~ x2)
summary(lm.fit10)
###: we can also reject the null for x2 at 5% significance 

###: Does the model contradict itself? 
###: No. The importance of x2 was hidden due to the presence of collinearity 
###: between the x1 and x2 variable. Uncertainty due to broad range in coeff. estimates
###: causes SE(pred(Bj)) to grow and our t-stat to shrink, thereby reducing the power of 
###: our predictors. 

library(MASS)
attach(Boston)
?Boston

lm.zn <- lm(crim~zn)
lm.indus <- lm(crim~indus)

chas <- as.factor(chas)
lm.chas <- lm(crim~chas)

lm.nox <- lm(crim~nox)
lm.rm <- lm(crim~rm)
lm.age <- lm(crim~age)
lm.dis <- lm(crim~dis)
lm.rad <- lm(crim~rad)
lm.tax <- lm(crim~tax)
lm.ptratio <- lm(crim~ptratio)
lm.black <- lm(crim~black)
lm.lstat <- lm(crim~lstat)
lm.medv <- lm(crim~medv)
###: we can reject all null at 5% sig. except 'chas' variable

mlm.all <- lm(crim ~., data = Boston)
summary(mlm.all)
###: we can reject null for 'zn','dis','rad','black','medv'
names(Boston)
Boston.quant <- Boston[-c(1,4)]
cor(Boston.quant)
###: Multiple correlated variables indicate univar. regression may suggest relationship
###: between predictor and response as other predictors are ignored in this model.
###: Multiple lin reg shows change in pred while holding other predictors constant, revealing
###: more deductive underlying relationships
###: *** note: some preds. show multicollinearity which may make seperation of individual effects
###: ***       of variables difficult
fit.nox <- lm(crim~poly(nox, 3))
summary(fit.nox) # low p-val and large f-stat imply strong model with cubic fit
