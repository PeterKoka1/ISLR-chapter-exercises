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
