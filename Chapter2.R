###: COLLEGE DATASET

df <- read.csv('College.csv')
fix(df)
###: creating an index-like label for each university so we don't treat as data
rownames(df) <- df[,1]
###: deleting the university titles
df <- df[,-1]

A <- df[,1:10]
pairs(A)
plot(df$Outstate,df$Private)

###: Creating 'Elite' qual variable dividing universities into 2 groups based on whether
###: or not the proportion of students coming from the top 10% of their highschool classes
###: exceeds 50%.

Elite <- rep("No",nrow(df)) # rep is for 'replicate' and will just print out "No" len(df) times
Elite[df$Top10perc>50]="Yes"
Elite <- as.factor(Elite) # creating as qualitative variables
df = data.frame(df,Elite) # appending Elite to our df

###: deleting extra irrelevant columns
drops <- c(df$var21,df$var22,df$var23)
df <- df[, !(names(df) %in% drops)]
fix(df)

###: AUTO DATASET

df2 <- read.csv('Auto.csv',header=TRUE,na.strings="?")
df2 <- na.omit(df2)

###: finding quantitative predictors, and qualitative predictors
str(df2) # only 'name' is qualitative

###: range of quantitative predictors
quant_var_df2 <- (df2[, !(names(df2)=='name')])
df_names_quant <- colnames(quant_var_df2)

for (quant in df_names_quant){
  print(quant)
}
sapply(quant_var_df2, range)

###: mean and standard deviation of each quantitative predictors
sapply(df2[, -c(9)], mean)
sapply(df2[, -c(9)], sd)

###: remove the 10th through 85th observations. What is the range, mean, sd of each predictor
df2_sample <- df2[-c(10:85),]
sapply(df2_sample[, -c(9)], mean)
sapply(df2_sample[, -c(9)], sd)

###: finding relationships
df2$year <- as.factor(df2$year)
df2$origin <- as.factor(df2$origin)
df2$cylinders <- as.factor(df2$cylinders)

plot(quant_var_df2$horsepower,quant_var_df2$weight, 
     xlab = 'Horsepower', ylab = 'Weight', main = 'Horsepower vs. Weight')
cor(df2$horsepower,df2$weight) 
# 0.8645377 correlation
# weight and horsepower are highly positively correlated 
# interesting because I would think increase in HP would come from lighter vehicle

plot(quant_var_df2$horsepower, quant_var_df2$acceleration,
     xlab = 'Horsepower', ylab = 'Acceleration', main='Horsepower vs. Acceleration')
cor(df2$horsepower, df2$acceleration)
# -0.6891955
# horsepower and acceleration are inversely correlated
# also interesting find

plot(df2$year,df2$mpg)
# the later the years, the better MPG we tend to have

###: BOSTON DATASET

library(MASS)
?Boston
Boston$chas <- as.factor(Boston$chas)
###: number of rows and cols
nrow(Boston) # 506
ncol(Boston) # 14
###: pairwise scatterplots
pairs(Boston)

par(mfrow = c(2,2))
plot(Boston$lstat, Boston$crim, main = 'Lower Status of Pop. vs. Crime Rate')
plot(Boston$dis,Boston$crim, main = 'Distance to Employment Centres vs. Crime Rate')
plot(Boston$tax,Boston$crim, main = 'Property Tax Rate vs. Crime Rate')
plot(Boston$nox,Boston$crim, main = 'Nitrogen Oxide Concentration vs. Crime Rate')

par(mfrow = c(1,1))
hist(Boston$crim,breaks = 50)
crime_rate = rep("No",nrow(Boston))
crime_rate[Boston$crim>20]="Yes"
ratio_yes <- table(crime_rate)
ratio <- ratio_yes[names(ratio_yes)=='Yes']

ratio_crimes = length(ratio) / length(crime_rate)
print(ratio_crimes)
# OR
ratio_crimes2 = nrow(Boston[Boston$crim > 20,])
print(ratio_crimes)
###: less than 0.002% of subburbs have per capita crime rate > 20

###: How many suburbs average more than 7 rooms per dwelling?
seven_rooms = nrow(Boston[Boston$crim > 7,])
# 85 suburbs
eight_rooms = nrow(Boston[Boston$crim > 8,])
# 76 suburbsws
