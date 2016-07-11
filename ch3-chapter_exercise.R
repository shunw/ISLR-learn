# introduction
# prediction function for lm with prediction/ confidence level
# linear regression with interaction *
# add curve in on the scatter plots. 
# vif function to check the collinearity
# function to get the confidence interval of coef
# in the lm function, a:b is the interaction a with b; a*b is a, b, a:b
# in the lm function, I(a^2) means add a^2 as a predictor
# higher poly predict could use poly()
# anova is to compare two models

#quantity data
ad_file = read.csv('Advertising.csv', header = TRUE, sep = ',')
ad_file = ad_file[, -1]

ad.lm.fit = lm(Sales ~ TV + Radio, data = ad_file)
predict(ad.lm.fit, data.frame(TV = 100, Radio = 20), interval = "confidence")

# interaction with two factors
ad.lm.fit1 = lm(Sales ~ TV * Radio, data = ad_file)
summary(ad.lm.fit1)

#quality data
credit_file = read.csv('Credit.csv', header = TRUE, sep = ',')
credit_file = credit_file[, -1]

summary(regsubsets(Balance ~ ., data = credit_file))
# two level of category
credict.lm.gender = lm(Balance ~ Gender, data = credit_file)
summary(credict.lm.gender)

# more than two level of the category
credit.lm.Eth = lm(Balance ~ Ethnicity, data = credit_file)
summary(credit.lm.Eth)

# interaction between income and sudent
credit.lm.std_income = lm(Balance ~ Student * Income, data = credit_file)
summary(credit.lm.std_income)

# try the vif
library(car)
credit.lm.ARL = lm(Balance ~ Age + Rating + Limit, data = credit_file)
vif(credit.lm.ARL)
summary(credit.lm.ARL)

credit.lm.AR = lm(Balance ~ Age + Rating, data = credit_file)
vif(credit.lm.AR)
summary(credit.lm.AR)
# polynomial regression
par(mar=c(5.1,4.1,.5,2.1))
par(mfrow = c(1, 1))
# this is for the linear regression
plot(mpg~horsepower, data=Auto)
Auto.lm0 = lm(mpg ~ horsepower, data = Auto)
abline(Auto.lm0, col = 'blue')
Auto.lm0.fitted = predict(Auto.lm0, data.frame(horsepower=Auto$horsepower))
plot(Auto.lm0.fitted, residuals(Auto.lm0))

# this is the poly ^2
Auto.lm = lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto)
summary(Auto.lm)
curve(56.9 -.46619*x + .0012305*x^2, add=T, col="red")

Auto.lm.fitted = predict(Auto.lm, data.frame(horsepower = Auto$horsepower))
plot(Auto.lm.fitted, residuals(Auto.lm))
#this is for the ploy^3
Auto.lm3 = lm(mpg ~ horsepower + I(horsepower ^ 2) + I(horsepower ^3), data = Auto)
summary(Auto.lm3)
curve(60.68 -.5689*x + .002079*x^2 - .000002147*x^3, add=T, col="orange")
#this is for the ploy^5
Auto.lm5 = lm(mpg ~ horsepower + I(horsepower ^ 2) + I(horsepower ^3) + I(horsepower ^4) + I(horsepower ^5), data = Auto)
curve(coef(Auto.lm5)[1] + coef(Auto.lm5)[2]*x + coef(Auto.lm5)[3]*x^2 
      + coef(Auto.lm5)[4]*x^3 + coef(Auto.lm5)[5]*x^4 + coef(Auto.lm5)[6]*x^5, 
      add=T, col="green")

# 3.4 questions
# 1. Is there a relationship between advertising sales and budget?
# this is to use the F value and the p value of F
ad.lm_all = lm(Sales ~ ., data = ad_file)
summary(ad.lm_all)

# 2. How strong is the relationship?
# use the RSE(residual standard deviation) and R square to explain the accuracy of the prediction and also how many % could be explained by these predictors.
summary(ad.lm_all)

#3. Which media contribute to sales?
# want to use the lm function and check the p value, but based on this how could the interaction be found
# the answer is same as my thought

# 4. How large is the effect of each medium on sales?
# this is to check the coef of each medium
# the ans is part same as mine. first is to check the confidence interval of coef, next to check if any collinearity
confint(ad.lm_all, level = .95)
confint(ad.lm_all, "TV", level = .95)

vif(ad.lm_all)

#5. How accurately can we predict future sales?
# guess this is to check the residuals or use cross validation
# the ans seems not relate with mine. 

#6. Is the relationship linear?
# Is there any function to check the linear
# yes, use residual plot, if the there is no pattern, the relationship should be linear, if there is some curve, the relationship may be not linear. 
plot(ad.lm_all)


#7. Is there synergy among the advertising media?
# still not sure how to find the interaction between the predictors. ???ans seems not clearly explain this. 

# lab########
#3.6.2

# analysis Bonston data
## following is to find the key elements. 
lm.boston.fit = lm(medv ~ ., data = Boston)
summary(lm.boston.fit)
library(MASS)
reg.boston = regsubsets(medv ~ ., data = Boston)
summary(reg.boston)

## to find the confince level for each coef and for the predict value
lm.boston.fit1 = lm(medv ~ lstat, data = Boston)
confint(lm.boston.fit1)
predict(lm.boston.fit1, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.boston.fit1, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

## plot the fit
plot(Boston$lstat, Boston$medv)
abline(coef(lm.boston.fit1)) # abline(a, b) a is the intercept, b is the slope
abline(lm.boston.fit1, col = "red") # this is from the ans of the book

plot(1:20, 1:20, pch = 1:20)

plot(predict(lm.boston.fit1), rstudent(lm.boston.fit1))
# the plot is same as the first plot of the plot(lm)
# residuals vs the fitted
plot(predict(lm.boston.fit1), residuals(lm.boston.fit1))
plot(lm.boston.fit1)

# hat value should be related with the predictor/x value. 
plot(hatvalues(lm.boston.fit1))
which.max(hatvalues(lm.boston.fit1))

# 3.6.3
# for multiple regression
lm.boston.fit22 = lm(medv ~ lstat + rm, data = Boston)
lm.boston.fit20 = lm(medv ~ lstat + age, data = Boston)
summary(lm.boston.fit20)
summary(lm.boston.fit22)
summary(lm.boston.fit20)$sigma # this is RSE
summary(lm.boston.fit20)$r.sq # this is r squared
vif(lm.boston.fit22)

# 3.6. 5
# non-linear
lm.boston.fit_multi = lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm.boston.fit_multi)
anova(lm.boston.fit_multi, lm.boston.fit1)
plot(predict(lm.boston.fit_multi), residuals(lm.boston.fit_multi))

lm.boston.fit_m3 = lm(medv ~ lstat + I(lstat ^ 2) + I(lstat ^3), data = Boston)
lm.boston.fit_m3 = lm(medv ~ poly(lstat, 3), data = Boston)
summary(lm.boston.fit_m3)
anova(lm.boston.fit1, lm.boston.fit_multi, lm.boston.fit_m3)

plot(predict(lm.boston.fit_m3), residuals(lm.boston.fit_m3))

# try some transform here
lm.boston.fit_t1 = lm(medv ~ log(rm), data = Boston)
lm.boston.fit_t2 = lm(medv ~ rm, data = Boston)
plot(Boston$rm, Boston$medv)
plot(log(Boston$rm), Boston$medv)

# 3.6.6 
head(Carseats)
reg.carseats = regsubsets(Sales ~ ., data = Carseats)
summary(reg.carseats)

lm.carseats = lm(Sales ~ ShelveLoc + Price, data = Carseats)
summary(lm.carseats)

lm.carseats.fit = lm(Sales ~ .+Income:Advertising + Price:Age, data = Carseats)
summary(lm.carseats.fit)
