# 4.1 overview
setwd("/Users/zhang/Downloads/statsLearning_standford")
library(ISLR)
default_yes = Default[which(Default$default == "Yes"), ]
default_no = Default[which(Default$default == "No"), ]
plot(default_yes$balance, default_yes$income, xlab = "Balance", ylab = "Income", col = "red", pch = "+", xlim = c(min(Default$balance)-100, max(Default$balance) + 100), ylim = c(min(Default$income)-100, max(Default$income) + 100))
points(default_no$balance, default_no$income, pch = "<", col = "blue")

boxplot(balance ~ default, data = Default)
boxplot(income ~ default, data = Default)

### simple conclusion.
### default yes based on higher balance, not with income. 
### is this correct? 

#4.3.2
lm.default_balance = glm(default ~ balance, data = Default, family = binomial)
summary(lm.default)$coef

#4.3.3
predict(lm.default_balance, data.frame(balance = 1000), type = "response")

lm.default_stu = glm(default ~ student, data = Default, family = "binomial")
summary(lm.default_stu)$coef
predict(lm.default_stu, data.frame(student = "Yes"), type = "response")
predict(lm.default_stu, data.frame(student = "No"), type = "response")

#4.3.4
#prediction is different with the manual calculation. 
lm.default_all = glm(default ~ student + balance + I(income / 1000), data = Default, family = binomial)

intercept = summary(lm.default_all)$coef["(Intercept)", "Estimate"]
studentYes = summary(lm.default_all)$coef["studentYes", "Estimate"]
balance = summary(lm.default_all)$coef["balance", "Estimate"]
income = summary(lm.default_all)$coef["I(income/1000)", "Estimate"]

predict(lm.default_all, data.frame (balance = 1500, income = 40, student = "Yes"), type = "response")
fun_exp_yes = intercept + balance * 1500 + income * 40 + studentYes
p_x = exp(fun_exp_yes)/(1+exp(fun_exp_yes))
p_x


predict(lm.default_all, data.frame (balance = 1500, income = 40, student = "No"), type = "response")
fun_exp_no = intercept + balance * 1500 + income * 40
p_x_no = exp(fun_exp_no)/(1+exp(fun_exp_no))
p_x_no

# 4.4.3
#with default threshold
lda.fit.default = lda(default ~ balance + student, data = Default)
default.input = Default[, c(-1, -4)]
pred.default = predict(lda.fit.default, default.input)
table(pred.default$class, Default$default)

#changed the threshold
default_pos = Default
default_pos$No = pred.default$posterior[, 1]
default_pos$Yes = pred.default$posterior[, 2]
default_pos$pred = rep(NA, nrow(default_pos))
default_pos[which(default_pos$Yes > .2), "pred"] = "Yes"
default_pos[which(default_pos$Yes <= .2), "pred"] = "No"
table(default_pos$pred, default_pos$default)


#4.6 lab
library("ISLR")
head(Smarket)
cor(Smarket[, -9])
plot(Smarket$Volume, Smarket$Year)

names(Smarket)
library("MASS")
lm.smarket = lda(Direction ~ ., data = Smarket)
lm.smarket.q = qda(Direction ~., data = Smarket)
summary(lm.smarket.q)


#prepare to check the regression is good enough. --- make the seperation training dataset and the test data set
dim(Smarket)
TF_index = sample(c(TRUE, FALSE), nrow(Smarket), replace = TRUE)
smarket_train = Smarket[TF_index, ]
smarket_test = Smarket[!TF_index, ]

#with the lda method
lm.smarket.lda = lda(Direction ~., data = smarket_train)
pre.smarket.lda = predict(lm.smarket.lda, data.frame(smarket_test[,-9]))
#left parameter is the left vertical, right parameter is the horizontal.
table(pre.smarket.lda$class, smarket_test$Direction)

#with the qda method
lm.smarket.qda = qda(Direction ~., data = smarket_train)
pre.smarket.qda = predict(lm.smarket.qda, data.frame(smarket_test[, -9]))
table(pre.smarket.qda$class, smarket_test$Direction)

#with the logistic regression
lm.smarket.log = glm(Direction ~ ., data = smarket_train, family = "binomial")
summary(lm.smarket.log)
pre.smarkt.log = predict(lm.smarket.log, data.frame(smarket_test[, -9]), type = "response")
pre.smarkt.log_1 = rep("Down", length(pre.smarkt.log))
pre.smarkt.log_1[which(pre.smarkt.log>0.5)] = "Up"
table(pre.smarkt.log_1, smarket_test$Direction)
contrasts(smarket_train$Direction)

mean(pre.smarkt.log_1 == smarket_test$Direction)
head(Smarket)

## just use the Lag1 and Lag2
lm.smarket.log12 = glm(Direction ~ Lag1 + Lag2, data = smarket_train, family = "binomial")
summary(lm.smarket.log12)
pre.smarket.log12 = predict(lm.smarket.log12, newdata = data.frame(Lag1 = smarket_test[, 2], Lag2 = smarket_test[, 3]), type = "response")
pre.smarket.log12_1 = rep("Down", length(pre.smarket.log12))
pre.smarket.log12_1[which(pre.smarket.log12 > .5)] = "Up"
table(pre.smarket.log12_1, smarket_test$Direction)
mean(pre.smarket.log12_1 == smarket_test$Direction)

#with the Linear Discriminant Analysis
lm.smarket.lda12 = lda(Direction ~ Lag1 + Lag2, data = smarket_train)
lm.smarket.lda12
pre.smarket.lda12 = predict(lm.smarket.lda12, newdata = data.frame(Lag1 = smarket_test[, 2], Lag2 = smarket_test[, 3]), type = "response")
table(pre.smarket.lda12$class, smarket_test$Direction)
mean(pre.smarket.lda12$class == smarket_test$Direction)
lm.smarket.lda12
plot(lm.smarket.lda12)

pre.smarket.lda12$posterior[1:20, 1]
pre.smarket.lda12$class[1:20]

#with Quadratic Discriminant Analysis
lm.smarket.qda12 = qda(Direction ~ Lag1 + Lag2, data = smarket_train)
lm.smarket.qda12
pre.smarket.qda12 = predict(lm.smarket.qda12, smarket_test)
table(pre.smarket.qda12$class, smarket_test$Direction)
mean(pre.smarket.qda12$class == smarket_test$Direction)

#with KNN
train_book = (Smarket $ Year < 2005)
train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train_book, ]
test.X = cbind(Smarket$Lag1, Smarket$Lag2)[!train_book, ]
train.Direction = Smarket$Direction[train_book]

set.seed(1)
library(class)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

#caravan with knn
head(Caravan)
dim(Caravan)
summary(Caravan$Purchase)

##standarize the data by making the mean = 0 and the deviation = 1
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 2])

train.index = rep(FALSE, nrow(Caravan))
train.index[1:1000] = TRUE
train.X = standardized.X[train.index, ]
test.X = standardized.X[!train.index, ]

label.train.X = Caravan$Purchase[train.index]
label.test.X = Caravan$Purchase[!train.index]
set.seed(1)
pre.knn = knn(train.X, test.X, label.train.X, k = 1)
mean(pre.knn != "No")

pre.knn = knn(train.X, test.X, label.train.X, k = 3)
table(pre.knn, label.test.X)
25/(96+25)

## analysis with logstic regression. 
test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Caravan$Purchase[-test]
test.Y = Caravan$Purchase[test]

lm.caravan.glm = glm(Purchase ~ ., data = Caravan, family = "binomial", subset = -test)
probs.caravan.glm = predict(lm.caravan.glm, Caravan[test, ], type = "response")
length(test)
pre.glm = rep("No", 1000)
pre.glm[probs.caravan.glm > .5] = "Yes"
table(pre.glm, test.Y)

pre.glm[probs.caravan.glm > .25] = "Yes"
table(pre.glm, test.Y)
