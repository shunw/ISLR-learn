### 6.5.1 Best Subset Selection
library(ISLR)
dim(Hitters)
summary(Hitters$Salary) #use summary function to check how the Field make of
Hitters = na.omit(Hitters) #use na.omit function to remove the missing values in varialble

library(leaps)
# regsubset, default is 8 subsets, but with nvmax, it could have more subset.
# seems under nvmax, it could have the Cp, BIC, etc data
# function: which.min/ which.max/ point
regfit.full = regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary = summary (regfit.full)
names(reg.summary)

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")
#which.max function is to get the index of the largest number in the vector
points(which.max(reg.summary$adjr2), reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# following plot is for cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "cp", type = "l")
cp.min_index = which.min(reg.summary$cp)
points(cp.min_index, reg.summary$cp[cp.min_index], col = "red", cex = 2, pch = 20)

# following plot is for bic
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "bic", type = "l")
bic.min_min = which.min(reg.summary$bic)
points(bic.min_min, reg.summary$bic[bic.min_min], col = "red", cex = 2, pch = 20)

# following chart is related to how to choose the predictors. 
par(mfrow = c(1, 1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")

# use coef function to check the coef
coef(regfit.full, 6)

### 6.5.2 Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

### 6.5.3 choosing among models using the validation set approach and cross-validation
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

regfit.best = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)
# this is to make a matrix from the data, but not sure if we don't make a matrix for it, what will happen
# the data in the test is not changed, except the category item change to number, and also Salary is moved out from this vectors. 
test.mat = model.matrix(Salary ~., data = Hitters[test, ])
coefi = coef(regfit.best, id = 3)

regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)


## since there is no PREDICT() method for regsubsets(), 
## we could make function for the above steps. 
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

val.errors = rep(NA, 19)
for (i in 1:19) {
  # this is to get the predictor i's coef
  coefi = coef(regfit.best, id = i)
  # this is to get the predict data(coef data * )
  pred = test.mat[, names(coefi)]%*%coefi
  # this is to make calculate the salary residuals avg
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
# the min error is get the best predictor number. 
which.min(val.errors)
coef(regfit.best, 10)


#
# following is to use the CV
#
k = 10
set.seed(1)
# make the 1-10 for each row
folds = sample(1:k, nrow(Hitters), replace = TRUE)
# make a na matrix, prepare for the 
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  # this is to do the fit without j row
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  # this should make the cv and get the test errors
  for (i in 1: 19) {
    # the predict should be just created before, but it does not show the full name here,
    # a little strange to me. 
    pred = predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = 'b')

which.min(mean.cv.errors)

reg.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)

### ridge regression
x = model.matrix(Salary~., Hitters)[, -1] #this is to make the x ready. 
y = Hitters$Salary
library(glmnet)
grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) 
#alpha = 0 --> ridge regression/ alpha=1 --> lasso model fit

dim(coef(ridge.mod))
# 20 is 20 predictor/ 100 is 100 lambda

predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
predict(ridge.mod, s = 50, type = "coefficients")

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train) # '-' is the exclude that row
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)

# the following shows that s=0 and the linear regression is in the same level. 
ridge.pred = predict(ridge.mod, s = 0, newx = x[test, ], exact = T)
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20, ]
# end

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

ridge.pred = predict (ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

### the lasso
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2)

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef [lasso.coef != 0]

## 6.7.1 Principal Cmponents Regression
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
# scale = TRUE---> standardize each predictor
# validation = CV --> to compute the ten-fold cross-validation error. 
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "RMSEP")

set.seed(1)
pcr.fit = pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred = predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test) ^ 2)

pcr.fit = pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

### 6.7.2 Partial Least Squares
set.seed(1)
pls.fit = plsr (Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred = predict (pls.fit, x[test, ], ncomp = 2)
mean ((pls.pred - y.test)^2)

pls.fit = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
