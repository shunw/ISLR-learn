library(MASS)
library(ISLR)

#wendy's tryout
# about the adverstising data
ad = read.csv(file = "Advertising.csv", header = T)
ad = ad[-1]
ad.lm = lm(Sales ~ TV, data = ad)
plot (Sales ~ TV, data = ad)
abline(ad.lm, col = "red")

summary(ad.lm)$coef
beta0_95low = 7.032 - .4578*2
beta0_95high = 7.032 + .4578*2
beta1_95low = 0.04753664 - 0.002690607 * 2
beta1_95high = 0.04753664 + 0.002690607 * 2

# this is to ge the correlation of the elements in the dataset. 
cor(ad[-1])

ad.lm_all = lm(Sales ~ ., data = ad)
summary(ad.lm_all)
