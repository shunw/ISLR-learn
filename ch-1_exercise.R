#---------------
# for the applied 8
#---------------
getwd()
setwd("/Users/zhang/Downloads/statslearning_standford/")
college = read.csv("college.csv")
rownames(college) = college[,1]

# get the row name of each row
college = college [, -1]

summary(college)

# draw the pair plot with the first 10 col
pairs(college[1:10])

boxplot(Outstate~Private, data = college)

# to make the Elite, following is my answer, but seems not right
nrow(college)
Elite = college$Top10perc/college$Enroll>.5
head(Elite)
# following is the book's
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame (college, Elite)
summary(college$Elite)

boxplot(Outstate~Elite, data = college)

par(mfrow = c(2, 2))

# personal investigation
plot(Enroll ~ Accept, data = college)
plot(Top10perc ~ Grad.Rate, data = college)
plot(perc.alumni ~ Top10perc, data = college)

# ----------------------
# for the Applied 9
# ----------------------
Auto = read.table("Auto.data.txt", header = T, na.strings = "?")
head(Auto)
dim(Auto)
Auto = na.omit(Auto)
dim(Auto)

summary(Auto)

sd(Auto$year)

dim(Auto)
Autorm = Auto[-(10:85), ]
dim(Autorm)
Auto[86:87,]
Autorm[9:12, ]


# ----------------------
# for the Applied 10
# ----------------------
library(MASS)
Boston
head(Boston)
?Boston

dim(Boston)

??scatterplot
pairs(Boston)

# for quation g
# to find the min medv data 
Boston[Boston$medv == min(Boston$medv),]

# for question h
dim(Boston[Boston$rm>7, ])
dim(Boston[Boston$rm>8, ])
Boston[Boston$rm>8, ]
