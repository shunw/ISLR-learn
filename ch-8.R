library (tree)
library (ISLR)
attach(Carseats)
High = ifelse ( Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

tree.carseats = tree ( High ~. - Sales, Carseats)
summary(tree.carseats)
