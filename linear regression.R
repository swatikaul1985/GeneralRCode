
library(MASS)
library(ISLR)

# simple linear regression
names(Boston)
?Boston
head(Boston)
attach(Boston)
plot(medv~lstat,col = "blue",pch = 20)

fit1 <- lm(medv ~ lstat,data = Boston)

fit1

summary(fit1)

fit1$coefficients

abline(fit1,col = "red")

# confidence interval

confint(fit1)

plot(fit1)
par(mfrow(2,2))

# predictions

pred = predict(fit1,data.frame(lstat = c(5,10,15)),interval = "confidence")

#  multiple linear regression

fit2 <- lm(data = Boston, medv ~ lstat+age)
fit2
summary(fit2)

#  all vars included

fit3 <- lm(data = Boston, medv ~ .)
summary(fit3)
par(mfrow = c(2,2))

plot(fit3)

fit4 <- update(fit3, ~ .-age-indus)

#  INTERACTION  


fit5 <- lm(data = Boston, medv ~ .+lstat*age+I(lstat^2))

par(mfrow = c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit5),col = "red",pch = 20)

fit6 <- lm(data = Boston,medv ~ poly(lstat,4))

