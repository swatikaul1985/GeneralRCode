# Author Auro Tripathy, auro@shatterline.com
# Adapted from ...Trevor Hastie's talk
rm(list=ls())

visualize.matrix <- function(mat) {
  print(names(mat))
  
  image(1:nrow(mat$x), 1:ncol(mat$x), z=mat$x,
        col = c("darkgreen", "white"),
        xlab = "Observations", ylab = "Attributes")
  
  title(main = "Visualizing the Sparse Binary Matrix",
        font.main = 4)
  return (dim(mat$x)) #returns the dimensions of the matrix
}

#---main---#


library(glmnet)
?glmnet
setwd("C:\\Github\\Pi's future\\elements of stats leanring exercise")
download.file("http://www.shatterline.com/MachineLearning/data/hiv.rda",
              "hiv.rda", mode="wb")
load("hiv.rda",
     verbose=TRUE) #contains hiv.train & hiv.test
visualize.matrix(hiv.train)
visualize.matrix(hiv.test)

print(length(hiv.train$y)) #length of response variable

fit <- glmnet(hiv.train$x,hiv.train$y, alpha=0) #Ridge penalty
plot(fit)
legend("bottomleft",legend=c("Ridge Penalty, alpha=0"))

fit <- glmnet(hiv.train$x,hiv.train$y, alpha=1) #Lasso penalty
plot(fit)
legend("bottomleft",legend=c("LASSO Penalty, alpha=1"))

fit <- glmnet(hiv.train$x,hiv.train$y, alpha=0.2) #ElasticNet penalty
plot(fit)
legend("bottomleft",legend=c("Elastic Net, alpha=0.2"))

cv.fit <- cv.glmnet(hiv.train$x,hiv.train$y) #10-fold cross-validation
plot(cv.fit)
legend("topleft",legend=c("10-fold Cross Validation"))
pred.y <- predict(fit, hiv.test$x) #predict the test data
mean.test.error <- apply((pred.y - hiv.test$y)^2,2,mean)
points(log(fit$lambda), mean.test.error, col="blue",pch="*")
legend("topleft",legend=c("10-fold Cross Validation","Test HIV Data"), pch="*", col=c("red","blue"))
plot(fit,xvar="lambda")
plot(fit,xvar="dev")