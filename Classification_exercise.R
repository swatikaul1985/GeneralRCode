# glm functions in R
require(ISLR)

names(Smarket)

summary(Smarket)


# direction is a response to be predicted
 pairs(Smarket, col = Smarket$Direction
       )
 
 # logistic regression
 
 drop <-  c("Year","Today")
 
 var.list <- setdiff(names(Smarket),drop)
 
 subset.Smarket = Smarket[,var.list]
 
 glm.fit = glm(Direction ~ . , data = subset.Smarket, family = binomial)
 summary(glm.fit)
 
 glm.probs = predict(glm.fit,type = "response")
 
 glm.pred = ifelse(glm.probs> 0.5,"Up","Down")
 
 attach(Smarket)
 
 
 table(glm.pred,Direction)
 
 mean(glm.pred==Direction)
 
 # make training and test set
 
 train <- Year < 2005
 
 
 glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3+ Lag4  + Lag5 + Volume, data = Smarket, family = binomial,subset = train)
 
 test = Smarket[!train,]
 
 glm.probs = predict(glm.fit,newdata = test,type = "response")
 
 glm.pred = ifelse(glm.probs > 0.5,"Up", "Down")
 

  Direction.2005 <- Smarket$Direction[!train]
 table(Direction.2005,glm.pred)
 
 mean(Direction.2005 == glm.pred)
  