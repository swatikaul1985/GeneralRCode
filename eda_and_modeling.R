
require(RODBC)
require(dplyr)
require(data.table)
require(lubridate)
require(sqldf)
require(foreach)
require(doSNOW)
require(parallel)
require(doParallel)
require(moments)

library(data.table)
library(dplyr)


setwd("C:\\Github\\Pi\\data_science_job_takehome\\Marketing_email_campaign\\email")

email.table <- fread("email_table.csv", sep = ",")

email.open <- fread("email_opened_table.csv", sep = ",")

links.clicked <- fread("link_clicked_table.csv" , sep = ",")

# looking into each of tehse tables

head(email.table)


ex1 <- email.table %>% filter(email_id == 966622)


# 100,000 emails were sent to randomly selected customers

# 10,345 of these opened teh email, which makes email open rate at 10.34%

#  only 2,119 customers clicked on teh link, click trhough rate = people clickling on link/total emails sent = 2.1%


# joining email table with , email open data and links clicked data for correlations and exploration

email.open$open <- 1

links.clicked$clicked <- 1

email.all <- left_join(email.table,email.open)

email.all <- left_join(email.all,links.clicked)

head(email.all)


email.all$open[is.na(email.all$open)] <- 0

email.all$clicked[is.na(email.all$clicked)] <- 0


# eda

#  what is click rate for people who open teh email

email.all  %>% summarise(mean(clicked))

# mean(clicked)
# 1       0.02119

email.all %>% filter(open == 1) %>% summarise(mean(clicked))
# 
# mean(clicked)
# 1           0.2

#  10% of people open emails
# if the open the email then 20% of that population clicks

# checking for teh follwing variables relationship with clicks

# "email_text"          "email_version"       "hour"               
# "weekday"             "user_country"        "user_past_purchases"


email.all$clicked <- as.factor(email.all$clicked)

email.all$email_text <- as.factor(email.all$email_text)


email.all$email_version <- as.factor(email.all$email_version)

email.all$weekday <- as.factor(email.all$weekday)

email.all$user_country <- as.factor(email.all$user_country)

# looking at scatter plots

pairs(email.all, col = email.all$clicked)

# bar plots for frequency of links clicked in each of teh category of indepnedent variables

#  1. text type

df <- email.all

df$clicked <- as.numeric(df$clicked)

clicled_by_text <- df %>% group_by(email_text) %>% summarise(click_rate = mean(clicked))

xlabel_name <- clicled_by_text$email_text
H <- clicled_by_text$click_rate
a = paste("frequency plot for ",names(clicled_by_text)[1], sep = "")

barplot(H,names.arg = xlabel_name,xlab = names(clicled_by_text)[1] ,ylab = "ClickRatio(%)",col = "blue",main = a,border = "red")

# email_text click_rate
# <fctr>      <dbl>
#   1  long_email 0.01853767
# 2 short_email 0.02387177

#short email gets 1.3 times more clicked a long email

# do a chi square test for significance


#  2. email version, generic or personalized

# df <- email.all
# 
# df$clicked <- as.numeric(df$clicked)

clicled_by_email_version <- df %>% group_by(email_version) %>% summarise(click_rate = mean(clicked))

xlabel_name <- clicled_by_email_version$email_version

H <- clicled_by_text$click_rate
a = paste("frequency plot for ",names(clicled_by_text)[1], sep = "")

barplot(H,names.arg = xlabel_name,xlab = names(clicled_by_email_version)[1] ,ylab = "ClickRatio(%)",col = "blue",main = a,border = "red")


#  personalized gets more clicks


table(df$email_text,df$email_version)

#             generic personalized
# long_email    25236        25040
# short_email   24973        24751

# if they are short and personalized then?

df$shortAndPersonal <- ifelse((df$email_version == "personalized" & df$email_text == "short_email"),1,0)


clikedby_shortAndPersonal <- df %>% group_by(shortAndPersonal) %>% summarise(click_rate = mean(clicked))

xlabel_name <- clikedby_shortAndPersonal$shortAndPersonal

H <- clikedby_shortAndPersonal$click_rate
a = paste("frequency plot for ",names(clikedby_shortAndPersonal)[1], sep = "")

barplot(H,names.arg = xlabel_name,xlab = names(clikedby_shortAndPersonal)[1] ,ylab = "ClickRatio(%)",col = "blue",main = a,border = "red")


# it shoudl be short and it shoudl be personal, then its two tiems likely to click

# weekday

clikedby_weekday <- df %>% group_by(weekday) %>% summarise(click_rate = mean(clicked))

xlabel_name <- clikedby_weekday$weekday

H <- clikedby_weekday$click_rate
a = paste("frequency plot for ",names(clikedby_weekday)[1], sep = "")

barplot(H,names.arg = xlabel_name,xlab = names(clikedby_weekday)[1] ,ylab = "ClickRatio(%)",col = "blue",main = a,border = "red")

# it shoudl be sent on weekdays (Monday through thrusday)


# country

clikedby_country <- df %>% group_by(user_country) %>% summarise(click_rate = mean(clicked))

xlabel_name <- clikedby_country$user_country

H <- clikedby_country$click_rate
a = paste("frequency plot for ",names(clikedby_country)[1], sep = "")

barplot(H,names.arg = xlabel_name,xlab = names(clikedby_country)[1] ,ylab = "ClickRatio(%)",col = "blue",main = a,border = "red")


#  UK and US have higehr click rates

# France and ES have much lower click rates

table(df$weekday,df$user_country)

# summary: personalizesd short emails sent to UK and US on weekdays get clicked with higher rates


#  looking at user_purchases in past and time when email was sent

df <- df %>% select(hour,clicked, user_past_purchases)

DV <- "clicked"

IV <- c("hour", "user_past_purchases")

quantile_cutoff <- function(df,DV,IV) { 
  
  z<- function(x){quantile(x,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),na.rm = T)}
  
  IV.df <- as.data.frame(df %>% select(which(names(df) %in% IV)))
  
  IV.df <- as.data.frame(apply(IV.df,2,as.numeric))
  
  DV.df <- as.data.frame(df %>% select(which(names(df) %in% DV)))
  
  ptm <- proc.time()
  
  # Setting the up the cores
  
  cl = makeCluster(2)
  registerDoSNOW(cl)
  cat(paste(getDoParWorkers(),": Threads intiated!"))
  
  #clusterExport(cl, timeseriesFunc.list,envir = .GlobalEnv)
  
  out.df <- 
    foreach(j = 1:length(IV))  %dopar% { 
      require(dplyr)
      require(moments)
      require(reshape)
      fun <- z  
      #ptm <- proc.time()
      b <-  cut(IV.df[,j],unique(c(-10000,z(IV.df[,j]))))
      
      
      temp.df <- as.data.frame(cbind(DV.df,name = b))
      
      names(temp.df) <- c(DV , paste0(IV[j],"_Quantile_Cat"))
      
      final.df <- as.data.frame(cast(as.data.frame(table( label = temp.df[,1], temp.df[,2])), Var2 ~ label))
      
      final.df$pctLabel = (final.df$`1`/(final.df$`1`+final.df$`0`))*100
      
      final.df$TotalPopulation <- ((final.df$`1`+final.df$`0`)/nrow(DV.df))*100
      
      final.df$`0` <- NULL
      
      names(final.df)[1] <- c(paste0(IV[j],"_Quantile_Cat"))
      
      final.df
    }
  
  
  
  stopCluster(cl)
  # Stop the clock
  proc.time() - ptm
  
  for(i in 1:length(out.df)){
    a = paste("frequency plot for ",IV[i], sep = "")
    write.csv(out.df[[i]],paste0(a,".csv"),row.names = F)
    b = paste0(IV[i],"_Quantile_Cat")
    H <- out.df[[i]]$pctLabel
    L <- out.df[[i]]$TotalPopulation
    M <- out.df[[i]][1]
    xlabel_name <- as.character(M[[1]])
    png(file = paste("frequency plot for ",IV[i],".png" ,sep = ""), height=630, width = 864)
    par(mfrow = c(2, 1)) 
    barplot(H,names.arg = xlabel_name,xlab = b,ylab = "LabelRatio(%)",col = "blue",main = a,border = "red")
    barplot(L,names.arg = xlabel_name,xlab = b,ylab = "Population(%)",col = "blue",main = a,border = "red")
    dev.off()
  }
  
  return(out.df)
}

t <- quantile_cutoff(df,DV,IV)

hour <- t[[1]]

# 8-11 am are when is teh best time to send emails
#  if they have purchased more tahn 5 times they are more likely to click


# So we are done with bivariates and we have gained insights into teh clicke rates



# buidl a predictive mdoel to predict which email is going to get clicked


# we have teh follwoing variables in teh model
# 
# names(email.all)
# [1] "email_id"            "email_text"          "email_version"       "hour"               
# [5] "weekday"             "user_country"        "user_past_purchases" "open"               
# [9] "clicked" 

IV <- c("email_text","email_version","hour","weekday","user_country","user_past_purchases")
DV <- "clicked"

# logistic regression


# https://www.r-bloggers.com/evaluating-logistic-regression-models/

var.list <- c(IV,DV)

df = email.all[,var.list]

glm.fit = glm(clicked ~ . , data = df, family = binomial)
summary(glm.fit)

glm.probs = predict(glm.fit,type = "response")

glm.pred = ifelse(glm.probs> 0.1,"Click_Yes","Click_No")

attach(df)
# AIC: 19014

table(glm.pred,clicked)

df$glm.probs.train = glm.probs

obj <- hist(df$glm.probs.train, col = df$clicked)

# now create train and test

library(caret)

df1 <- df

df1$glm.probs.train <- NULL

Train <- createDataPartition(df1$clicked,p=0.6,list = F)

df.train <- df1[Train,]
df.test <- df1[-Train,]

names(df.train)

mod_fit <- train(clicked ~ .,  data=df.train, method="glm", family="binomial")

summary(mod_fit)

mod_fit$finalModel

exp(coef(mod_fit$finalModel))
# (Intercept)     email_textshort_email email_versionpersonalized 
# 0.0009724098              1.3715181638              1.8919036180 
# hour             weekdayMonday           weekdaySaturday 
# 1.0177704415              1.6839859725              1.2824764531 
# weekdaySunday           weekdayThursday            weekdayTuesday 
# 1.1798521440              1.9016783353              1.8563865648 
# weekdayWednesday            user_countryFR            user_countryUK 
# 2.0659974336              1.0564298172              3.2702462343 
# user_countryUS       user_past_purchases 
# 3.4190460782              1.2011588564 

# predicting with the above model
attach(df.test)

test.prob = predict(mod_fit,newdata = df.test, type = "prob")

test.pred = ifelse(test.prob$`1` > 0.1 ,1,0)

test.prob$clicked = clicked

ggplot(test.prob, aes(x=`1`, fill=clicked)) + geom_histogram(binwidth=.025, alpha=.5, position="identity")

ggplot(test.prob, aes(x=`1`, colour=clicked)) + geom_density()

glm.probs = predict(glm.fit,type = "response")

glm.pred = ifelse(glm.probs> 0.1,"Click_Yes","Click_No")


mod_fit2 <- train(clicked ~ user_past_purchases + email_version + email_text,  data=df.train, method="glm", family="binomial")

# AIC shoudl be lower teh better

test.pred <- as.factor(test.pred)

accuracy <- table(test.pred,clicked)

sum(diag(accuracy))/sum(accuracy)


# no information model

mean(as.numeric(as.character(df.test$clicked)))

# 2.1%

# accuracy is 97.88

#  so we are doing worse than no information model

confusionMatrix(data=test.pred, df.test$clicked)


# Confusion Matrix and Statistics

# Reference
# Prediction     0     1
# 0 38674   800
# 1   478    47
# 
# Accuracy : 0.968           
# 95% CI : (0.9663, 0.9698)
# No Information Rate : 0.9788          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0532          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.98779         
# Specificity : 0.05549         
# Pos Pred Value : 0.97973         
# Neg Pred Value : 0.08952         
# Prevalence : 0.97882         
# Detection Rate : 0.96687         
# Detection Prevalence : 0.98687         
# Balanced Accuracy : 0.52164         
# 
# 'Positive' Class : 0  


# compute AUC and ROC curve on test data


library(pROC)

test <- cbind(df.test,pred_prob = test.prob$`1`)

f1 = roc(clicked ~ pred_prob, data=test)


plot(f1, col="red")


f1$auc

# Area under the curve: 0.736



# Compute AUC for predicting Class with the model
library(ROCR)
prob <- predict(mod_fit, newdata=df.test, type="prob")
names(prob) <- c("No","Yes")

pred <- prediction(prob$Yes, df.test$clicked)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)


auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# cross validation error K-Fold Cross Validation

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(clicked ~ .,  data=df.train, method="glm", family="binomial", trControl = ctrl,tuneLength = 5)


attach(df.test)

test.prob = predict(mod_fit,newdata = df.test, type = "prob")

test.pred = ifelse(test.prob$`1` > 0.1 ,1,0)


confusionMatrix(data=test.pred, df.test$clicked)

ggplot(test.prob, aes(x=`1`, fill=clicked)) + geom_histogram(binwidth=.025, alpha=.5, position="identity")

ggplot(test.prob, aes(x=`1`, colour=clicked)) + geom_density()

sum(df.test$clicked)

# use smote first on train data




# use non linear approach like random forest












