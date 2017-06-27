
# Author: swkaul

# Model for leads that have gone through a trial or have become known through marketing channels

# Modeling dataset: ModelPopulation.v11



############################# Model phase starts here ###############################################################

setwd("E:/SWKAUL/TFS/Data Sciences/Lead Scoring V2/Documents")

install.packages('reshape')
install.packages('data.table')
install.packages('stringr')
install.packages('Epi')
library(stringr)
library(data.table)
library(reshape)
library(dplyr)
library(RODBC)
library(caret)
library(Epi)
library(ROCR)




# Converting a few features to factor type

ModelPopulation.v11$HasUsed_80_pct_FreeCred = as.factor(ModelPopulation.v11$HasUsed_80_pct_FreeCred)
ModelPopulation.v11$HasUsed_AppServiceNoIAAS =  as.factor(ModelPopulation.v11$HasUsed_AppServiceNoIAAS)
ModelPopulation.v11$UsedFirstDay =  as.factor(ModelPopulation.v11$UsedFirstDay)
ModelPopulation.v11$UsedLastDay =  as.factor(ModelPopulation.v11$UsedLastDay)
ModelPopulation.v11$Used_IAAS_LastDay = as.factor(ModelPopulation.v11$Used_IAAS_LastDay)
ModelPopulation.v11$Used_BWDT_LastDay = as.factor(ModelPopulation.v11$Used_BWDT_LastDay)


# creating training dataset, validation set and testing dataset

# 70% as training, 15% as validation and 15% as testing

# selecting training sample
# 
# ModelPopulation.Train <- ModelPopulation.v11 %>% group_by(WonFlag) %>% sample_frac(0.7)
# 
# 
# ModelPopulation.NotTrained <- ModelPopulation.v11[-ModelPopulation.Train,]
#                                           
# require(caTools)
# 
# sample = sample.split(ModelPopulation.v11$leadid, SplitRatio = .70)
# 
# # training set creation
# ModelPopulation.Train = subset(ModelPopulation.v11, sample == TRUE)
# 
# ModelPopulation.NotTrained = subset(ModelPopulation.v11, sample == FALSE)
# 
# sample.valid = sample.split(ModelPopulation.NotTrained$leadid, SplitRatio = .50)
# 
# 
# #Validation set creation
# 
# ModelPopulation.Validation = subset(ModelPopulation.NotTrained, sample.valid == TRUE)
# 
# # test ste creation
# 
# ModelPopulation.Test = subset(ModelPopulation.NotTrained, sample.valid == FALSE)


# feature selection to understand variables that are important predictors


# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(e1071)
# load the data

###### correction to variable AgeOfLeadInDays from datafram: AcquisitionDate.v1

dropVar <- c("AgeOfLeadInDays","AgeOfLeadInDays_T")

ModelPopulation.v11 <- ModelPopulation.v11[,!(names(ModelPopulation.v11) %in% dropVar)]

AcquisitionDate.v1.FOrCorrection <- AcquisitionDate.v1 %>% select(Leadid,AgeOfLeadInDays)


ModelPopulation.v11 <-  left_join(ModelPopulation.v11,AcquisitionDate.v1.FOrCorrection, by = c("leadid" = "Leadid" ))


ModelPopulation.v11 <- ModelPopulation.v11 %>% mutate(AgeOfLeadInDays_T = ifelse(is.na(AgeOfLeadInDays), 153, AgeOfLeadInDays))

ModelPopulation.v11 <- ModelPopulation.v11 %>% mutate(AgeOfLeadInDays_T = ifelse(AgeOfLeadInDays_T < 0 , 0, AgeOfLeadInDays_T))

FOrScoring <- ModelPopulation.v11[,c(1:45,288:292,449,472:555)]

write.csv(ModelPopulation.v11[,c(1:45,288:292,449,472:555)],"LSM_forScoringAllLeads.csv")

dropVar <- c("SubscriptionGUID.x","subscriptionguid","AgeOfLeadInDays","LeadSource_T","SubscriptionGUID.y","i_billable_acct_id","si_subscription_ref_id","WonDate","leadid","Activated Microsoft Azure Account","OriginatingOffer","Signed up for Microsoft Azure Account","Azure Account Member","Azure Account Email Delivered","Azure Account Open Email","Azure Account Send Email","List Import Member","List Import Send Email")
ModelPopulation.v12 <- ModelPopulation.v11[,!(names(ModelPopulation.v11) %in% dropVar)]

# Subsetting varaibles to remove page visited and interesting moment type variables

# need to remove variables like nurture member 

names(ModelPopulation.v12) <- make.names(names(ModelPopulation.v12), unique = TRUE, allow_ = FALSE)

ModelPopulation.Subset.1<- ModelPopulation.v12[,c(1:37,280:283,440,463:538)]


# testing dropping afew email send and nurture program member type features from teh feature set

dropVar <- c("EmailSentCount","Email.Send.Send.Email","Data.Management.Member","Signed.up.for.Microsoft.Azure.Account...Free.Trial" ,"Nurture.Send.Email","Nurture.Email.Delivered","Nurture.Member","Email.Send.Member","Email.Send.Email.Delivered","Email.Blast.Email.Delivered","Nurture.Email.Delivered","List.Import.Email.Delivered" ,"Email.Send.Email.Delivered","Newsletters.Email.Delivered","Newsletters.Member")

ModelPopulation.Subset.2 <- ModelPopulation.Subset.1[,!(names(ModelPopulation.Subset.1) %in% dropVar)]


write.csv(ModelPopulation.Subset.2, "ModelPopulation_LSM_reducedSetVars.csv")

# reducing sample size to tune the GBM model using caret

ModelPopulation.Subset.ReducedSize <- ModelPopulation.Subset.2 %>% group_by(WonFlag) %>% sample_frac(0.3)

predictors <- names(ModelPopulation.Subset.ReducedSize)[names(ModelPopulation.Subset.ReducedSize) != "WonFlag"]

inTrainingSet <- createDataPartition(ModelPopulation.Subset.ReducedSize$WonFlag,
                                        p = .75, list = FALSE)
Train <- ModelPopulation.Subset.ReducedSize[ inTrainingSet,]
Test <- ModelPopulation.Subset.ReducedSize[-inTrainingSet,]

library(gbm)
 # The gbm function does not accept factor response values so we
 # will make a copy and modify the outcome variable

forGBM <- Train
 
names(forGBM) <- make.names(names(forGBM), unique = TRUE, allow_ = FALSE)


forGBM$WonFlag <- ifelse(forGBM$WonFlag == 1 , "yes", "no")


forGBM$WonFlag <- as.factor(forGBM$WonFlag)



forGBM.Test <- Test

names(forGBM.Test) <- make.names(names(forGBM.Test), unique = TRUE, allow_ = FALSE)


forGBM.Test$WonFlag <- ifelse(forGBM.Test$WonFlag == 1 , "yes", "no")


forGBM.Test$WonFlag <- as.factor(forGBM.Test$WonFlag)


install.packages('e1071', dependencies=TRUE)
library(e1071)

library(doParallel)
library(caret)
#library(Rmpi)
library(plyr)


 # GBM Model tuning 
 
 grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                      n.trees = seq(100, 1000, by = 100),
                       shrinkage = c(0.01, 0.1),
                     n.minobsinnode = 20)
 
 ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                      allowParallel = TRUE)
 
 
 library('doParallel')
 cl <- makeCluster(4) #number of cores
 registerDoParallel(cl)
 

 set.seed(1)

 gbmTune <- train(WonFlag ~ ., data = forGBM,
                     method = "gbm",
                     metric = "ROC",
                     tuneGrid = grid,
                     verbose = FALSE,
                     trControl = ctrl)
 
 stopCluster(cl)
 
 ggplot(gbmTune) + theme(legend.position = "top")
 
 gbmPred <- predict(gbmTune, forGBM.Test) 
 
 str(gbmPred)
 
 gbmProbs <- predict(gbmTune, forGBM.Test, type = "prob") 
 str(gbmProbs)
 
 confusionMatrix(gbmPred, forGBM.Test$WonFlag)
 
 rocCurve <- roc(response = forGBM.Test$WonFlag,  predictor = gbmProbs[, "yes"],  levels = rev(levels(forGBM.Test$WonFlag))) 
 
plot(rocCurve)



str(rocCurve)

gbmImp <- varImp(gbmTune, scale = FALSE)

plot(gbmImp, top = 20)

Imp.Predictor <- c()

forGBM.Test.Probs <- as.data.frame(cbind(forGBM.Test,gbmProbs))

head(forGBM.Test.Probs$WonFlag)

library(dplyr)

forGBM.Test.Probs.1 <- forGBM.Test.Probs %>%
                select(WonFlag,yes,AgeOfLeadInDays.T, Newsletters.Member, Nurture.Member ,NormalizedUsage ,NormalizedUsage.Emerging.WL) %>%
              arrange(desc(yes))



write.csv(as.data.frame(rbind(head(forGBM.Test.Probs.1,10),tail(forGBM.Test.Probs.1,10))), "topAndBottomScoredLits.csv")



#partial dependence plots 

# par(mfrow=c(1,2))
# 
# library(gbm)
# 
# names(Train) <- make.names(names(Train), unique = TRUE, allow_ = FALSE)
# 
# 
# 
# 
# Train$WonFlag <- as.factor(Train$WonFlag)
# 
# 
# fit <- gbm(WonFlag ~ ., data = Train ,n.trees = 1000, shrinkage = 0.1 ,interaction.depth = 7,n.minobsinnode = 20)
# 
# summary(fit)
# 
# plot(fit,i.var="AgeOfLeadInDays_T",lwd = 2, main = "")



# only 20 most important variables shown (out of 117)
# 
# Overall
# AgeOfLeadInDays.T                 3485.7
# Newsletters.Member                2753.6
# Nurture.Member                    2033.7
# NormalizedUsage                   1717.3
# NormalizedUsage.Emerging.WL       1667.6
# Email.Send.Send.Email              901.0
# Content.Send.Email                 861.6
# Data.Management.Member             840.0
# InterestingMoment.MileStoneCount   797.3
# EmailSentCount                     746.3
# Basic.Email.Engaged...Success      347.4
# DaysUsed.Emerging.WL               333.2
# Email.Send.Member                  239.1
# LastWeek.Usage                     217.8
# Power.BI.Account.Member            217.4
# NormalizedUsage.Estab.WL           180.8
# ThirdWeek.Usage                    155.8
# FirstWeek.Usage                    154.2
# SecondWeek.Usage                   139.6
# NormalizedUsage.Core.Servc         129.3


performance <- as.data.frame(cbind(thresholds = rocCurve$thresholds,sensitivities = rocCurve$sensitivities,specificities= rocCurve$specificities))

head(performance %>% filter(sensitivities > 0.8 &  specificities > 0.8 & specificities < 0.85 & sensitivities < 0.9))

################# testing on teh rest of teh data not used for training ####################




Test2 <- ModelPopulation.Subset.1

forGBMTest2 <- Test2

names(forGBMTest2) <- make.names(names(forGBMTest2), unique = TRUE, allow_ = FALSE)


forGBMTest2$WonFlag <- ifelse(forGBMTest2$WonFlag == 1 , "yes", "no")


forGBMTest2$WonFlag <- as.factor(forGBMTest2$WonFlag)

gbmPred.Test2 <- predict(gbmTune, forGBMTest2) 

str(gbmPred.Test2)

gbmProbs.Test2 <- predict(gbmTune, forGBMTest2, type = "prob") 
str(gbmProbs.Test2)

confusionMatrix(gbmPred.Test2, forGBMTest2$WonFlag) # on entire data 70% of which was not used in training

confusionMatrix(gbmPred, forGBM.Test$WonFlag) # on test data 

rocCurve.Test2 <- roc(response = forGBMTest2$WonFlag,  predictor = gbmProbs.Test2[, "yes"],  levels = rev(levels(forGBMTest2$WonFlag))) 

plot(rocCurve.Test2)

###############################################################################################

#Now that we have our sample, data, let's look at optimizing the decision threshold by the apex of the ROC curve:



library(Epi) # To help us vizualize the apex of the ROC curve 

opt<-ROC(gbmProbs[, "yes"], forGBM.Test$WonFlag ,plot = c("ROC"))

#We can see the apex quite clearly. 


#Next, using the same data, let's extract a different threshold optimizing for the F-measure

library(ROCR) # Used to calculate the F-measure
predROCR = prediction(gbmProbs[, "yes"], forGBM.Test$WonFlag)
perfROCR = performance(predROCR,"f") #calculates the f measures across cutoffs

# Plot the output, add a red line at the maximum
plot(perfROCR) 
abline(v=PRFMAX,col="red")


prfmx<-max(perfROCR@y.values[[1]],na.rm=T) # extracts the max F measure
PRFMAX<-min(perfROCR@x.values[[1]][perfROCR@y.values[[1]]==prfmx],na.rm=T)
print(PRFMAX)


# threshold: 0.3586712


############ Visualizing top 20 most important variables ########################

# only 20 most important variables shown (out of 117)

# Overall
# AgeOfLeadInDays.T                 3485.7
# Newsletters.Member                2753.6
# Nurture.Member                    2033.7
# NormalizedUsage                   1717.3
# NormalizedUsage.Emerging.WL       1667.6
# Email.Send.Send.Email              901.0
# Content.Send.Email                 861.6
# Data.Management.Member             840.0
# InterestingMoment.MileStoneCount   797.3
# EmailSentCount                     746.3
# Basic.Email.Engaged...Success      347.4
# DaysUsed.Emerging.WL               333.2
# Email.Send.Member                  239.1
# LastWeek.Usage                     217.8
# Power.BI.Account.Member            217.4
# NormalizedUsage.Estab.WL           180.8
# ThirdWeek.Usage                    155.8
# FirstWeek.Usage                    154.2
# SecondWeek.Usage                   139.6
# NormalizedUsage.Core.Servc         129.3



#################  check for multicollinearilty##############################

names(ModelPopulation.Subset.1) <- make.names(names(ModelPopulation.Subset.1), unique = TRUE, allow_ = FALSE)





ModelPopulation.Subset.1$WonFlag <- as.factor(ModelPopulation.Subset.1$WonFlag)

Imp.Vars.Subset <- ModelPopulation.Subset.1 %>%
                      select (AgeOfLeadInDays.T                 ,
                              Newsletters.Member                ,
                              Nurture.Member                    ,
                              NormalizedUsage                   ,
                              NormalizedUsage.Emerging.WL       ,
                              Email.Send.Send.Email              ,
                              Content.Send.Email                ,
                              Data.Management.Member             ,
                              InterestingMoment.MileStoneCount   ,
                              EmailSentCount                     ,
                              Basic.Email.Engaged...Success      ,
                              DaysUsed.Emerging.WL               ,
                              Email.Send.Member                  ,
                              LastWeek.Usage                     ,
                              Power.BI.Account.Member            ,
                              NormalizedUsage.Estab.WL           ,
                              ThirdWeek.Usage                    ,
                              FirstWeek.Usage                    ,
                              SecondWeek.Usage                   ,
                              NormalizedUsage.Core.Servc         ,
                              WonFlag) %>%
                          group_by(WonFlag) %>%
                          summarise_each(funs(mean,median))
  

##################  histModelPopulation.Subset.1$AgeOfLeadInDays.T) ##########################
# histogram
ggplot(ModelPopulation.Subset.1, aes(x=AgeOfLeadInDays.T)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(AgeOfLeadInDays.T, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=AgeOfLeadInDays.T)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(AgeOfLeadInDays.T)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=AgeOfLeadInDays.T, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ddply(ModelPopulation.Subset.1, "WonFlag", summarise, AgeOfLeadInDays.T.mean=mean(AgeOfLeadInDays.T))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.1, aes(x=AgeOfLeadInDays.T, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.2, aes(xintercept=AgeOfLeadInDays.T.mean,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.1, aes(x=WonFlag, y=AgeOfLeadInDays.T, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)



#######################      visualization of  Newsletters.Member ################################

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Newsletters.Member)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Newsletters.Member, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Newsletters.Member)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Newsletters.Member)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Newsletters.Member, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)

ModelPopulation.Subset.2 <- ddply(ModelPopulation.Subset.1, "WonFlag", summarise, Newsletters.Member=mean(Newsletters.Member))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.1, aes(x=Newsletters.Member, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.2, aes(xintercept=Newsletters.Member,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.1, aes(x=WonFlag, y=Newsletters.Member, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)


#######################      visualization of  Nurture.Member ################################

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Nurture.Member)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Nurture.Member, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Nurture.Member)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Nurture.Member)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Nurture.Member, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ddply(ModelPopulation.Subset.1, "WonFlag", summarise, Nurture.Member=mean(Nurture.Member))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.1, aes(x=Nurture.Member, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.2, aes(xintercept=Nurture.Member,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.1, aes(x=WonFlag, y=Nurture.Member, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)

##NormalizedUsage

#######################      visualization of  NormalizedUsage ################################

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(NormalizedUsage, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(NormalizedUsage)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(NormalizedUsage < 500)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, NormalizedUsage=mean(NormalizedUsage))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=NormalizedUsage, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=NormalizedUsage,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=NormalizedUsage, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)



#NormalizedUsage.Emerging.WL

#######################      visualization of  NormalizedUsage ################################

Var <- "NormalizedUsage.Emerging.WL"

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage.Emerging.WL)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(NormalizedUsage.Emerging.WL, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage.Emerging.WL)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(NormalizedUsage.Emerging.WL)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=NormalizedUsage.Emerging.WL, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(NormalizedUsage.Emerging.WL < 25)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, NormalizedUsage.Emerging.WL=mean(NormalizedUsage.Emerging.WL))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=NormalizedUsage.Emerging.WL, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=NormalizedUsage.Emerging.WL,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=NormalizedUsage.Emerging.WL, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)






#######################      visualization of  


#######################      visualization of  NormalizedUsage ################################

## InterestingMoment.MileStoneCount

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(InterestingMoment.MileStoneCount, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(InterestingMoment.MileStoneCount)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(InterestingMoment.MileStoneCount < 100)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, InterestingMoment.MileStoneCount=mean(InterestingMoment.MileStoneCount))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=InterestingMoment.MileStoneCount, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=InterestingMoment.MileStoneCount,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=InterestingMoment.MileStoneCount, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE) ################################

## InterestingMoment.MileStoneCount

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(InterestingMoment.MileStoneCount, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(InterestingMoment.MileStoneCount)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=InterestingMoment.MileStoneCount, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(InterestingMoment.MileStoneCount < 100)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, InterestingMoment.MileStoneCount=mean(InterestingMoment.MileStoneCount))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=InterestingMoment.MileStoneCount, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=InterestingMoment.MileStoneCount,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=InterestingMoment.MileStoneCount, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)





#######################      visualization of  NormalizedUsage ################################

## Email.Send.Send.Email

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Send.Email)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Email.Send.Send.Email, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Send.Email)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Email.Send.Send.Email)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Send.Email, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(Email.Send.Send.Email < 100)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, Email.Send.Send.Email=mean(Email.Send.Send.Email))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=Email.Send.Send.Email, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=Email.Send.Send.Email,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=Email.Send.Send.Email, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)


#######################      visualization of  Email.Send.Click.Email ################################

## Email.Send.Click.Email

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Click.Email)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Email.Send.Click.Email, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Click.Email)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Email.Send.Click.Email)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Email.Send.Click.Email, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(Email.Send.Click.Email < 100)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, Email.Send.Click.Email=mean(Email.Send.Click.Email))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=Email.Send.Click.Email, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=Email.Send.Click.Email,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=Email.Send.Click.Email, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)

## CountWebPageVisisted

#######################      visualization of  CountWebPageVisisted ################################

## CountWebPageVisisted

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=CountWebPageVisisted)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(CountWebPageVisisted, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=CountWebPageVisisted)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(CountWebPageVisisted)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=CountWebPageVisisted, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(CountWebPageVisisted < 20)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, CountWebPageVisisted=mean(CountWebPageVisisted))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=CountWebPageVisisted, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=CountWebPageVisisted,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=CountWebPageVisisted, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)

#######################      visualization of  Webinar.Open.Email ################################

## Webinar.Open.Email

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Open.Email)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Webinar.Open.Email, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Open.Email)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Webinar.Open.Email)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Open.Email, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(Webinar.Open.Email < 20)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, Webinar.Open.Email=mean(Webinar.Open.Email))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=Webinar.Open.Email, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=Webinar.Open.Email,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=Webinar.Open.Email, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)


#######################      visualization of  Webinar.Attended.On.demand..Success ################################

## Webinar.Attended.On.demand..Success

# histogram
ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Attended.On.demand..Success)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Webinar.Attended.On.demand..Success, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# side by side historgram

ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Attended.On.demand..Success)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(WonFlag ~ .) +
  geom_vline(data=ModelPopulation.Subset.1, aes(xintercept=mean(Webinar.Attended.On.demand..Success)),
             linetype="dashed", size=1, colour="red")

# Overlaid histograms
ggplot(ModelPopulation.Subset.1, aes(x=Webinar.Attended.On.demand..Success, fill=WonFlag)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

library(plyr)
ModelPopulation.Subset.2 <- ModelPopulation.Subset.1 %>%
  filter(Webinar.Attended.On.demand..Success < 5)

ModelPopulation.Subset.3 <- ddply(ModelPopulation.Subset.2, "WonFlag", summarise, Webinar.Attended.On.demand..Success=mean(Webinar.Attended.On.demand..Success))


# Density plots with semi-transparent fill
ggplot(ModelPopulation.Subset.2, aes(x=Webinar.Attended.On.demand..Success, fill=WonFlag)) + geom_density(alpha=.3) +
  geom_vline(data=ModelPopulation.Subset.3, aes(xintercept=Webinar.Attended.On.demand..Success,  colour=WonFlag),
             linetype="dashed", size=1)


# Box plot 
ggplot(ModelPopulation.Subset.2, aes(x=WonFlag, y=Webinar.Attended.On.demand..Success, fill=WonFlag)) + geom_boxplot() +
  guides(fill=FALSE)

############### check for correlations with 117 variables and remove colliner vars ################

# data = ModelPopulation.Subset.1
ModelPopulation.NZ <- ModelPopulation.Subset.1

ModelPopulation.NZ$HasUsed.80.pct.FreeCred <-  as.numeric(ModelPopulation.NZ$HasUsed.80.pct.FreeCred)

####  near zero variance predictors #####

nzv <- nearZeroVar(ModelPopulation.NZ[,2:ncol(ModelPopulation.NZ)], saveMetrics= TRUE)


#a vector of logicals for whether the predictor is a near zero variance predictor
nzv[nzv$nzv,][1:60,]

nzv.1 <- nearZeroVar(ModelPopulation.NZ[,2:ncol(ModelPopulation.NZ)])

filtered.ModelPopulation.NZ <- ModelPopulation.NZ[,-nzv.1]

################## checking for correlations #################################
#data = filtered.ModelPopulation.NZ

filtered.ModelPopulation.NZ.Cor <-  cor(filtered.ModelPopulation.NZ[,c(2:18,23:30)])
#highCorr <- sum(abs(filtered.ModelPopulation.NZ.Cor[upper.tri(filtered.ModelPopulation.NZ.Cor)]) > .99)

highlyCorDescr <- findCorrelation(filtered.ModelPopulation.NZ.Cor, cutoff = .75, verbose = TRUE, names = FALSE )

filtereCorrData <- filtered.ModelPopulation.NZ[,c(2:18,23:30)]
filteredDescr <- filtereCorrData[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

filtereCorrData.final <-cbind(filtered.ModelPopulation.NZ[,-c(2:18,23:30)],filteredDescr)


################# running glm with 117 variables to start with ##############################



library(ggplot2)
library(randomForest)




ModelPopulation.Subset.1$WonFlag <- as.factor(ModelPopulation.Subset.1$WonFlag)



inTrainingSet.glm <- createDataPartition(ModelPopulation.Subset.1$WonFlag,
                                         p = .75, list = FALSE)

Train.glm <- ModelPopulation.Subset.1[ inTrainingSet.glm,]
Test.glm <- ModelPopulation.Subset.1[-inTrainingSet.glm,]


fit <- glm( WonFlag ~ ., data = Train.glm, family='binomial', model = TRUE )

summary(fit)



# Calculate Odds Ratio and Plot
is_sig <- summary(fit)$coeff[-1,4] < 0.05
sig_vars <- summary(fit)$coeff[is_sig,1]
odds <- (sort(exp(sig_vars)) - 1)*100
df.odds <- data.frame(var_names = names(odds), estimate = odds, isPositive = (odds >= 0))
ggplot(data = df.odds[1:10,], aes(x = var_names, y = estimate, fill=isPositive)) + 
  geom_bar(stat="identity")

# results show that there is a lot of multicollinearity in variables




#rf <- randomForest(ChurnTag ~ ., 
#                    data = dataset1, importance=TRUE, ntree=500)

# varImpPlot(rf)




################# run glm with these top 20 variables that are not correlated ########################

#data = filtereCorrData.final 

str(filtereCorrData.final)




filtereCorrData.final$WonFlag <- as.factor(filtereCorrData.final$WonFlag)


inTrainingSet.glm <- createDataPartition(filtereCorrData.final$WonFlag,
                                     p = .75, list = FALSE)
Train.glm <- filtereCorrData.final[ inTrainingSet.glm,]
Test.glm <- filtereCorrData.final[-inTrainingSet.glm,]

Model.Train.glm <- glm( WonFlag ~ ., data = Train.glm, family='binomial', model = TRUE )


summary(Model.Train.glm)



# Calculate Odds Ratio and Plot
is_sig <- summary(Model.Train.glm)$coeff[-1,4] < 0.05
sig_vars <- summary(Model.Train.glm)$coeff[is_sig,1]
odds <- (sort(exp(sig_vars)) - 1)*100
df.odds <- data.frame(var_names = names(odds), estimate = odds, isPositive = (odds >= 0))


ggplot(data = df.odds[2:10,], aes(x = var_names, y = estimate, fill=isPositive)) + 
  geom_bar(stat="identity")

# running glm with only these variables with significance

Model.Train.glm.2 <- glm( WonFlag ~  Used.Estab.WL.LastDay + Used.Emerging.WL.LastDay     
                           + Used.IAAS.LastDay + UsedFirstDay + DaysToActivation                
                           + FirstWeek.Usage + LastWeek.Usage + ThirdWeek.Usage                    
                           + HasUsed.80.pct.FreeCred + NormalizedUsage.Emerging.WL + NormalizedUsage.PAAS.AppServc       
                           + Contact.Me.Filled.Out.Form...Success + Email.Send.Engaged...Success + Inside.Sales.Member                
                           + Power.BI.Account.Member + EmailOpenRate + AgeOfLeadInDays.T
                          , data = Train.glm, family='binomial', model = TRUE )



# Calculate Odds Ratio and Plot
is_sig <- summary(Model.Train.glm.2)$coeff[-1,4] < 0.05
sig_vars <- summary(Model.Train.glm.2)$coeff[is_sig,1]
odds <- (sort(exp(sig_vars)) - 1)*100

 # equation for logistic fit model
df.odds <- data.frame(var_names = names(odds), estimate = odds, isPositive = (odds >= 0))


ggplot(data = df.odds[2:20,], aes(x = var_names, y = estimate, fill=isPositive)) + 
  geom_bar(stat="identity")

## cheking for model performance of Model.Train.glm.2

pred.glm <- predict(Model.Train.glm.2,Test.glm, type = "response")

pr = prediction(pred.glm,Test.glm$WonFlag)



NewPred <-ifelse(pred.glm >0.1050455,"yes","no")

rocCurve <- roc(response = Test.glm$WonFlag,  predictor = pred.glm,  levels = rev(levels(Test.glm$WonFlag))) 

plot(rocCurve)

performance <- as.data.frame(cbind(thresholds = rocCurve$thresholds,sensitivities = rocCurve$sensitivities,specificities= rocCurve$specificities))

head(performance %>% filter(sensitivities > 0.6 &  specificities > 0.6 & specificities < 0.8 & sensitivities < 0.8))

# thresholds sensitivities specificities
# 1  0.1045173     0.7998244     0.6355163

xtab <- table(Predicted = NewPred, Actual = Test.glm$WonFlag)

pf = performance(pr, measure = "acc", x.measure = "tpr")  

plot(pf) 

auc(rocCurve)

# Area under the curve: 0.7842

# parameter tuning using caret

library('doParallel')
cl <- makeCluster(4) #number of cores
registerDoParallel(cl)
set.seed(1)

tc <- trainControl("cv", 5, savePredictions=T,classProbs = TRUE)  #"cv" = cross-validation, 10-fold

glmtune <- train(Train.glm$WonFlag ~ .     ,
             data      = Train.glm    ,
             method    = "glmStepAIC"    ,
             metric = "ROC",
             family    = binomial ,
             verbose = FALSE,
             trControl = tc)



stopCluster(cl)

glmtune # To see results from running the glm
glmtune$finalModel # To observe the coefficients for the glm

## computing odds ratio

OR <- exp(glmtune$finalModel[[1]])

# (Intercept)          Used.Estab.WL.LastDay       Used.Emerging.WL.LastDay              Used.IAAS.LastDay 
# 0.02292667                     1.70993854                     2.62206734                     0.59071441 
# UsedFirstDay               DaysToActivation                FirstWeek.Usage                 LastWeek.Usage 
# 1.39268447                     1.00007097                     1.00343315                     1.00409848 
# ThirdWeek.Usage        HasUsed.80.pct.FreeCred    NormalizedUsage.Emerging.WL  NormalizedUsage.PAAS.AppServc 
# 1.00531434                     1.87253549                     1.00231808                     0.99899955 
# CountDaysOnWhichVIsitedWebPage   Content.Downloaded...Success            Inside.Sales.Member        Power.BI.Account.Member 
# 1.00588609                     0.86788186                     3.59224754                     2.60123712 
# EmailOpenRate              AgeOfLeadInDays.T 
# 7.26675629                     1.00122117


predictions<-predict(glmtune, newdata = Test.glm)
predictions # To obtain vector of results from model
confusionMatrix(predictions, Test.glm$WonFlag)


str(predictions)

glmProbs <- predict(glmtune, Test.glm, type = "prob") 
str(glmProbs)



rocCurve <- roc(response = Test.glm$WonFlag,  predictor = glmProbs[, "yes"],  levels = rev(levels(Test.glm$WonFlag))) 

plot(rocCurve)

performance <- as.data.frame(cbind(thresholds = rocCurve$thresholds,sensitivities = rocCurve$sensitivities,specificities= rocCurve$specificities))

head(performance %>% filter(sensitivities > 0.6 &  specificities > 0.6 & specificities < 0.8 & sensitivities < 0.8))

#thresholds sensitivities specificities
#1  0.1050455     0.7990759     0.6479437

NewPred <-ifelse(glmProbs$yes>0.1050455,"yes","no")

confusionMatrix(NewPred, Test.glm$WonFlag)


# 
# Reference
# Prediction    no   yes
# no  80073  4306
# yes 20134  7925
# 
# Accuracy : 0.7826         
# 95% CI : (0.7802, 0.785)
# No Information Rate : 0.8912         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.2851         
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.7991         
#             Specificity : 0.6479  

library(Epi) # To help us vizualize the apex of the ROC curve 

opt<-ROC(glmProbs[, "yes"], Test.glm$WonFlag ,plot = c("ROC"))

#We can see the apex quite clearly. 


#Next, using the same data, let's extract a different threshold optimizing for the F-measure

library(ROCR) # Used to calculate the F-measure
predROCR = prediction(glmProbs[, "yes"], Test.glm$WonFlag)
perfROCR = performance(predROCR,"f") #calculates the f measures across cutoffs

# Plot the output, add a red line at the maximum
plot(perfROCR) 
abline(v=PRFMAX,col="red")
# 0.3586712


prfmx<-max(perfROCR@y.values[[1]],na.rm=T) # extracts the max F measure
PRFMAX<-min(perfROCR@x.values[[1]][perfROCR@y.values[[1]]==prfmx],na.rm=T)
print(PRFMAX)

filter(performance, thresholds > 0.3585 & thresholds < 0.3588)
# thresholds sensitivities specificities
#   0.3586739     0.9695231     0.2697245

###########building a decision tree for better interpretability with 117 variables#####################################################

data = ModelPopulation.Subset.1




############# plot partial plots for these top 20 most important variables ############




############ selecting all 500 variables and checking for correlations ##############


###########building a decision tree #####################################################






######################### vriable selection using recursive feature elimination ############################
# 
# subsets <- c(1:5, 10, 15, 20, 25)
# 
# set.seed(10)
# 
# ctrl <- rfeControl(functions = lmFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# 
# lmProfile <- rfe(x, y,
#                  sizes = subsets,
#                  rfeControl = ctrl)
# 
# lmProfile




############# selecting variables important in the decision tree#######################



############# tune GBM and random forest with only important vraibles############################







############ build GLM moidel with important variabels that are not correlated #####################





