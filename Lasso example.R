#http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

library(glmnet)

age <- c(4,8,7,12,6,9,10,14,7) 
gender <- c(1,0,1,1,1,0,1,0,0) ; gender<-as.factor(gender)
bmi_p <- c(0.86,0.45,0.99,0.84,0.85,0.67,0.91,0.29,0.88) 
m_edu <- c(0,1,1,2,2,3,2,0,1); m_edu<-as.factor(m_edu)
p_edu <- c(0,2,2,2,2,3,2,0,0); p_edu<-as.factor(p_edu)
f_color <- c("blue", "blue", "yellow", "red", "red", "yellow", "yellow", "red", "yellow")
asthma <- c(1,1,0,1,0,0,0,1,1)

f_color <- as.factor(f_color)
xfactors <- model.matrix(asthma ~ gender + m_edu + p_edu + f_color)[,-1]
x <- as.matrix(data.frame(age, bmi_p, xfactors))

#note alpha =1 for lasso only and can blend with ridge penalty down to alpha=0 ridge only
glmmod<-glmnet(x,y=as.factor(asthma),alpha=1,family='binomial')

#plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod,xvar="lambda")
grid()


# some results

#model shown for lambda up to first 3 selected variables. Lambda can have manual
# tuning grid for wider range
 glmmod

# Call:  glmnet(x = x, y = as.factor(asthma), family = "binomial", alpha = 1) 

# Df    %Dev   Lambda
# [1,]  0 0.00000 0.273300
# [2,]  1 0.01955 0.260900
# [3,]  1 0.03737 0.249000
# [4,]  1 0.05362 0.237700
# [5,]  1 0.06847 0.226900
# [6,]  1 0.08204 0.216600
# [7,]  1 0.09445 0.206700
# [8,]  1 0.10580 0.197300
# [9,]  1 0.11620 0.188400
# [10,]  3 0.13120 0.179800
# [11,]  3 0.15390 0.171600
#coefficents can be extracted from the glmmod. Here shown with 3 variables selected.
 coef(glmmod)[,10]
# (Intercept)           age         bmi_p       gender1        m_edu1        m_edu2        m_edu3        p_edu2        p_edu3 
# 0.59445647    0.00000000    0.00000000   -0.01893607    0.00000000    0.00000000    0.00000000   -0.01882883    0.00000000 
# f_colorred f_coloryellow 
# 0.00000000   -0.77207831 

cv.glmmod <- cv.glmnet(x,y=asthma,alpha=1)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min