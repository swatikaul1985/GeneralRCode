# 
# Written by:
# -- 
# John L. Weatherwax                2007-07-05
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

###install.packages('gbm')
library(gbm)

set.seed(0)

source('gen_eq_10_2_data.R')

p = 10 # the dimension of the feature vector (a "p" dimensional vector)
N_train = 2000 # number samples to use in training
N_test = 10000 # number of samples to use in testing 

# Extract the data we will to classify: 
D_train = gen_eq_10_2_data(N=N_train,p=p)
D_train[ D_train$Y==-1, p+1 ] = 0 # Map the response "-1" to the value of "0" (required format for the call to gbm): 

# Generate the formula used to fit our model with: 
#
terms = paste( colnames(D_train)[1:p], collapse="+" ) # dont consider the last column (the response variable)
formula = formula( paste( colnames(D_train)[p+1], " ~ ", terms ) )

# Do training with the maximum number of trees: 
#
n_trees = 400
if( T ){
  print( "Running Adaboost ..." )
  m = gbm( formula, data=D_train, distribution='adaboost', n.trees=n_trees, shrinkage=1, verbose=TRUE )
}else{
  print( "Running Bernoulli Boosting ..." )
  m = gbm( formula, data=D_train, distribution='bernoulli', n.trees=n_trees, verbose=TRUE )
}

# Lets plot the training error as a function of the number of trees:
#
# Note that when using the "bernoulli" output to compute the output of predict into a probability one needs to use:
# plogis( predict( m, D_train[,1:p], n.trees=nti ) ) 
#
training_error = matrix( 0, nrow=n_trees, ncol=1 )
for( nti in seq(1,n_trees) ){
  Fhat = predict( m, D_train[,1:p], n.trees=nti )
  pcc = mean( ( ( Fhat <= 0 ) & ( D_train[,p+1] == 0 ) ) | ( ( Fhat > 0 ) & ( D_train[,p+1] == 1 ) ) )
  training_error[nti] = 1 - pcc
}

# Lets plot the testing error as a function of the number of trees:
#
D_test = gen_eq_10_2_data(N=N_test,p=p)
D_test[ D_test$Y==-1, p+1 ] = 0 # Map the response "-1" to the value of "0" (required format for the call to gbm): 

test_error = matrix( 0, nrow=n_trees, ncol=1 )
for( nti in seq(1,n_trees) ){
  Fhat = predict( m, D_test[,1:p], n.trees=nti )
  pcc = mean( ( ( Fhat <= 0 ) & ( D_test[,p+1] == 0 ) ) | ( ( Fhat > 0 ) & ( D_test[,p+1] == 1 ) ) )
  test_error[nti] = 1 - pcc 
}


#postscript("../../WriteUp/Graphics/Chapter10/dup_fig_10_2.eps", onefile=FALSE, horizontal=FALSE)

plot( seq(1,n_trees), training_error, type="l", main="Boosting Probability of Error", col="red", xlab="number of boosting iterations", ylab="classification error" )
lines( seq(1,n_trees), test_error, type="l", col="green" )

legend( 275, 0.45, c("training error", "testing error"), col=c("red", "green"), lty=c(1,1) )

#dev.off()


