


Score_model <- function(df,model,Dim.level,month, feature.list, feature.list.scale = feature.list,pct) { # df is an object as a result of function data_pull
# 
#   require(RODBC,lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(dplyr, lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(data.table, lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(lubridate,lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(sqldf,lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(foreach,lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(doSNOW,lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(parallel, lib.loc="C:/Program Files/R/R-3.2.2/library")
#   require(doParallel, lib.loc="~/R/win-library/3.2")

 final.model.df <- df[[1]]
  
  #final.model.df <- df
  
  #Scaling the customers
  
  Usage.Data.v1 <- final.model.df %>% select( c(one_of(feature.list)))
  
#   
#  Usage.Data.v2 =  Usage.Data.v1 %>%
#    mutate_each(funs(scale))  
# 

  


 Usage.Data.v1.subset <- Usage.Data.v1 %>% select(c(one_of(feature.list.scale)))

source("scaling function.R")

Subset_Usage.Data.v1_scaled <- Modified_scaling(Usage.Data.v1.subset ,pct)

Scaled_data <- Subset_Usage.Data.v1_scaled

Usage.Data.v2 <- Scaled_data
	
  
  
  
  
  cap <- function(x) {return(ifelse(abs(x)>3,ifelse(x>3,3,-3),x))}
  
  
  Usage.Data.v3 <- as.data.frame(apply(Usage.Data.v2,2,cap))
  

  
mod2 <- model

tcenters = t(mod2$centers)

dist <- function(tcenters,dfrow) {
  index <- 0
  for (i in 1:ncol(tcenters)) {
    
    
    index[i] <- (sum((dfrow - tcenters[,i])^2))
    
  }
  return(as.integer(which.min(index)))
}



Usage.Data.v3$ClusterId  <- apply(Usage.Data.v3, 1, function(x) dist(tcenters, x)) 




final_cluster_map_customer <- as.data.frame(cbind(final.model.df , ClusterId = Usage.Data.v3$ClusterId))


final.data <- final_cluster_map_customer %>% select(one_of(c("ClusterId",feature.list)))

#final.data$ServiceCount <- as.numeric(final.data$ServiceCount)
#final.data$DBCount <- as.numeric(final.data$DBCount)
# final.data$Logincount <- as.numeric(final.data$Logincount)

final.data$ClusterId <- as.factor(final.data$ClusterId)


Acct.cust.mapping <- df[[2]]

Sub.Cust.mapping.acct <- Acct.cust.mapping %>%  distinct(CustomerID,BillableAcctId) %>% select(CustomerID,BillableAcctId)


final.data.acct  <- left_join(final_cluster_map_customer,Sub.Cust.mapping.acct, by = c("Customer" = "CustomerID"))




pt25 = function(x) {return (quantile(x, 0.25))}
pt75 = function(x) {return (quantile(x, 0.75))}


summary_cluster <- final.data %>% 
  group_by(ClusterId) %>% 
  summarise_each(funs(pt25, median, pt75))

median_cluster <- final.data %>% 
  group_by(ClusterId) %>% 
  summarise_each(funs(median)) 

count_cluster <- final.data %>% 
  group_by(ClusterId) %>% 
  summarise(count = n()) 



summary_cluster.final.median <- inner_join(median_cluster,count_cluster)

summary_cluster.final.quantile <- inner_join(summary_cluster,count_cluster)




final.object <- list(final.data,final.data.acct,summary_cluster.final.median,summary_cluster.final.quantile)

return(final.object)

}

