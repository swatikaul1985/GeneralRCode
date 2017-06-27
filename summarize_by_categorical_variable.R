# function to summarize data

summary_cluster <- function(df,output.range,output.quantile) { #output.range is tring type for nam eof output csv file
  
  pt25 = function(x) {return (quantile(x, 0.25,na.rm = T))}
  pt75 = function(x) {return (quantile(x, 0.75,na.rm = T))}
  mean_ts = function(x) {return (mean(x, na.rm = T))}
  min_ts = function(x) {return (min(x, na.rm = T))}
  max_ts = function(x) {return (max(x, na.rm = T))}
  
  final.data <- df
  
  summary_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise_each(funs(pt25, median, pt75))
  
  median_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise_each(funs(median)) 
  
  names(median_cluster) <- c("ClusterID",sapply(names(median_cluster)[-c(1)],  function(x) {return(paste0(x,"_median"))}))
  
  count_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise(count = n()) 
  
  
  
  mean_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise_each(funs(mean_ts)) 
  
  names(mean_cluster) <- c("ClusterID",sapply(names(mean_cluster)[-c(1)],  function(x) {return(paste0(x,"_mean"))}))
  
  
  min_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise_each(funs(min_ts)) 
  
  names(min_cluster) <- c("ClusterID",sapply(names(min_cluster)[-c(1)],  function(x) {return(paste0(x,"_min"))}))
  
  max_cluster <- final.data %>% 
    group_by(ClusterID) %>% 
    summarise_each(funs(max_ts)) 
  
  names(max_cluster) <- c("ClusterID",sapply(names(max_cluster)[-c(1)],  function(x) {return(paste0(x,"_max"))}))
  
  summary_cluster.final.median <- inner_join(median_cluster,count_cluster)
  
  summary_cluster.final.mean <- inner_join(median_cluster,mean_cluster)
  
  summary_cluster.final.range <- inner_join(summary_cluster.final.mean,min_cluster)
  
  summary_cluster.final.range <- inner_join(summary_cluster.final.range,max_cluster)
  
  summary_cluster.final.range <- inner_join(summary_cluster.final.range,count_cluster)
  
  write.csv(summary_cluster.final.range,paste0(output.range,".csv"))
  
  summary_cluster.final.quantile <- inner_join(summary_cluster,count_cluster)
  
  write.csv(summary_cluster.final.quantile,paste0(output.quantile,".csv"))
  
}