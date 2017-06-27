#df = dataframe, DV is the label column name, IV is list of column names for IVs
# df is dataset with all independent variables adnone DV whichis categorical called lable
# label is string for column name of DV

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

