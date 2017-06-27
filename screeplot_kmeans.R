# Function to implement the scree plot
plotParallelScree = 
  function (df, th, seedvector, prefix = ""){
    # Setting the up the cores
    cl = makeCluster(th)
    registerDoSNOW(cl)
    cat(paste(getDoParWorkers(),": Threads intiated!"))
    x = seedvector
    wss0 <- (nrow(df)-1)*sum(apply(df,2,var))  
    wss <- 
      foreach(j=1:length(x), .combine='cbind') %:%
      foreach(i=2:18, .combine = "c") %dopar% {
        set.seed(x[j]);  sum(kmeans(df, centers=i, iter.max = 65532)$withinss)
      }
    for(j in 1:length(x)){ 
      a = paste("Elbow Curve - Kmeans (seed = ",x[j],")", sep = "")
      png(file = paste(prefix,"Cluster_seed",x[j] ,"_oct_nov_dec.png",sep = ""), height=630, width = 864)
      plot(1:18, c(wss0,wss[,j]), main = a, type="b", xlab="Number of Clusters", ylab="Average within - cluster sum of squares",col = "royalblue")                                                                                                                                                                                               
      dev.off()
    } 
    stopCluster(cl)
    print(paste("Knee elbow plots created successfully at",getwd()))
    return (0)
  } 
