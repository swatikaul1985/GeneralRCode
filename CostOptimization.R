library(Cairo)
Cairo(file="optim.png", 
      type="png",
      units="in", 
      width=12, 
      height=8, 
      pointsize=12, 
      dpi=500)
rocData <- data.frame(converted=trueLabel, pred=PropensityScore) 

roc <- calculate_roc(rocData, cost_of_fp=1, cost_of_fn=20,gain_of_tp=50,gain_of_tn=0, n = 500)

plot_roc(roc, threshold= roc[which.max(roc$ROI),1], 1, 20,50,0)

dev.off()






calculate_roc <- function(df, cost_of_fp, cost_of_fn,gain_of_tp,gain_of_tn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$converted == 1) / sum(df$converted == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$converted == 0) / sum(df$converted == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$converted == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$converted == 1) * cost_of_fn
  }
  
  gain <- function(df, threshold, gain_of_tp, gain_of_tn) {
    sum(df$pred >= threshold & df$converted == 1) * gain_of_tp + 
      sum(df$pred < threshold & df$converted == 0) * gain_of_tn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  roc$gain <- sapply(roc$threshold, function(th) gain(df, th, gain_of_tp, gain_of_tn))
  
  roc$ROI <- roc$gain -roc$cost
  
  
  return(roc)
}


plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn,gain_of_tp,gain_of_tn) {
  library(gridExtra)
  library(grid)
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  col_ramp <- colorRampPalette(rev(c("green","orange","red","black")))(100)
  col_by_gain <- col_ramp[ceiling(norm_vec(roc$gain)*99)+1]
  col_by_ROI <- col_ramp[ceiling(norm_vec(roc$ROI)*99)+1]
  
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  p_gain <- ggplot(roc, aes(threshold, gain)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_gain, size=4, alpha=0.5) +
    labs(title = sprintf("gain function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  
  p_ROI <- ggplot(roc, aes(threshold, ROI)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_ROI, size=4, alpha=0.5) +
    labs(title = sprintf("ROI function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  
  sub_title <- sprintf("threshold at %.2f : \n cost of FP = %d, \n cost of FN = %d, \n gain of TP = %d,  \n gain of TN = %d, \n ROI = %d", threshold, cost_of_fp, cost_of_fn,gain_of_tp, gain_of_tn, max(roc$ROI))
  
  grid.arrange(p_roc, sub=textGrob(sub_title, gp=gpar(cex=1), just="center"),p_ROI, p_cost, p_gain, layout_matrix=matrix(c(1,2,3,3,4,4,5,5),ncol=4,byrow=T))
}
