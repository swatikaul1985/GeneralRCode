#data = data frame or table containing response and variable of interest
#y = name of response variable (string), the variable must be numeric
#x = name of independent variable
#numbins = how many bins to bin the histogram by
#countordered = 1 means sorted by frequency, 0 means sorted by response variable


# example:
#univariate(data, "variable1", "Original_Quote_Date", 30, 1)

univariate = function(data, y, x, numbins = 30, countordered = 1){
  require(ggplot2)
  require(dplyr)
  require(gridExtra)
  
  #cat("x: ",x,"\n")
  
  #trim dataset to only relevant columns
  d = data.frame(x=data[[x]], y=data[[y]])
  
  #get the type of x
  vartype = ifelse(is.numeric(d$x), 'numeric',
                   ifelse(is.factor(d$x), 'factor', 'character'))
  coord_cartesian(ylim = c(0,10000))
  #summarise y
  if(vartype %in% c('factor', 'character')){
    
    if(vartype == 'character'){d$x = as.factor(d$x)}
    s = summarise(group_by(d, x), 
                  m = mean(y),
                  count = n())  ############ Detach plyr and load plyr to run n()
    if (countordered == 1){
      p1 = ggplot(data=s, aes(x=reorder(x, -count), y=m , colour = "blue"))
      p2 = ggplot(data=s, aes(x=reorder(x, -count), y=count , colour ="blue"))
    }else {
      p1 = ggplot(data=s, aes(x=reorder(x, -m), y=m , colour = "blue"))
      p2 = ggplot(data=s, aes(x=reorder(x, -m), y=count ,colour ="blue"))
    }
    
  }else if(vartype == 'numeric'){
    #bin variables first
    num_values = length(unique(d$x))
    if(num_values > 0){num_values = numbins}
    h = hist(d$x, breaks = num_values, plot=F, include.lowest=T)
    bin_numbers = .bincode(d$x, breaks=h$breaks, include.lowest=T)
    d$x = as.factor(sapply(bin_numbers, function(x){return(h$mids[x])}))
    s = summarise(group_by(d, x), 
                  m = mean(y),
                  count = n())
    p1 = ggplot(data=s, aes(x=x, y=m),colour = "black")
    p2 = ggplot(data=s, aes(x=x, y=count),colour = "black")
  }
  print(s)
  
  #plot the summarised data
  p1 = p1 + 
    geom_line(aes(group=1), colour='black', size=0.5) + 
    geom_point(size=3, colour = "black") + 
    scale_y_continuous(name=paste("mean", y)) +
    scale_x_discrete(name=x) + 
    theme(panel.background = element_rect('#dcdde0'),
          plot.title = element_text(size=11), 
          axis.title.x = element_text(size=12), 
          axis.title.y = element_text(size=12), 
          axis.text.x  = element_text(size=12, colour="#000000", face="plain"), 
          axis.text.y  = element_text(size=12, colour="#000000", face="plain"))
  p2 = p2 + 
    geom_bar(stat="identity") + 
    scale_y_continuous(name='Count') +
    scale_x_discrete(name=x) + 
    theme( panel.background = element_rect('#dcdde0'),
          plot.title = element_text(size=11), 
          axis.title.x = element_text(size=12), 
          axis.title.y = element_text(size=12), 
          axis.text.x  = element_text(size=12, colour="#000000", face="plain"), 
          axis.text.y  = element_text(size=12, colour="#000000", face="plain"))
  grid.arrange(p1, p2, ncol=1)
  
  return (h)
}

