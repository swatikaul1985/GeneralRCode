
#This sample data will be used for the examples below:
set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
# View first few rows
head(dat)
#>   cond     rating
#> 1    A -1.2070657
#> 2    A  0.2774292
#> 3    A  1.0844412
#> 4    A -2.3456977
#> 5    A  0.4291247
#> 6    A  0.5060559

library(ggplot2)

## Basic histogram from the vector "rating". Each bin is .5 wide.
## These both result in the same output:
ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5)
# qplot(dat$rating, binwidth=.5)

# Draw with black outline, white fill
ggplot(dat, aes(x=rating)) +
  geom_histogram(binwidth=.5, colour="red", fill="white")

# Density curve
ggplot(dat, aes(x=rating)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(dat, aes(x=rating)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

#Add a line for the mean:

ggplot(dat, aes(x=rating)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)


#Histogram and density plots with multiple groups
# Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")



# Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, position="dodge")

# Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

# Density plots with semi-transparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)


#Add lines for each mean requires first creating a separate data frame with the means:


# Find the mean of each group
library(plyr)
cdat <- ddply(dat, "cond", summarise, rating.mean=mean(rating))
cdat
#>   cond rating.mean
#> 1    A -0.05775928
#> 2    B  0.87324927

# Overlaid histograms with means
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=cond),
             linetype="dashed", size=1)

# Density plots with means
ggplot(dat, aes(x=rating, colour=cond)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=cond),
             linetype="dashed", size=1)

#Using facets:

ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(cond ~ .)

# With mean lines, using cdat from above
ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(cond ~ .) +
  geom_vline(data=cdat, aes(xintercept=rating.mean),
             linetype="dashed", size=1, colour="red")


# univariate

 x <- c(seq(1:200))
 sample.dat_b <- sample(x,20)
 dat_b <- dat[dat$cond == "B",]
 sample.dat_b <- dat_b[sample(x,20),]
 dat_a <- dat[dat$cond == "A",]
 dat.new <- rbind(dat_a,sample.dat_b)

 #Histogram and density plots with multiple groups
 # Overlaid histograms
 ggplot(dat.new, aes(x=rating, fill=cond)) +
   geom_histogram(binwidth=.5, alpha=.5, position="identity")

hist.plot <- hist(dat.new$rating,breaks = c(seq(-3,3.5,0.25)))

break.points <- hist.plot$breaks

dat.new$Ratingcat1<-cut(dat.new$rating, break.points)


pos1 = regexpr(',', as.character(dat.new$Ratingcat1))
pos2 = regexpr(']', as.character(dat.new$Ratingcat1))

dat.new$xlab <- as.factor(substring(dat.new$Ratingcat1,pos1+1,pos2-1))

levels(dat.new$cond) <- c(0,1)

dat.new$cond <- as.numeric(dat.new$cond) -1

prop <- dat.new %>% group_by(xlab) %>% summarise(prop_cond = mean(cond))

;

p1<- plot(as.numeric(prop$xlab), prop$prop_cond, type="l")
p2 <-  hist(dat.new$rating,breaks = c(seq(-3,3.5,0.25)))

par(mfrow=c(2,1))
plot(as.numeric(prop$xlab), prop$prop_cond, type="l")

grid.arrange(p1, p2, ncol=1)

####################################################################################



a <- table(dat.new$Ratingcat1,dat.new$cond)
b <- table(dat.new$Ratingcat1)

data <- as.data.frame(cbind(a[,1],a[,2],b,ratio = (a[,2]/b)))

data <- as.data.frame(cbind(a[,1],a[,2],b,ratio = (a[,2]/b)))

ggplot(dat, aes(x=rating)) +
  geom_histogram(binwidth=.5, colour="red", fill="white") +
  geom_line(data = data$ratio)