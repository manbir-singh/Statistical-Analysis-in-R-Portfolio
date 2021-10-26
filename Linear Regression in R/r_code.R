library(readr)
dataxy <- read.table("dataxy.txt", header = TRUE)
View(dataxy)
## Dataset ##
dataxy


## Plotting the data ##
x <- dataxy$x
y <- dataxy$y

plot(x, y, main = "X vs Y",
     xlab = "X", ylab = "Y")

## Creating Linear Regression Model and plotting ##
linreg <- lm(y ~ x)
abline(linreg, col = "red")
summary(linreg)
## The linear model appears to have a moderately strong relation with b1 being 0.875. It has a positive direction indicating positive relation and is of linear form ##

## plotting the residual ##
dataxy$residuals <- residuals.lm(linreg)
dataxy$residuals

## residual vs x ##
plot(x, dataxy$residuals, main = "Residuals vs X", xlab = "X", ylab = "Residuals")
## The residuals against x have most of their points around Residual = 0 indicating that the variance of the residuals is less ##

## Scatterplot of y vs x ##
plot(y, x, main = 'Y vs X', xlab = "y", ylab = "x")
## Likewise the if a line were to be fitted for y against x we would see strong strength given the close clusters of data points ##

## Brown-Forsythe Test ##

n1 = x[1:25]
n1
n2 = x[26:50]
n2
## i) e1 and e2 ##
e1bar <- median(dataxy$residuals[1:25])
e1bar
e2bar <- median(dataxy$residuals[26:50])
e2bar

## ii) d1 and d2 ##
di1 <- c()
for(i in dataxy$residuals[1:25]){
  di1[[i]] <- abs(i - e1bar)
  return(di1)
}
print(di1)

di2 <- c()
 for (i in dataxy$residuals[26:50]){
   di2[[i]] <- abs(i - e2bar)
   return(di2)
 }
print(di2)
 
## iii) T- test statistic BF ##
