install.packages('datasets')
install.packages('tidyverse')
install.packages('dplyr')

rm(list = ls())
set.seed(1000660251)

### Question 2 ###
library(datasets)
data(warpbreaks)
head(warpbreaks)

## 2a) Poisson Regression ##
summary(glm(breaks ~ wool + tension, family = poisson, data = warpbreaks))
# Our mean value would be exp(intercept) = exp(3.69196)

# For woolB this is the estimate that for one increase point while other variables remain constant. 
# For an increase in woolB by one point, the difference in the logs of expected counts would be
# expected to decrease by 0.20599 units, while holding the other variables in the model constant.
# woolB's p value is much smaller than an alpha level of 0.05, therefore we reject the null hypothesis
# and woolB is statistically significant.

# For tensionM its estimate for one point increase would be the difference in the logs of expected
# counts and it would decrease by 0.32132, while holding other variables constant.
# its p value < 0.05 therefore we reject the null hypothesis that tensionM has no effect on wool breaks
# and tensionM is statistically significant.

# For tensionH its estimate for one point increase would be the difference in the logs of expected 
# counts and it would decrease by 0.051849, when other variables are held constant.
# tensionH's pvalue < 0.05, therefore we reject the null hypothesis and tensionH is statistically significant.


## 2b) Negative Binomial Regression ##
library(MASS)
summary(glm.nb(breaks ~ wool + tension, data = warpbreaks))

# the mean for this model is the dispersion parameter of 9.94
# for one increase in woolB there is a decrease in the difference in the logs of the
# expected counts by 0.1862.
# woolB's pvalue > alpha = 0.05 therefore we fail to reject the null hypothesis

# for one increase in tensionM there is a decrease in the diff in the logs of the expected
# counts by 0.2992.
# tensionM's pvalue < alpha = 0.05 therefore we reject the null hypothesis and tensionM is
# statistically significant.

# for one increase in tensionH there is a decrease in the diff in the logs of the expected
# counts by 0.5114.
# tensionH's pvalue <<<< alpha=0.05 therefore we reject the null hypothesis and tensionH
# is statistically significant

## 2c) Model Comparison ##
# The AIC for the poisson regression is 493.06, whereas for the negative
# binomial regeression it is 408.76. 
# Therefore the negative binomial regression is a better fit given the
# smaller AIC value.
# For neg. bin. the dispersion factor is: 1/k = 1/theta = 1/9.94
# This value is not close to zero.
# Therefore we cannot use the poisson regression model since it would lead to
# overdispersion. Negative binomaial regression model is a better model to use.
