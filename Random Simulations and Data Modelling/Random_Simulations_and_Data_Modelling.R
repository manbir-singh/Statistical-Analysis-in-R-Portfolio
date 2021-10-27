### Question 3 ###
## 3a) Random Simulations ##
set.seed(1000660251)
n <- 500
X1 <- runif(n, 0, 1)
X2 <- runif(n, 0, 1)
X3 <- runif(n, 0, 1)
X4 <- runif(n, 0, 1)
X5 <- runif(n, 0, 1)
fX <- 4*(sin(pi*X1*X2) + 8*(X3 - 0.5)^3 + 1.5*X4 - X5 - 0.77)
pX <- (exp(fX))/(1 + exp(fX))
Y <- rbinom(n, 1, pX)

## 3b) Logistic Regression ##
library(nnet)
library(pROC)
df <- c(list(X1, X2, X3, X4, X5))
#df
multi.mod <- multinom(Y ~ X1 + X2 + X3 + X4 + X5, data = df)
summary(multi.mod)
logit.mod <- glm(Y ~ X1 + X2 + X3 + X4 + X5, family = binomial(link = logit), data = df)
summary(logit.mod)

# ROC Curve #
library(pROC)
pred <- predict(logit.mod, type = "response")
roc_logit <- roc(Y ~ pred)
## The True Positive Rate ##
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
pdf("ROC_LOG_REG.pdf")
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red', main = "Logistic Model Predictors vs Outcome (3b)")
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))
dev.off()
auc(roc_logit)
# We see that the AUC is 0.8888 indicating the model can discriminate between true 
# positive rate and a false positive rate 88% of the time.
# From the coefficients we see that for a one point increase in X1, X2, X3, X4
# there is an increase in the difference of the log odds.
# While there is a decrease in the difference in the log odds of 3.3904 for an increase in X5

## 3c) Linear Transformation Predictor ##
new_X1 <- 4*(sin(pi*X1*X2))
new_X2 <- 32*(X3 - 0.5)^3
new_X3 <- 4*(1.5*X4)
new_X4 <- 4*(-X5 - 0.77)

transformed.logit.mod <- glm(Y ~ new_X1 + new_X2 + new_X3 + new_X4,
                             family = binomial(link = logit), data = df)
summary(transformed.logit.mod)

# ROC Curve #
library(pROC)
transformed.pred <- predict(transformed.logit.mod, type = "response")
roc_logit.transformed <- roc(Y ~ transformed.pred)
## The True Positive Rate ##
TPR.transformed <- roc_logit.transformed$sensitivities
## The False Positive Rate ##
FPR.transformed <- 1 - roc_logit.transformed$specificities
pdf("ROC_LOG_REG_TRANSFORMED.pdf")
plot(FPR.transformed, TPR.transformed, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red', 
     main = "Linearly Transformed Predictor vs Outcome (3C)")
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit.transformed),2)))
dev.off()
auc(roc_logit.transformed)
# Here we see that the AUC is 0.9105,indicating that this model can discriminate
# between a true positive rate from a false positive rate 91% of the time.
# All four newly transformed variables provide an increase by some value (different for each
# transformed predictor) in the difference in the log odds with a one point increase in their
# respective transformed predictor value.

## 3c) Interpretation ##
# First we see that the logit.mod (from 3b) has a higher AIC value (429.68) compared
# to the transformed.logit.mod (in 3c) with an AIC value of 387.23.
# This indicates that the transformed.logit.mod has a better goodness of fit.

# Secondly, the coefficents in the transformed model(3c) are smaller absolute values
# than the logistic regression(3b).
# and there is 1 negative coefficent in the transformed model, whereas there
# are 2 in the logistic regression.
# This could suggest that the transformed model has more accurate odds given the
# very large and extreme odds found in the logistic regression coefficients.

# Thirdly, the AUC for the transformed model is 0.9105 and the AUC for the logistic
# regression model is 0.8888.
# This suggests the transformed model is much better at discriminating TPR from
# FPR compared to the logistic model.

# All in all, these findings indicate that the transformed model is a better
# model for predicting compared to the logistic model in 3b.
# This could be due to the fact that the new_X variables(3c) that were linearly
# transformed to match the fX function,
# which was used in the simulating Y, the outcome variable, better
# than the X variables in 3b.
# In other words the linearly transformed variables were better at simulating
# values that Y was also producing.
# This lead to better overall prediction due to lower AIC score,
# goodness of fit and stronger coefficients and AUC.