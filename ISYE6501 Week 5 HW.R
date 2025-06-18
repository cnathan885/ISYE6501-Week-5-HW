#-----Question 11.1-----

uscrime <- read.table('uscrime.txt', header=TRUE)
head(uscrime)

#stepwise regression
stepwise.model <- lm (Crime ~., data = uscrime)
stepwise.model <- step(stepwise.model, direction = "both")
summary(stepwise.model)



#LASSO
install.packages('glmnet')
library(glmnet)

#define response variable
y <- uscrime$Crime

#define predictor variables in matrix
x.unscaled <- as.matrix(uscrime[,1:15])
x <- scale(x.unscaled)

#fit lasso regression model
lasso.model <- cv.glmnet(x,y, alpha=1, standardize=FALSE)

plot(lasso.model)
title("Lasso Regression Cross-Validation Plot", line = 3)

#find optimal lambda value that minimizes mean squared error
best.lambda <- lasso.model$lambda.min
best.lambda

#best model
best.lasso <- glmnet(x,y, alpha = 1, lambda = best.lambda)
coef(best.lasso)

