#====Question 11.1====

install.packages("caret")
library(caret)

#load data
uscrime <- read.table('uscrime.txt', header=TRUE)
head(uscrime)

set.seed(123)
#split data into training and test (70% and 30% respectively)
trainIndex <- createDataPartition(y= uscrime$Crime, p=0.7, list=FALSE)
train_data_raw <- uscrime[trainIndex,]
test_data_raw <- uscrime[-trainIndex,]

#prepare training set
pred.train.raw <- as.matrix(train_data_raw[, -which(names(train_data_raw) == "Crime")])
dv.train <- train_data_raw$Crime

#prepare test set
pred.test.raw <- as.matrix(test_data_raw[, -which(names(test_data_raw) == "Crime")])
dv.test <- test_data_raw$Crime

#scale predictors
preProc_obj <- preProcess(pred.train.raw, method = c("center", "scale"))
pred.train.scaled <- predict(preProc_obj, pred.train.raw)
pred.test.scaled <- predict(preProc_obj, pred.test.raw)



#----stepwise regression ====
stepwise.model <- lm (Crime ~., data = uscrime)
stepwise.model <- step(stepwise.model, direction = "both")
summary(stepwise.model)




#----LASSO====
install.packages('glmnet')
library(glmnet)

lasso_model_cv <- cv.glmnet(pred.train.scaled, dv.train,
                            type.measure = "mse",
                            alpha = 1,
                            family = "gaussian")

plot(lasso_model_cv)
title("Lasso Regression: CV Curve", line = 2.5)

print(lasso_model_cv)
#find optimal lambda value that minimizes mean squared error
best_lambda <- lasso_model_cv$lambda.min
best_lambda

#best model with optimal lambda value
best.lasso <- glmnet(x,y, alpha = 1, lambda = best_lambda)
coef(best.lasso)

lambda_1se_lasso <- lasso_model_cv$lambda.1se

#make predictions on test set
predictions_lasso <- predict(lasso_model_cv, s = lambda_1se_lasso, newx = pred.test.scaled)

#evaluate performance on test set
mse_lasso_test <- mean((predictions_lasso - dv.test)^2)
mse_lasso_test



#---Elastic Net====

alpha_values <- c(0.1, 0.5, 0.9)

for (alpha_val in alpha_values) {
  cat(paste("\nTraining Elastic Net with alpha =", alpha_val, "\n"))
  
  elastic_net_model_cv <- cv.glmnet(x = pred.train.scaled, y = dv.train,
                                    family = "gaussian", alpha = alpha_val, nfolds = 10)
  
  plot(elastic_net_model_cv)
  title(main = paste("Cross-Validation for Elastic Net (alpha =", alpha_val, ")"), line = 2.5)
  
  best_lambda_elastic <- elastic_net_model_cv$lambda.min
  cat(paste("Best lambda for alpha =", alpha_val, ":", best_lambda_elastic, "\n"))
  
  final_elastic_net_model <- glmnet(x = pred.train.scaled, y = dv.train,
                                    family = "gaussian", alpha = alpha_val, lambda = best_lambda_elastic)
  
  cat(paste("Coefficients for alpha =", alpha_val, ":\n"))
  print(coef(final_elastic_net_model))
  
  predictions_elastic <- predict(final_elastic_net_model, newx = pred.test.scaled)
  
  mse_elastic <- mean((dv.test - predictions_elastic)^2)
  cat(paste("Mean Squared Error on test set for alpha =", alpha_val, ":", mse_elastic, "\n"))
}


#====Question 12.2====




