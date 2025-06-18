#-----Question 11.1-----

uscrime <- read.table('uscrime.txt', header=TRUE)
head(uscrime)

#stepwise regression
stepwise.model <- lm (Crime ~., data = uscrime)
stepwise.model <- step(stepwise.model, direction = "both")
summary(stepwise.model)

