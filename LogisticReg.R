library(ISLR)
library(lmerTest)

data = ISLR ::Smarket
train_data <- data[data$Year != "2003",]
train_data
logistic_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = train_data, family = "binomial")
summary(logistic_model)

test_data_x <- data[data$Year == "2003", -c(1,7,9)]
test_data_x
test_y <- data[data$Year == "2003",9]
test_y
predictions <- predict(logistic_model, test_data_x, type = "response")
glm_pred = rep("Down", 252)
#Assigning all predictions greater than 0.5 to Up. 
glm_pred[predictions>0.5] = "Up"
#Confusion Matrix- calculating the accuracy 
#of your predictions with set threshold.
table(glm_pred, test_y)
plot(predictions)


data = ISLR ::Default

default_mod <- glm(default~., data = data, family = "binomial")
summary(default_mod)
anova(default_mod)
library(car)
Anova(default_mod, type = 2)


#Provides odds ratio after exponential operation
default_mod$coefficients
beta_1 <- coef(default_mod)[2]
beta_1
exp(beta_1)

prob = predict(default_mod, type = "response")
data$prob = prob

#To plot roc curve (sensitivity ~ 1 - specificity)
#for true positive rate over false positive rate. 
library(pROC)

roccurve <- roc(default ~ prob, data = data)
roccurve
plot(roccurve, xlim = c(.5,0))


res.deviance = default_mod$deviance
df <- default_mod$df.residual
p = pchisq(res.deviance,df,lower.tail = F)
cbind(res.deviance, df, p)
