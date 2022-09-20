# Get the working directory
getwd()
# Change the working directory
setwd("/Users/Tonkos/OneDrive/Documents/ECON124/Homework 2")
install.packages("gamlr")
library(gamlr)

# Question 1
data <- read.csv("NBA.csv")
data$Tm <- factor(data$Tm)

# Question 2
is.na(data)
data_omit <- na.omit(data)

# Question 3
hist(data_omit$Salary,  breaks = 15, main = "Histogram Player Salary", xlab = "Salary", ylab = "Players")
abline(v = mean(data_omit$Salary), col = "red")  

# Question 4
games_played50 <- data_omit[which(data_omit$G >= 50),]
games_played50[which.max(games_played50$PER),]
games_played50[which.max(games_played50$TS),]
games_played50[which.max(games_played50$VORP),]

# Question 5
SC <- data_omit[data_omit$Player == "Stephen Curry",]
length(which(data_omit$PER > SC$PER))/nrow(data_omit)
length(which(data_omit$TS > SC$TS))/nrow(data_omit)
length(which(data_omit$VORP > SC$VORP))/nrow(data_omit)

# Question 6
set.seed(0)
data_omit_usa <- data_omit[data_omit$NBA_Country == "USA",]
K <- 10
folds <- rep(1:K,each=ceiling(nrow(data_omit_usa)/K))
folds
permute_observations <- sample(1:nrow(data_omit_usa))
permute_observations
random_folds <- folds[permute_observations]
random_folds
training_set <- random_folds!= 10
data_omit_usa[training_set,]
test_set <- random_folds==10
data_omit_usa[test_set,]
nrow(data[test_set,])/nrow(data_omit_usa)

# Question 7
# A
data_omit_usa_subset <- subset(data_omit_usa, select = -c(1, 3))
training_data <- data_omit_usa_subset[training_set,]
OLS <- glm(log(Salary) ~ ., data = training_data)
coef(OLS)

#b
coef(OLS)["TRB."]
coef(OLS)["AST."]
coef(OLS)["STL."]
coef(OLS)["BLK."]
coef(OLS)["BPM"]

#c
1 - OLS$deviance/OLS$null.deviance

test_data <- data_omit_usa_subset[test_set,]

#cant figure out the right formula
ols_dev <- function(y, pred){
  return(sum((y-pred)^(2)))
}

new_y <- test_data$Salary
ols_pred <- predict(OLS, newdata = test_data)
mean_pred <- mean(log(training_data$Salary))
1 - ols_dev(new_y, ols_pred)/ols_dev(new_y, mean_pred)

# Question 8
# a
test_data <- naref(test_data)
training_data <-naref(training_data)

# b
X <- model.matrix(~ .,data = training_data)[,-1]

# Question 9
# a
lasso_model <- gamlr(X, log(training_data$Salary))

plot(lasso_model, ylab="estimated betas", ylim = c(-0.5, 1))

# b

# c

# Question 10
set.seed(0)
cv.X <- model.matrix(~ ., data = data_omit_usa_subset)[,-1]
cv.lasso_model <- cv.gamlr(cv.X, log(data_omit_usa_subset$Salary))
plot(cv.lasso_model, main = "Cross Validation Model")

# Question 11
set.seed(0)
cv.lasso_model_10 <- cv.gamlr(cv.X, log(data_omit_usa_subset$Salary), nfold = 10)
plot(cv.lasso_model_10, main = "Model with 10 Folds")

cv.betas <- coef(cv.lasso_model_10$gamlr)[-1,]
nonzeros <- cv.betas[which(cv.betas!=0)]
cv.betas["TRB."]
cv.betas["AST."]
cv.betas["STL."]
cv.betas["BLK."]
cv.betas["BPM"]
nonzeros

par(mfrow=c(2,1))
plot(cv.lasso_model, main = "Cross Validation Model")
plot(cv.lasso_model_10, main = "Model with 10 Folds")


# Question 12
betas <- coef(lasso_model)[-1,]
betas
cv.betas

# Question 13
lasso_model$deviance[which.min(lasso_model$deviance)]

OOS <- model.matrix(~ .,data = test_data)[,-1]
lasso_model_OOS <- gamlr(OOS, log(test_data$Salary))

lasso_model_OOS$deviance[which.min(lasso_model_OOS$deviance)]

# Question 14
# a
X_inter <- model.matrix(~ . + Tm*., data = training_data)[,-1]
lasso_model_inter <- gamlr(X_inter, log(training_data$Salary))
lasso_model_inter$deviance[which.min(lasso_model_inter$deviance)]

OOS_inter <- model.matrix(~ . + Tm*.,data = test_data)[,-1]
lasso_model_OOS_inter <- gamlr(OOS_inter, log(test_data$Salary))
lasso_model_OOS_inter$deviance[which.min(lasso_model_OOS_inter$deviance)]

# Question 15
pred_model <- predict(cv.lasso_model_10, newdata = cv.X, select = "min")
pred_model_2 <- predict(lasso_model, newdata = OOS, type = c("response"))
plot(pred_model)
plot(pred_model_2)
