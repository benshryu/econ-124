# Get the working directory
getwd()
# Change the working directory
setwd("/Users/Tonkos/OneDrive/Documents/ECON124/Homework 4")
install.packages("gamlr")
library(gamlr)

# Question 1
data  <- read.csv("sipp1991.csv")
coef(summary(glm(net_tfa ~ e401 + age + inc, data=data)))[2,]

# a
# the enrollment variable e401, if there employer offers 401k e401 is equal 1 
# which adds 5.135e+03 dollars in net total financial assets

# b
# We may not consider it a valid estimate of a causal effect in this regression because of the significance not being any higher
# being only at 0.001 while all other variable in this control is at 0 significance level. It also raise the concern that
# exprience which may be accounted to age and income to actually profit to be more significant especially if the participation rate for 401k is low.
 
# c
# Since 401k was not something that is often consider into when getting a job in the 1990s the sample size may be small 
# and not be as significant in over net profit. It may also be that people did not put much into 401k therefore the effect on net profit was less significant
# less then age in which also should be accounting for experience.

# Question 2

# a
# because of retirement age does not prove to be linear rather its going to be polynomial regression which at the end and begin of the regression.
# we expect to see a dip in income increase. 

# b
data$age <- factor(data$age)
data$educ <- factor(data$educ)

poly_x_reg <- model.matrix(net_tfa ~ age + educ + ( inc + db + marr +twoearn + pira + hown)^2 + poly(fsize, 8, raw=TRUE) + poly(inc, 8, raw=TRUE), data=data)
summary(poly_x_reg)
# 87 covarites

# c
set.seed(0)

net_tfa_model <- cv.gamlr(poly_x_reg, data$net_tfa) 
e401_model <- cv.gamlr(poly_x_reg, data$e401)

net_tfa_nonzero_coef_ind <- which(coef(net_tfa_model, select="min")[-1] != 0) 
e401_nonzero_coef_ind <- which(coef(e401_model, select="min")[-1] != 0) 
nonzero_coef_ind <- union(net_tfa_nonzero_coef_ind, e401_nonzero_coef_ind)

data2 <- data.frame(net_tfa=data$net_tfa, e401=data$e401, as.matrix(poly_x_reg[,nonzero_coef_ind]))

post_lasso <- glm(net_tfa ~ ., data=data2) 
coef(summary(post_lasso))['e401',]

# d
data_counter <-  data2
data_counter$e401 <- 1
 
mean(predict(post_lasso, data_counter))
mean(data$net_tfa)

# under the Counter factual there is an increase in income using the same model as part c

# e
# no because our model that is used to predict is invalid