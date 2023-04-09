##McFadden's R-squared for logistic regression
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit),data = diabetes) 

# check your model 
summary(full_model) 

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit),data = diabetes) 

# check 
summary(null_model) 
# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 
# print it 
R2 

## c-statistics

# install a package 
install.packages("DescTools") 

# load package 
library(DescTools) 
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit),data = diabetes) 

# check your model 
summary(full_model) 
# generate the c-statistic 
Cstat(full_model) 

##Hosmer-Lemeshow statistic and test: 

# H-L test 

# install package "ResourceSelection" 
install.packages("ResourceSelection") 
  
# load package 
library(ResourceSelection) 

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit),data = diabetes) 

full_model$y

# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL  

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 
# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 
# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 

##  Hosmer and Lemeshow goodness of fit (GOF) test 
##  
## data:  full_model$y, fitted(full_model) 
## X-squared = 11.25, df = 8, p-value = 0.1879 
# verify result with another package? 

# install package("generalhoslem") 
install.packages("generalhoslem") 
 
##   
# load package 
library(generalhoslem) 
 
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 
 
##  Hosmer and Lemeshow test (binary model) 
##  
## data:  full_model$y, fitted(full_model) 
## X-squared = 11.25, df = 8, p-value = 0.1879 


