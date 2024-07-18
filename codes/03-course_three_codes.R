# import data 
g <- read.csv(file = paste0(getwd(), "/diabetes.csv"), header=TRUE, sep=",") 
 
# define your variables 
############### 
 
chol <- g[,"chol"]  
gender <- as.factor(g[,"gender"])  
height <- g[,"height"] 
weight <- g[,"weight"] 
age <- g[,"age"] 
dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"])# let"s say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
hdl <- g[,"hdl"] 
ratio <- g[,"ratio"] 
location <- as.factor(g[,"location"]) 
frame <- as.factor(g[,"frame"]) 
systolic <- g[,"bp.1s"] 
diastolic <- g[,"bp.1d"] 
 
# calculate BMI from height and weight: 
############### 
 
# 1. convert height and weight to metric units 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
 
# 2. BMI = weight over height squared 
bmi <- weight.si/height.si^2 
 
############### 
 
# create a table for the gender variable 
table_gender <- table(gender)  
 
# display % in each gender 
round(100 * prop.table(table_gender), digits = 1)  
## gender 
## female   male  
##   58.1   41.9 
# categorise BMI by category 
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 
 
# cross tabulate diabetes status and BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm, exclude = NULL) 
dm_by_bmi_category 
##                dm 
## bmi_categorised  no yes <NA> 
## 	normal  	100   9    4 
## 	obese       118  29	5 
## 	overweight   99  20    4 
## 	underweight   9   0	0 
## 	<NA>          4   2	0 
# produce the table as % in each BMI category with or without diabetes 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
##                dm 
## bmi_categorised	no   yes  <NA> 
## 	normal   	88.5   8.0   3.5 
## 	obese        77.6  19.1   3.3 
## 	overweight   80.5  16.3   3.3 
## 	underweight 100.0   0.0   0.0 
## 	<NA>         66.7  33.3   0.0 
# categorise age by group 
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 
 
# cross tabulate age by gender 
age_group_by_gender <- table(age_grouped, gender, exclude = NULL) 
 
# print % in each age group by gender 
round(100 * prop.table(age_group_by_gender, margin = 2), digits = 1) 
##             gender 
## age_grouped  female male 
##   45 - 64  	32.1 37.9 
##   65 - 74   	9.0 11.8 
##   75 or over    5.1  6.5 
##   under 45 	53.8 43.8 
# create a null logistic model for diabetes 
m <- glm(dm ~ 1, family = binomial(link = logit)) 
summary(m) 
##  
## Call: 
## glm(formula = dm ~ 1, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
##	Min  	1Q  Median  	3Q 	Max   
## -0.578  -0.578  -0.578  -0.578   1.935   
##  
## Coefficients: 
##         	Estimate Std. Error z value Pr(>|z|)     
## (Intercept)  -1.7047 	0.1403  -12.15   <2e-16 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.87  on 389  degrees of freedom 
## Residual deviance: 334.87  on 389  degrees of freedom 
##   (13 observations deleted due to missingness) 
## AIC: 336.87 
##  
## Number of Fisher Scoring iterations: 3 
# perform logistic regression with gender as predictor variable 
m <- glm(dm ~ gender, family = binomial(link = logit)) 
summary(m) 
##  
## Call: 
## glm(formula = dm ~ gender, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
## 	Min   	1Q   Median   	3Q  	Max   
## -0.5915  -0.5915  -0.5683  -0.5683   1.9509   
##  
## Coefficients: 
##         	Estimate Std. Error z value Pr(>|z|)     
## (Intercept) -1.74150    0.18592  -9.367   <2e-16 *** 
## gendermale   0.08694	0.28352   0.307	0.759     
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.87  on 389  degrees of freedom 
## Residual deviance: 334.78  on 388  degrees of freedom 
##   (13 observations deleted due to missingness) 
## AIC: 338.78 
##  
## Number of Fisher Scoring iterations: 4 
# perform logistic regression with gender as predictor variable, with male as reference group 
# generate odds of having diabetes if female compared to male 
############### 
 
# 1. check order of the levels in the gender variable 
levels(gender) 
## [1] "female" "male" 
# 2. make "male" the reference group 
gender <- relevel(gender, ref = "male") 
 
# 3. run logistic regression 
m <- glm(dm ~ gender, family = binomial(link = logit)) 
summary(m) 
##  
## Call: 
## glm(formula = dm ~ gender, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
## 	Min   	1Q   Median   	3Q  	Max   
## -0.5915  -0.5915  -0.5683  -0.5683   1.9509   
##  
## Coefficients: 
##          	Estimate Std. Error z value Pr(>|z|)     
## (Intercept)  -1.65456	0.21404  -7.730 1.08e-14 *** 
## genderfemale -0.08694    0.28352  -0.307	0.759     
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.87  on 389  degrees of freedom 
## Residual deviance: 334.78  on 388  degrees of freedom 
##   (13 observations deleted due to missingness) 
## AIC: 338.78 
##  
## Number of Fisher Scoring iterations: 4 
# 4. exponentiate the log odds of having diabetes when female to obtain the odds 
exp(m$coefficients["genderfemale"]) 
## genderfemale  
##	0.9167328 
############### 
 
 
# run logistic model with age as predictor variable 
m <- glm(dm ~ age, family = binomial(link = logit)) 
summary(m) 
##  
## Call: 
## glm(formula = dm ~ age, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
## 	Min   	1Q   Median   	3Q  	Max   
## -1.3612  -0.5963  -0.4199  -0.3056   2.4848   
##  
## Coefficients: 
##          	Estimate Std. Error z value Pr(>|z|)     
## (Intercept) -4.404530   0.542828  -8.114 4.90e-16 *** 
## age      	0.052465   0.009388   5.589 2.29e-08 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.87  on 389  degrees of freedom 
## Residual deviance: 299.41  on 388  degrees of freedom 
##   (13 observations deleted due to missingness) 
## AIC: 303.41 
##  
## Number of Fisher Scoring iterations: 5 
# to check whether the relationship between age and log odds of having diabetes is linear (an assumption of the logistic regression) 
############### 
 
# 1. create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 
 
# 2. output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 
 
# 3. calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 
 
# 4. calculate the log odds 
logodds <- log(odds) 
 
# 5. plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 
