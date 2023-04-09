# loading packages
library(tidyverse)



diabetes <- read_csv("final-diabetes-data-for-R.csv")
dim(diabetes)
colnames(diabetes)
dimnames(diabetes[2])
str(diabetes)
diabetes$dm <- factor(diabetes$dm)
diabetes$gender <- factor(diabetes$gender)
class(diabetes$dm)
table(diabetes$dm)
#Simple Logistic Regression
m<- glm(dm~1,family=binomial (link=logit),data = diabetes)
summary(m)
table(m$y) # checking how R has interpreted the binary outcome
m1<- glm(dm~age,family=binomial (link=logit),data = diabetes)
summary(m1)


# create a cross tabulation of age and diabetes status  
dm_by_age <- table(diabetes$age, diabetes$dm) 
# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 
# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 
# calculate the log odds 
logodds <- log(odds) 
# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 

#comparing age and gender as predictors

m1<- glm(dm~age,family=binomial (link=logit),data = diabetes)
summary(m1)
m2<- glm(dm~gender,family=binomial (link=logit),data = diabetes)
summary(m2)
m3 <- glm(dm~location,family=binomial (link=logit),data = diabetes)
summary(m3)


#Kernel density plots // for the distribition- in place of histogram

d <- density(diabetes$age) 
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title 

class(diabetes$chol)

summary(diabetes$chol)
#remove NAs
chol.no.na <- diabetes$chol[is.na(diabetes$chol)==0]
d <- density(chol.no.na)
plot(d,main = "") 

lr2 <- glm(dm~age+gender+height,data = diabetes, family = binomial(link = logit))
summary(lr2)

lgr <- glm(dm~age+chol+insurance,data = diabetes, family = binomial(link = logit))
summary(lgr)
confint(lgr)
# analysis of deviance
anova(lgr,test = "Chisq")
diabetes$insurance <- factor(diabetes$insurance)
