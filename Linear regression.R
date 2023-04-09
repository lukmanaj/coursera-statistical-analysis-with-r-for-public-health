library(tidyverse)
library(readr)

# Preliminaries


COPD <- read_csv("COPD_student_dataset.csv")
COPD

# Examining the dataset
dim(COPD)
colnames(COPD)
str(COPD)
head(COPD)
class(COPD$AGE)
summary(COPD$AGE) #gives NAs,if any 
hist(COPD$AGE)
#categorical variable
class(COPD$COPDSEVERITY)
class(COPD$gender)
table(COPD$COPDSEVERITY,exclude = NULL)
COPD$COPDSEVERITY <- as_factor(COPD$COPDSEVERITY)
class(COPD$COPDSEVERITY)
COPD$gender <- as_factor(COPD$gender)
class(COPD$gender)

ggplot(data=COPD) + geom_histogram(aes(x=MWT1Best))

hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)
subset(COPD, MWT1Best > 650)
subset(COPD, MWT1Best > 600 | MWT1Best < 150) 
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1")   
summary(COPD$MWT1Best)
mean(COPD$MWT1Best, na.rm = TRUE)
  sd(COPD$MWT1Best, na.rm = TRUE)
range(COPD$MWT1Best,na.rm = TRUE)  
IQR(COPD$MWT1Best,na.rm = TRUE)
desc<- list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 

desc

hist(COPD$FEV1)
summary(COPD$FEV1)
sd(COPD$FEV1)

plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

cor.test(COPD$FEV1,COPD$MWT1Best,use="complete.obs",method="pearson")

cor.test(COPD$FEV1,COPD$MWT1Best,use="complete.obs",method="spearman")
# General formula
# cor.test(x, y, method=c(“pearson”, “kendall”, “spearman”))

hist(COPD$AGE)
summary(COPD$AGE)
sd(COPD$AGE)

cor.test(COPD$AGE,COPD$FEV1,use="complete.obs",method="pearson")
cor.test(COPD$AGE,COPD$FEV1,use="complete.obs",method="spearman")


# Linear Regression Proper

MWT1Best_FEV1 <- lm(MWT1Best~FEV1,data = COPD)
summary(MWT1Best_FEV1)
confint(MWT1Best_FEV1)
plot(MWT1Best_FEV1)  #didn't work, have to troubleshoot.

par(mfrow=c(1,1)) 

MWT1Best_AGE <- lm(MWT1Best~AGE,data = COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)
plot(MWT1Best_AGE)





# Multiple Linear Regression

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE,data = COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)
plot(MWT1Best_FEV1_AGE)

# MLR Practice

MWT1Best_FVC <- lm(MWT1Best~FVC,data = COPD)
summary(MWT1Best_FVC)

MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE,data = COPD)
summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)
plot(MWT1Best_FVC_AGE)



IQR((cop))