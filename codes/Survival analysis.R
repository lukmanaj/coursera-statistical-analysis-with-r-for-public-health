data <- read_csv("simulated HF mort data for GMPH (1K) final.csv")
dim(data)
str(data)
head(data)
class(data$gender)

#install required packages, ggplot2 is part of tidyverse
install.packages("survival")
library(survival)
library(ggplot2)

#create the objects for our variables
gender <- factor(data$gender) #categorical
fu_time <- data$fu_time  #continous
death <- data$death #binary numeric variable
#Kaplan-Meier curve
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)
#Seeing the km estimates
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 
#Splitting by gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

#compare survival : log rank test
survdiff(Surv(fu_time, death) ~ gender, rho=0) 
#Practice
#Dichotomise age
broad_age <- ifelse(data$age>=65,1,0)
#check
table(broad_age)
#compare with age
table(data$age,broad_age,exclude = NULL)
km_br_age_fit <- survfit(Surv(fu_time,death) ~ broad_age)
plot(km_br_age_fit)
survdiff(Surv(fu_time,death) ~broad_age)
