# cox model
# required libraries: survival and survminer
#install required libraries
install.packages("survival")
install.packages("survminer")

#load required packages
library(survival)
library(survminer)

#dataset
data <- read_csv("simulated HF mort data for GMPH (1K) final.csv")
#create objects for predictors and outcomes
fu_time <- data$fu_time  #continous
death <- data$death #binary numeric variable
ethnicgroup <- factor(data$ethnicgroup)
copd <- factor(data$copd) #binary 
prior_dnas <- data$prior_dnas
gender <- factor(data$gender) #categorical
age <- data$age
quintile <- factor(data$quintile)
#cox proportional hazard model 
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = data) # take variables straight from data
summary(cox)

cox1 <- coxph(Surv(fu_time,death) ~ ethnicgroup) #making ethnicgroup a factor
summary(cox1)

#working on the missing values: people with no ethnicity\
levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"


#Knowing the data
summary(data$age)
hist(data$age)

table(data$gender,exclude = NULL)
table(data$ethnicgroup,exclude = NULL)
table(data$prior_dnas,exclude = NULL)
table(data$copd,exclude = NULL)
table(data$quintile,exclude = NULL)
# Multiple cox model 

cox <- coxph(Surv(fu_time,death) ~ age + gender + copd +  
               prior_dnas + ethnicgroup)
summary(cox)

cox3 <- coxph(Surv(fu_time,death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox3)


#fix non convergence caused by quintile: relevel
quintile_5groups <- data$quintile
quintile_5groups[quintile_5groups==0] <- 5
quintile_5groups <- factor(quintile_5groups)


# corrected cox model
cox4 <- coxph(Surv(fu_time,death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox4)


