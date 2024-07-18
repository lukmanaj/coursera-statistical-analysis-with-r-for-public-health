# Install and load relevant packages 
 
install.packages("survival") 

## Installing package into  
## (as 'lib' is unspecified) 

## package 'survival' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##  install.packages("ggplot2") 

## Installing package into  
## (as 'lib' is unspecified) 

## package 'ggplot2' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##  install.packages("survminer") 

## Installing package into  
## (as 'lib' is unspecified) 

## package 'survminer' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##   

install.packages("ggfortify") 

## Installing package into  
## (as 'lib' is unspecified) 

## package 'ggfortify' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
 

require(survival)  

## Loading required package: survival 

## Warning: package 'survival' was built under R version 3.5.1 

require(ggplot2)  

## Loading required package: ggplot2 

## Warning: package 'ggplot2' was built under R version 3.5.1 

require(survminer) 

## Loading required package: survminer 

## Warning: package 'survminer' was built under R version 3.5.1 

## Loading required package: ggpubr 

## Warning: package 'ggpubr' was built under R version 3.5.1 

## Loading required package: magrittr 

require(ggfortify) 

## Loading required package: ggfortify 

## Warning: package 'ggfortify' was built under R version 3.5.1 

# Load dataset 
g <- read.csv(file = paste0(getwd(),"/simulated HF mort data for GMPH (1K) final.csv"), header=TRUE, sep=',') 
 
 
# Define variables 
gender <- factor(g[,"gender"]) 
fu_time <- g[,"fu_time"]  
death <-  g[,"death"] 
age <- g[,"age"] 
copd <- factor(g[,"copd"]) 
ethnicgroup <- factor(g[,"ethnicgroup"]) 
quintile <- factor(g[,"quintile"]) 
ihd <- factor(g[,'ihd']) 
valvular <- factor(g[,'valvular_disease']) 
pvd <- factor(g[,'pvd']) 
stroke <- factor(g[,'stroke']) 
pneumonia <- factor(g[,'pneumonia']) 
renal <- factor(g[,'renal_disease']) 
ca <- factor(g[,'cancer']) 
mets <- factor(g[,'metastatic_cancer']) 
mental_health <- factor(g[,'mental_health']) 
ht <- factor(g[,"hypertension"]) 
cog_imp <- factor(g[,"senile"]) 
prior_dnas <- g[,"prior_dnas"] 
 
 
 
# Plotting a Kaplan-Meier curve 
###################### 
 
# 1. Generate the survival curve 
km_fit <- survfit(Surv(fu_time, death) ~ 1) 
