#Assessing cox models

#Checking proportionality assumptions

cox.zph(fit, transform="km", global=TRUE)
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model

temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results

plot(temp) # plot the curves
#kaplan-meir plot
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 


res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 


res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "deviance", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

#martingale residual
ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age)) 






fit2 <- coxph(Surv(fu_time, death) ~ copd) # fit the desired model

temp2 <- cox.zph(fit2)# apply the cox.zph function to the desired model
print(temp2) # display the results


#when proportionality is not met, interaction term can be added, e.g. time and gender as below
fit3 <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function 
summary(fit3) 
fit



#model selection in multiple cox models
colnames(data)
mod <- coxph(Surv(fu_time,death) ~ age+gender+quintile_5groups+ethnicgroup+prior_dnas)
summary(mod)
