# make the other covariates 

ihd <- factor(g[,'ihd']) 

valvular <- factor(g[,'valvular_disease']) 

pvd <- factor(g[,'pvd']) 

stroke <- factor(g[,'stroke']) 

copd<- factor(g[,'copd'])

pneumonia <- factor(g[,'pneumonia']) 

ht <- factor(g[,'hypertension'])

renal <- factor(g[,'renal_disease']) 

ca <- factor(g[,'cancer']) 

mets <- factor(g[,'metastatic_cancer']) 

mental_health <- factor(g[,'mental_health']) 

los <- g[,'los']

prior_dna <- g[,'prior_dnas']

# generate cognitive impairment variable (senility and dementia combined)
 
cog_imp <- as.factor(ifelse(g$dementia == 1 | g$senile == 1, 1, 0))

# run the full model 

cox <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + 

               valvular + pvd + stroke + copd + pneumonia + ht + renal + 

               ca + mets + mental_health + cog_imp + los + prior_dna) 

summary(cox) 

> summary(cox) 

Call: 

coxph(formula = Surv(fu_time, death) ~ age + gender + ethnicgroup +  

    ihd + valvular + pvd + stroke + copd + pneumonia + ht + renal +  

    ca + mets + mental_health + cog_imp + los + prior_dna) 

 

  n= 1000, number of events= 492  
  

                    coef exp(coef)  se(coef)      z Pr(>|z|)  
                    
age             0.058348  1.060083  0.005801 10.058  < 2e-16 ***

gender2        -0.215988  0.805745  0.097592 -2.213  0.02688 * 

ethnicgroup2   -0.159816  0.852301  0.351947 -0.454  0.64976

ethnicgroup3   -0.724770  0.484436  0.416059 -1.742  0.08151 .

ethnicgroup9    0.437472  1.548787  0.363859  1.202  0.22924  

ethnicgroup8   -0.086505  0.917131  0.231729 -0.373  0.70892  

ihd1            0.174550  1.190710  0.096181  1.815  0.06955 . 

valvular1       0.194524  1.214732  0.108805  1.788  0.07380 .  

pvd1            0.035442  1.036077  0.161851  0.219  0.82667    

stroke1        -0.007433  0.992594  0.301432 -0.025  0.98033    

copd1           0.103113  1.108616  0.106823  0.965  0.33441    

pneumonia1      0.302242  1.352889  0.141232  2.140  0.03235 *  

ht1            -0.057556  0.944069  0.096372 -0.597  0.55036    

renal1          0.153977  1.166464  0.109547  1.406  0.15985    

ca1             0.280847  1.324251  0.206595  1.359  0.17402    

mets1           2.194757  8.977824  0.399639  5.492 3.98e-08 ***

mental_health1 -0.066662  0.935511  0.181811 -0.367  0.71387    

cog_imp1        0.327423  1.387388  0.149461  2.191  0.02847 *  

los             0.011553  1.011620  0.003257  3.547  0.00039 ***

prior_dna       0.107472  1.113460  0.041807  2.571  0.01015 *

--- 

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



               exp(coef) exp(-coef) lower .95 upper .95
               
age               1.0601     0.9433    1.0481    1.0722

gender2           0.8057     1.2411    0.6655    0.9756

ethnicgroup2      0.8523     1.1733    0.4276    1.6989

ethnicgroup3      0.4844     2.0643    0.2143    1.0949

ethnicgroup9      1.5488     0.6457    0.7591    3.1602

ethnicgroup8      0.9171     1.0904    0.5823    1.4444

ihd1              1.1907     0.8398    0.9861    1.4377

valvular1         1.2147     0.8232    0.9814    1.5035

pvd1              1.0361     0.9652    0.7544    1.4229

stroke1           0.9926     1.0075    0.5498    1.7921

copd1             1.1086     0.9020    0.8992    1.3668

pneumonia1        1.3529     0.7392    1.0258    1.7843

ht1               0.9441     1.0592    0.7816    1.1403

renal1            1.1665     0.8573    0.9411    1.4458

ca1               1.3243     0.7551    0.8833    1.9853

mets1             8.9778     0.1114    4.1020   19.6492

mental_health1    0.9355     1.0689    0.6551    1.3360

cog_imp1          1.3874     0.7208    1.0351    1.8596

los               1.0116     0.9885    1.0052    1.0181

prior_dna         1.1135     0.8981    1.0259    1.2085 

 

Concordance= 0.707  (se = 0.012 )

Likelihood ratio test= 225.9  on 20 df,   p=<2e-16

Wald test            = 216.9  on 20 df,   p=<2e-16

Score (logrank) test = 235.1  on 20 df,   p=<2e-16 




fit <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + 

               mets + cog_imp) # test them all in the same model 

temp <- cox.zph(fit)  

print(temp) 

 

> print(temp) 

                rho   chisq     p 

age        -0.03179 0.47585 0.490 

gender2     0.05503 1.43314 0.231 

valvular1  -0.04830 1.15050 0.283 

pneumonia1 -0.04239 0.92648 0.336 

mets1       0.00403 0.00787 0.929 

cog_imp1   -0.01859 0.18460 0.667 

GLOBAL           NA 3.95244 0.683 
