# Using relevel function to change a factor reference
#Creating new variables from old ones
comorbid <- length(COPD$Diabetes) 
comorbid[COPD$Diabetes==1|COPD$muscular==1|COPD$hypertension==1|COPD$AtrialFib==1|COPD$IHD==1]<-1 #assign a value of 1 if any of the comorbidities is present
comorbid[is.na(comorbid)] <- 0 #assign a value of zero if no comorbidity
comorbid <- factor(comorbid)  #make a factor
COPD$comorbid <- comorbid
class(COPD$comorbid)
print(COPD$comorbid)

# Checking the dataset prior to fitting the model
# Good practice analysis
#install  and load required packages
install.packages("Hmisc")
library(Hmisc)
install.packages("gmodels")
library(gmodels)
install.packages("mctest")
library(mctest)
describe(COPD)
# for categorical variables, use Crosstable function from the gmodels package
CrossTable(COPD$COPDSEVERITY,COPD$gender)
sum(is.na(COPD$COPDSEVERITY))#checking missing values for cat variables
# constinuous variables -> summary function
#use hist to check normality
#calculate correlation coefficient using cor.test()
# Correlation matrix
my_data <- COPD[,c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD","SGRQ")]
#new vector for variables to be analysed
cor_matrix <- cor(my_data)
cor_matrix
round(cor_matrix,2) #2 decimal places
#correlation plot using pairs()
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ,data = COPD)
mlr1 <- lm(MWT1Best~FEV1+AGE+gender+COPDSEVERITY+comorbid,data = COPD)
summary(mlr1)
confint(mlr1)  
plot(mlr1,method="spearman")  

#checking VIF using imcdiagF() from mctest package
imcdiag(mlr1,method = 'VIF')
  
#interactions between binary variables
#first change the variables to integers
# eg Diabetes and AtrialFIb comorbidities

COPD$Diabetes <- as.integer(COPD$Diabetes)
COPD$AtrialFib <- as.integer(COPD$AtrialFib)
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)
summary(r1)

