# Loading the dataset for stat course
# 30th December, 2022
# Lukman

# stat is fun
install.packages("tidyverse")
library(tidyverse)
library(readr)
cancer_data <- read_csv("cancer data for MOOC 1.csv")
head(cancer_data)
colnames(cancer_data)
cancer_data$fruit_veg <- (cancer_data$fruit + cancer_data$veg)
fruit_veg
as.factor((fruit_veg))
str(cancer_data)
summary(cancer_data)
map(cancer_data,~sum(is.na(.)))

# Plotting using ggplot

ggplot() + geom_histogram(data = cancer_data, aes(x = fruit_veg), bins = 10, fill = "darkgreen", col = "black") +
  
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()

cancer_data$healthy_BMI <- ifelse(cancer_data$bmi > 18.5 & cancer_data$bmi < 25, 1, 0)

# Plotting normally
# fruits and vegetable singly
hist(cancer_data$fruit, xlab = "Portions of fruit", main = "Daily consumption of fruit", axes = F)
axis(side = 1, at = seq(0, 4, 1))
axis(side = 2, at = seq(0, 24, 4))

hist(cancer_data$veg, xlab = "Portions of vegetables",main = "Daily consumption of vegetables", axes = F)
axis(side = 1, at = seq(0, 9, 1))
axis(side = 2, at = seq(0, 18, 2))


#Using ggplot
ggplot() + geom_histogram(data = cancer_data, aes(x = fruit), bins = 5, fill = "darkgreen", col = "black") +
  theme_bw() + labs(x = "Portions of fruit", y = "Frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))


ggplot() + geom_histogram(data = cancer_data, aes(x = veg), bins = 10, fill = "darkgreen", col = "black") + 
  theme_bw() + labs(x = "Portions of vegetables", y = "Frequency") + 
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1))
# Assignment

cancer <- cancer_data$cancer
overweight <- ifelse(cancer_data$bmi >= 25, 1, 0)
chisq.test(x = overweight, y = cancer)
