
library(lmtest)
library(sandwich)
library(parameters)
library(readxl)
library(car)
library(tidyverse)
install.packages("readxl")
HeartProject <- read.csv ("/Users/Minal/Desktop/Projects_Github/heart disease/heart_2020_cleaned.csv")
HP <- data.frame(HeartProject)
HP

# H0- There is no relationship between the HeartDisease and the independent variables
# H1- There is a relationship between the HeartDisease and the Independent variables


# 1) Summary 
summary(HP)
View(HP)
sapply(HP, class)
head(HP)


library(plyr)

# Ensure the columns are character vectors
HP$HeartDisease <- as.character(HP$HeartDisease)
HP$Smoking <- as.character(HP$Smoking)
HP$AlcoholDrinking <- as.character(HP$AlcoholDrinking)
HP$Diabetic <- as.character(HP$Diabetic)
HP$Sex <- as.character(HP$Sex)
HP$Stroke <- as.character(HP$Stroke)
HP$PhysicalActivity <- as.character(HP$PhysicalActivity)
HP$Asthma <- as.character(HP$Asthma)
HP$KidneyDisease <- as.character(HP$KidneyDisease)
HP$SkinCancer <- as.character(HP$SkinCancer)

# Revalue the columns
HP$HeartDisease <- revalue(HP$HeartDisease, c("Yes" = 1, "No" = 0))
HP$Smoking <- revalue(HP$Smoking, c("Yes" = 1, "No" = 0))
HP$AlcoholDrinking <- revalue(HP$AlcoholDrinking, c("Yes" = 1, "No" = 0))
HP$Diabetic <- revalue(HP$Diabetic, c("Yes" = 1, "No" = 0))
HP$Sex <- revalue(HP$Sex, c("Female"=1, "Male" = 0))
HP$Stroke <- revalue(HP$Stroke, c("Yes" = 1, "No" = 0))
HP$PhysicalActivity <- revalue(HP$PhysicalActivity, c("Yes" = 1, "No" = 0))
HP$Asthma <- revalue(HP$Asthma, c("Yes" = 1, "No" = 0))
HP$KidneyDisease <- revalue(HP$KidneyDisease, c("Yes" = 1, "No" = 0))
HP$SkinCancer <- revalue(HP$SkinCancer, c("Yes" = 1, "No" = 0))

# Convert the columns to numeric
HP$HeartDisease <- as.numeric(HP$HeartDisease)
HP$Smoking <- as.numeric(HP$Smoking)
HP$AlcoholDrinking <- as.numeric(HP$AlcoholDrinking)
HP$Diabetic <- as.numeric(HP$Diabetic)
HP$Sex <- as.numeric(HP$Sex)
HP$Stroke <- as.numeric(HP$Stroke)
HP$PhysicalActivity <- as.numeric(HP$PhysicalActivity)
HP$Asthma <- as.numeric(HP$Asthma)
HP$KidneyDisease <- as.numeric(HP$KidneyDisease)
HP$SkinCancer <- as.numeric(HP$SkinCancer)

# Verify the conversion
str(HP)
head(HP)


# 2) Showing Correlation Coefficient
cor(HP$HeartDisease, HP$BMI)
cor(HP$HeartDisease, HP$Smoking)
cor(HP$HeartDisease, HP$AlcoholDrinking)
cor(HP$HeartDisease, HP$MentalHealth)
cor(HP$HeartDisease, HP$Sex)
cor(HP$HeartDisease, HP$Diabetic, use = "complete.obs")
cor(HP$HeartDisease, HP$PhysicalActivity)
cor(HP$HeartDisease, HP$SleepTime)
cor(HP$HeartDisease, HP$Asthma)


# 3) Linear Regression Model

library(lmtest)
library(sandwich)

Linear_model<- lm(HeartDisease~SleepTime+BMI, data= HP)
summary(Linear_model)$adj.r.squared
summary(Linear_model)
coeftest(Linear_model, vcov = vcovHC(Linear_model, type = "HC1"))


dev.off


# Load necessary library
library(ggplot2)


# Fit a linear model
linear_model <- lm(HeartDisease ~ BMI, data = HP)
# Create the scatter plot
ggplot(HP, aes(x = BMI, y = HeartDisease)) +
  geom_point(color = "gray4") +
  labs(x = "BMI", y = "Heart Disease", title = "Scatter Plot of Heart Disease vs. BMI") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)



# 4) Quadratic Model
Quadratic_model <- lm(HeartDisease~ SleepTime+BMI, data= HP)
summary(Quadratic_model)
coeftest(Quadratic_model, vcov = vcovHC, type = "HC1")


# 5) Cubic Model
Cubic_model<- lm(HeartDisease~SleepTime+BMI, data= HP)
Cubic_model
summary(Cubic_model)
summary(Cubic_model)$adj.r.squared


# 6) Scatterplots 


attach(HP)
plot(BMI, HeartDisease, main="Heart Disease and BMI",
     xlab="BMI ", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~BMI), col="red") 
lines(lowess(BMI,HeartDisease), col="blue") 

attach(HP)
plot(Smoking, HeartDisease, main="Heart Disease and a Smoking",
     xlab="Smoking ", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~Smoking), col="red")
lines(lowess(Smoking,HeartDisease), col="blue") 

attach(HP)
plot(PhysicalHealth, HeartDisease, main="Heart Disease and a Physical Health",
     xlab="PhysicalHealth", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~PhysicalHealth), col="red") 
lines(lowess(PhysicalHealth,HeartDisease), col="blue")

attach(HP)
plot(MentalHealth, HeartDisease, main="Heart Disease and a Mental Health",
     xlab="MentalHealth", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~MentalHealth), col="red") 
lines(lowess(MentalHealth,HeartDisease), col="blue") 

attach(HP)
plot(DiffWalking, HeartDisease, main="Heart Disease and a individual having difficulty walking",
     xlab="DiffWalking", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~DiffWalking), col="red") 
lines(lowess(DiffWalking,HeartDisease), col="blue") 

attach(HP)
plot(Diabetic, HeartDisease, main="Heart Disease and Diabetic Individual",
     xlab="Diabetic", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~Diabetic), col="red") 
lines(lowess(Diabetic,HeartDisease), col="blue") 

attach(HP)
plot(PhysicalActivity, HeartDisease, main="Heart Disease and Physical Acitivity",
     xlab="PhysicalActivity", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~PhysicalActivity), col="red") 
lines(lowess(PhysicalActivity,HeartDisease), col="blue") 

attach(HP)
plot(SleepTime, HeartDisease, main="Heart Disease and SleepTime",
     xlab="SleepTime", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~SleepTime), col="red") 
lines(lowess(SleepTime,HeartDisease), col="blue") 


attach(HP)
plot(Asthma, HeartDisease, main="Heart Disease and Asthma",
     xlab="Asthma", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~Asthma), col="red") 
lines(lowess(Asthma,HeartDisease), col="blue")

attach(HP)
plot(KidneyDisease, HeartDisease, main="Heart Disease and KidneyDisease",
     xlab="KidneyDisease", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~KidneyDisease), col="red") 
lines(lowess(KidneyDisease,HeartDisease), col="blue")

attach(HP)
plot(SkinCancer, HeartDisease, main="Heart Disease and SkinCancer",
     xlab="SkinCancer", ylab="HeartDisease ", pch=19)
abline(lm(HeartDisease~SkinCancer), col="red")
lines(lowess(SkinCancer,HeartDisease), col="blue")
