# read in csv file
insurance <- read.csv("insurance.csv")

head(insurance)

# Data exploration and assumptions:


# get column names
names(insurance)

# get number of rows and columns of data frame
dim(insurance)

# explore structure of data frame columns
str(insurance)

# check null values in dataset
colSums(is.na(insurance))  

# calculate descriptive statistics
summary(insurance)

# hitograms of the columns
layout(matrix(1:6, 3, 2))
hist(insurance$charges, main = "Histogram Charges")
hist(insurance$bmi, main = "Histogram BMI")
hist(insurance$age, main = "Histogram Age")
barplot(table(insurance$children), main = "Children")
barplot(table(insurance$smoker), main = "Smoker")
barplot(table(insurance$region), main = "Region")

# plots to show the relationship between the response variable (charges) and the 6 explanatory variables
layout(matrix(1:6, 2, 3))
plot(charges ~ bmi, data = insurance)
plot(charges ~ smoker, data = insurance)
plot(charges ~ children, data = insurance)
plot(charges ~ region, data = insurance)
plot(charges ~ sex, data = insurance)
plot(charges ~ age, data = insurance)

# Data preprocessing:


# change "sex", "smoker" and "region" variable into factors
insurance[,"sex"] <- as.factor(insurance[,"sex"])
insurance[,"smoker"] <- as.factor(insurance[,"smoker"])
insurance[,"region"] <- as.factor(insurance[,"region"])

# create a column containing the log of the charges column
insurance[,"log_charges"] <- log(insurance[,"charges"])
head(insurance, 3)

# standardize bmi
insurance[,"bmi"] <- (insurance[,"bmi"] - mean(insurance[,"bmi"])) / sd(insurance[,"bmi"])

# installing packages
install.packages("lmtest")
install.packages("sandwich")
install.packages("corrplot")

# load libraries
library("sandwich")
library("lmtest")

# fit multiple regression model
final_model <- lm(charges ~ age + bmi + children + smoker + region + bmi*smoker, data = insurance) 

# see statistical summary of the model
summary(final_model)

# check which variables are numeric
sapply(insurance, is.numeric)

# check outliers
plot(final_model, which=4)

insurance[129,]
# Probably an anorexic smoking woman with high charges. No need to exclude.

insurance[1048,]
# a very obese man, with high charges. No need to exclude

insurance[1301,]
# a smoking man that has to pay an outstanding high amount of charges. No need to drop in my opinion.

# create matrix
X <- model.matrix(final_model, data = insurance)
X[1, ]


install.packages("caret")
library(caret)

# show multicollinearity
library(corrplot)
corrplot(cor(X[, -1]), type = "lower")

# robust t-test
coeftest(final_model, vcov = vcovHC(final_model, type = "HC0"))

# robust F-Test with Waldtest
waldtest(final_model)

plot(final_model, which=1)
plot(final_model, which = 1:2)

# check assumptions of model based on the following plots
layout(matrix(1:6, 3, 2))
plot(final_model, which = 1:5)


interaction_model <- lm(charges ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 

summary(interaction_model)
plot(interaction_model, which = 1:2)

#An increase in bmi has a higher negative effect on insurance cost for smokers than for non-smokers. Adding the interaction term of bmi and smoker increases r2 by ~ 0.1. Having a bmi that is 1 standard deviation higher than the mean, increases medical insurance costs by 8790$!
#Smoker pay a 23805$! yearly premium according to the model.
#Also, the sex does not have a significant impact. When running the regression without it, the adj. R2 is higher.

# try transformations to address the validation of model assumptions

#log transformation
log_simple_model <- lm(log_charges ~ age + sex + bmi + children + smoker + region, data = insurance) 
summary(log_simple_model)
# check assumptions of model based on the following plots
layout(matrix(1:6, 3, 2))
plot(log_simple_model, which = 1:5)

log_interaction_model <- lm(log_charges ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 
summary(log_interaction_model)
plot(log_interaction_model, which = 1:2)
mean(insurance$charges)

# check assumptions of model based on the following plots
layout(matrix(1:6, 3, 2))
plot(log_interaction_model, which = 1:5)
plot(log_simple_model, which = 1:2)

# visualize the relationship insurance charges between smokers and non-smokers
plot(charges ~ bmi, data = insurance,
     col = c("blue", "red")[insurance$smoker], main = "How bmi affects insurance charges between smokers and non-smokers")
legend("topleft", col = c("blue", "red")[insurance$smoker], pch = 1,legend = levels(insurance$smoker), title= "Smoking habit")

# smoker and non smoker charges
plot(charges ~ smoker, data = insurance,
     col =c("blue", "red")[insurance$smoker], main = "Smoker and non-smoker charges")
legend("topleft", col = c("blue", "red")[insurance$smoker], pch = 1,legend = levels(insurance$smoker), title= "Smoking habit")

# Model of choice
interaction_model <- lm(charges ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 

log_interaction_model = lm(log(charges) ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 

plot(log_interaction_model, which=2)

# histogram of residuals
hist(log_interaction_model$residuals, xlab = "residuals",
     main = "Histogram of residuals")

sqrt_interaction_model = lm(sqrt(charges) ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 
plot(sqrt_interaction_model, which=1:2)
hist(sqrt_interaction_model$residuals, xlab = "residuals",
     main = "Histogram of residuals")

inv_interaction_model = lm(1/(charges) ~ age + bmi + children + smoker + region + I(bmi^2) + bmi*smoker, data = insurance) 
plot(inv_interaction_model, which=1:2)
hist(inv_interaction_model$residuals, xlab = "residuals",
     main = "Histogram of residuals")


plot(charges ~ fitted(final_model), data = insurance,col = c("blue", "red")[insurance$smoker], main = "Fitted Model Values vs Charges")
legend("topleft", col = c("blue", "red")[insurance$smoker], pch = 1,legend = levels(insurance$smoker), title= "Smoking habit")
## we can see some heteroskedasticity in this plot

# Train and Test Predictions with Final Model

sample <- sample(c(TRUE, FALSE), nrow(insurance), replace=TRUE, prob=c(0.8,0.2))
train  <- insurance[sample, ]
test   <- insurance[!sample, ]


model <- lm(charges ~ age + bmi + children + smoker + region + bmi*smoker, data = train)

insurance$region <- as.factor(insurance$region)
insurance$charges <- as.factor(insurance$charges)
insurance$age <- as.factor(insurance$age)
insurance$bmi <- as.factor(insurance$bmi)
insurance$children <- as.factor(insurance$children)
insurance$smoker <- as.factor(insurance$smoker)


pred_test <- predict(model, newdata = test)

performance_accuracy(final_model)




