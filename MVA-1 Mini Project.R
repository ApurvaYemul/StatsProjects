#   Name: Apurva Yemul
#   PRN: 22060641020

#MVA Mini Project

#Libraries used

library(readr)
library(fBasics)
library(corrplot)
library(caret)
library(car)
library(ggplot2)


#importing dataset
cancer_data = read_csv("C:/Users/appu0/Downloads/cancer_data.csv")
View(cancer_data)

dim(cancer_data)  #No.of rows and cols 
colnames(cancer_data) 

#Renaming Column names
colnames(cancer_data)[10]="concave_points_mean"
colnames(cancer_data)[20]="concave_points_se"
colnames(cancer_data)[30]="concave_points_worst"

#Converting Diagnosis (Malignant/Benign) as 1/0 
cancer_data$diagnosis<-ifelse(cancer_data$diagnosis=="M",1,0)
View(cancer_data)

#Removing unwanted columns 
D = cancer_data[-c(1,33)]
View(D)

##    Descriptive Analysis

df = D[-1]  #removing "diagnosis" column
View(df)
basicStats(df)

table(cancer_data$diagnosis)  # Count of Benign and Malignant cases in data
ggplot(cancer_data, aes(diagnosis)) + geom_bar()
M = cor(df)  # Correlation between all variables
M
corrplot(M)  

# Checking Variables that are highly correlated
HighlyCorrelated = findCorrelation(M, cutoff=0.70)
HighlyCorCol = colnames(df)[HighlyCorrelated]
HighlyCorCol

# Dropping the variables which are Highly Correlated
D = D[-which(colnames(D) %in% HighlyCorCol)]
View(D)
colnames(D)  # The variables to consider for Modelling 

#To check for Multicollinearity
fit1 = lm(diagnosis~., data = D)
vif(fit1)  # variables with vif value greater than 5 have critical level of correlation 


# As the Dependent variable (diagnosis) is categorical,
# we fit a Logistic Regression Model

#Fitting a Regression model

# Dividing the data to train and test

set.seed(3)
samp = sample(c(TRUE,FALSE), nrow(D), replace = TRUE, prob=c(0.8,0.2))
train = D[samp,]
test = D[!samp,]

# Fitting a Logistic Regression model
glm.fit = glm(formula = diagnosis ~ area_mean + symmetry_worst + texture_mean +
                smoothness_worst + fractal_dimension_worst, 
              data = train, family = "binomial")
summary(glm.fit)  

glm.probs = predict(glm.fit, newdata = test, type = "response")  
glm.pred = ifelse(glm.probs > 0.5, "Malignant", "Benign")

# Testing the model on the remaining 20% data

diag = test$diagnosis
table(glm.pred, diag)  # Confusion Matrix - 0 (Benign), 1 (Malignant)


#Graph plotting 

ggplot(cancer_data, aes(x =area_mean , y=diagnosis)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE, method.args = list(family=binomial)) +
  labs( x = "Mean Area", y ="Diagnosis" )

