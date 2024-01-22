
library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(GGally)
library(rpart)
library(caret)
library(naniar)
library(DescTools)
library(FactoMineR)


#Reading the data
data = read_csv("C:/Users/User/Documents/Winter 2023/STAT 847/Final Project/Loan Credit Kaggle/application_data.csv")

#Data before cleaning
dim(data)
summary(data)

#Data Cleaning
data = data[, -c(42:86)]
data = select(data, -c("TOTALAREA_MODE", "AMT_REQ_CREDIT_BUREAU_HOUR", "AMT_REQ_CREDIT_BUREAU_DAY", 
                       "AMT_REQ_CREDIT_BUREAU_WEEK", "AMT_REQ_CREDIT_BUREAU_MON",
                       "AMT_REQ_CREDIT_BUREAU_QRT", "AMT_REQ_CREDIT_BUREAU_YEAR",
                       "OWN_CAR_AGE"))

#Checking NA values in categorical features
colSums(is.na(data[, c("NAME_CONTRACT_TYPE", "FLAG_OWN_CAR",
                       "FLAG_OWN_REALTY", "NAME_TYPE_SUITE","NAME_INCOME_TYPE",
                       "NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS","NAME_HOUSING_TYPE",
                       "OCCUPATION_TYPE","WEEKDAY_APPR_PROCESS_START","ORGANIZATION_TYPE",
                       "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "WALLSMATERIAL_MODE",
                       "EMERGENCYSTATE_MODE" )]))

#Dropping features
data = select(data, -c("FONDKAPREMONT_MODE",          
                       "HOUSETYPE_MODE", "WALLSMATERIAL_MODE","EMERGENCYSTATE_MODE", "OCCUPATION_TYPE",
                       "NAME_TYPE_SUITE"))

#Checking XNA values
colSums(data == "XNA")

#Remove XNA values in CODE_GENDER
data = subset(data, CODE_GENDER!="XNA")

#Dropping ORGANIZATION_TYPE (55374 XNA values)
data = select(data, -c("ORGANIZATION_TYPE"))

#Replace NA values of Count of family members with 0 (2 Null values)
data$CNT_FAM_MEMBERS = replace(data$CNT_FAM_MEMBERS, is.na(data$CNT_FAM_MEMBERS),0)

#Replace 12 NA values of AMT_ANNUITY with the mean
data$AMT_ANNUITY = replace(data$AMT_ANNUITY, is.na(data$AMT_ANNUITY), mean(data$AMT_ANNUITY, na.rm=TRUE))

#Replace 278 NA values of AMT_GOODS_PRICE with the mean
data$AMT_GOODS_PRICE = replace(data$AMT_GOODS_PRICE, is.na(data$AMT_GOODS_PRICE), mean(data$AMT_GOODS_PRICE, na.rm=TRUE))


#Changing the days of birth in years 
data$AGE = as.integer(trunc(abs(data$DAYS_BIRTH/365)))
data$YEARS_EMPLOYED = as.integer(trunc(abs(data$DAYS_EMPLOYED/365)))

#Removing rows that has employment years as 1000
check_data = subset(data, YEARS_EMPLOYED!=1000)


#Removing other rows that contain NA values
new_data = na.omit(check_data)
dim(new_data)

#Converting it into factors/categorical features
new_data$TARGET = as.factor(new_data$TARGET)
new_data$FLAG_DOCUMENT_3 = as.factor(new_data$FLAG_DOCUMENT_3)



#Creating the ggpairs plot
vars = c("NAME_CONTRACT_TYPE", "AMT_INCOME_TOTAL","AMT_ANNUITY", "AMT_GOODS_PRICE", "FLAG_DOCUMENT_3", "AMT_CREDIT")

ggpairs(new_data, columns = vars, columnLabels = c("Loan_Type","Total_Income", "Loan_Annuity", 
                                                   "Price_of_Goods", "Document_3", "Credit Score"))

#Splitting the data into training and testing data
split_data = createDataPartition(y = new_data$NAME_CONTRACT_TYPE, p=0.75, list=FALSE)
train = new_data[split_data,]
test = new_data[-split_data,]

#Creating the classification tree
fit = rpart(NAME_CONTRACT_TYPE ~ AMT_INCOME_TOTAL+AMT_GOODS_PRICE+
              AMT_ANNUITY+FLAG_DOCUMENT_3+AMT_CREDIT,
            method = "class",data=train)

#Plotting the tree and checking the summary of the tree
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=1)
printcp(fit)
summary(fit)

#Testing the classification tree on the testing set
predictions = predict(fit, newdata = test, type = "class")

#Creating the confustion Matrix
confusion_matrix = table(predictions, test$NAME_CONTRACT_TYPE)
print(confusion_matrix)

#First example
pred1 = predict(fit, data.frame(AMT_INCOME_TOTAL = 67500, AMT_GOODS_PRICE = 513000,
                                AMT_ANNUITY = 29000 , FLAG_DOCUMENT_3 = "1", AMT_CREDIT=12000), 
                type="class")
print(pred1)



#Selecting the data
data_6 = select(new_data, c("NAME_CONTRACT_TYPE", "AMT_INCOME_TOTAL","AMT_ANNUITY","AMT_GOODS_PRICE", "FLAG_DOCUMENT_3", "AMT_CREDIT"))
dim(data_6)

#Implementing Factor Analysis of Mixed Data
res = FAMD(data_6, graph=FALSE)
summary(res)

#Creating the model based on the importance of the variables obtained from FAMD
model = glm(AMT_INCOME_TOTAL ~ I(log(AMT_GOODS_PRICE)) + I(AMT_ANNUITY^3)+ AMT_CREDIT + NAME_CONTRACT_TYPE, data = data_6)
summary(model)