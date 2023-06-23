#Members	
#Hafsa Ghuyoor Syed	19262 - 70636
#Javeria Malik	19292	- 70635
#Maryam Fatima	18817	- 70635
#Sheza Malik	18715	- 70635

##IMPORTING DATA## 

# Read in the three data sets 
churn_data <- read.csv("churn_data.csv") 
customer_data <- read.csv("customer_data.csv") 
internet_data <- read.csv("internet_data.csv") 

##MERGING DATA## 

# Join the datasets on the "customerId" variable 
merged_data <- merge(churn_data, customer_data, by = "customerID", all = TRUE) 
merged_data <- merge(merged_data, internet_data, by = "customerID", all = TRUE) 
print(merged_data) 
str(merged_data) 

##DATA CLEANING## 

# Checking for NAs and Null 
sum(is.na(merged_data)) #11 NA values 
summary(merged_data) #NA values in TotalCharges column 

boxplot(merged_data$TotalCharges, main = "Boxplot for Total Charges")
#use median to impute because of outliers

# Calculate the median of TotalCharges column 
median_total_charges <- median(merged_data$TotalCharges, na.rm = TRUE) 

# Replace the missing values in TotalCharges column with the median value 
merged_data$TotalCharges[is.na(merged_data$TotalCharges)] <- median_total_charges 

#Rechecking NA's 
sum(is.na(merged_data)) 

#Converting variables to factors  
merged_data$PhoneService <- as.factor(merged_data$PhoneService) 
merged_data$PaperlessBilling <- as.factor(merged_data$PaperlessBilling) 
merged_data$Churn <- as.factor(merged_data$Churn) 
merged_data$gender <- as.factor(merged_data$gender) 
merged_data$Partner <- as.factor(merged_data$Partner) 
merged_data$Dependents <- as.factor(merged_data$Dependents) 
merged_data$MultipleLines <- as.factor(merged_data$MultipleLines) 
merged_data$OnlineSecurity <- as.factor(merged_data$OnlineSecurity) 
merged_data$OnlineBackup <- as.factor(merged_data$OnlineBackup) 
merged_data$DeviceProtection <- as.factor(merged_data$DeviceProtection) 
merged_data$TechSupport <- as.factor(merged_data$TechSupport) 
merged_data$StreamingTV <- as.factor(merged_data$StreamingTV) 
merged_data$StreamingMovies <- as.factor(merged_data$StreamingMovies) 
merged_data$Contract <- as.factor(merged_data$Contract) 
merged_data$PaymentMethod <- as.factor(merged_data$PaymentMethod) 
merged_data$InternetService <- as.factor(merged_data$InternetService) 
merged_data$SeniorCitizen <- as.factor(merged_data$SeniorCitizen) 

#MODEL BUILDING 

#Binary Encoding - Recode factor levels to 0 and 1 
levels(merged_data$PhoneService) <- c(0, 1) 
levels(merged_data$PaperlessBilling) <- c(0, 1) 
levels(merged_data$Churn) <- c(0, 1) 
levels(merged_data$gender) <- c(0, 1) #female 0, male 1 
levels(merged_data$Partner) <- c(0, 1) 
levels(merged_data$Dependents) <- c(0, 1) 
levels(merged_data$MultipleLines) <- c(0, 0, 1) #3rd level 
levels(merged_data$OnlineSecurity) <- c(0, 0, 1) #3rd level 
levels(merged_data$OnlineBackup) <- c(0, 0, 1) #3rd level 
levels(merged_data$DeviceProtection) <- c(0, 0, 1) #3rd level 
levels(merged_data$TechSupport) <- c(0, 0, 1) #3rd level 
levels(merged_data$StreamingTV) <- c(0, 0, 1) #3rd level 
levels(merged_data$StreamingMovies) <- c(0, 0, 1) #3rd level 

##One Hot Encoding - Creating Dummy Variables from Categorical Vars## 

#First Categorical Variable was Contract 
unique <- unique(merged_data$Contract) 
unique

#Ctaegories: "Month-to-month", "One year", "Two year"
merged_data$One_year_Contract <- ifelse(merged_data$Contract == "One year", 1,0) 
merged_data$Two_year_Contract <- ifelse(merged_data$Contract == "Two year" ,1,0)  

#Second Categorical Variable was Payment Method 
unique <- unique(merged_data$PaymentMethod) 
unique

#Categories: "Mailed check", "Electronic check", "Credit card (automatic)","Bank transfer (automatic)" 
merged_data$Mailed_check <- ifelse(merged_data$PaymentMethod == "Mailed check", 1,0) 
merged_data$Electronic_check <- ifelse(merged_data$PaymentMethod == "Electronic check", 1,0) 
merged_data$Credit_card <- ifelse(merged_data$PaymentMethod == "Credit card (automatic)", 1,0) 

#Third Categorical Variable was Internet Service 
unique <- unique(merged_data$InternetService) 
unique 

#Categories: "DSL", "Fiber optic", "No" 
merged_data$DSL <- ifelse(merged_data$InternetService == "DSL", 1,0) 
merged_data$Nointernet <- ifelse(merged_data$InternetService == "No", 1,0) 

#Converting new cols into factor 
merged_data$One_year_Contract <- as.factor(merged_data$One_year_Contract) 
merged_data$Two_year_Contract <- as.factor(merged_data$Two_year_Contract) 
merged_data$Mailed_check <- as.factor(merged_data$Mailed_check) 
merged_data$Electronic_check <- as.factor(merged_data$Electronic_check) 
merged_data$Credit_card <- as.factor(merged_data$Credit_card) 
merged_data$DSL <- as.factor(merged_data$DSL)
merged_data$Nointernet <- as.factor(merged_data$Nointernet) 

#Removing extra columns 
str(merged_data) 

#To check col numbers 
cbind(column_no = seq_along(names(merged_data)), column_name = names(merged_data)) 

#To remove extra columns 
merged_data <- merged_data[, -c(1, 4, 6, 15)] 

##Randomizing data## 

#Set seed 
set.seed(123) 

#Assign random integers to each row 
merged_data$rand_int <- sample(1:nrow(merged_data)) 

#Sorting by random integer 
merged_data <- merged_data[order(merged_data$rand_int, decreasing = FALSE),] 
str(merged_data)
merged_data <- merged_data[, -c(25)] 

write.csv(merged_data, "merged_data.csv") 

##LOGISTIC REGRESSION##

#Split data into training and testing sets 
library(caret) 
train_data <- merged_data[1:5282,] #75% of data is train 
test_data <- merged_data[5283:7042,] 

#Train a logistic regression model through k-fold cross validation  
control <- trainControl(method="cv", number=10) 
model_cv <- train(Churn ~ .,data = train_data, method='glm', family = "binomial", trControl= control) 
summary(model_cv) 

#Running logistic regression 
model <- glm(formula = Churn~., 
             data = train_data,
             family = "binomial") 

#Step wise logistic regression  
model2 <- step(model) 
summary(model2) 

#Continuing with model2 

#Getting predictions 
trainpreds <- model2$fitted.values 
testpreds <- predict(model2,
                     test_data,
                     type = "response")  

allpreds <- c(trainpreds, testpreds) 

write.csv(allpreds, "projectpreds.csv") 

##ROC AND PR Curves for Logistic Regression Model## 

#Training Data Curve s
library(PRROC) 
train_roc <- roc.curve(scores.class0 = trainpreds, #predicted probabilities        
                       weights.class0 = as.numeric(as.character(train_data$Churn)), #actual flag,
                       curve = T) 
print(train_roc) 
plot(train_roc) 

train_prcurve <- pr.curve(scores.class0 = trainpreds,  
                          weights.class0 = as.numeric(as.character(train_data$Churn)),  
                          curve = T) 
print(train_prcurve) 
plot(train_prcurve) 

#Testing Data Curves 
library(PRROC)
test_roc <- roc.curve(scores.class0 = testpreds, #predicted probabilities  
                      weights.class0 = as.numeric(as.character(test_data$Churn)), #actual flag,  
                      curve = T) 
print(test_roc) 
plot(test_roc) 

test_prcurve <- pr.curve(scores.class0 = testpreds,  
                         weights.class0 = as.numeric(as.character(test_data$Churn)),  
                         curve = T) 
print(test_prcurve) 
plot(test_prcurve)

##CUSTOMER SEGMENTATION - DECISION TREES##

library(rpart) 
library(rpart.plot) 
model_dt <- rpart(formula = Churn~.,   
                  data = train_data, 
                  control =  rpart.control(minbucket = 10, cp=0.004)) #minbucket has no impact 
model_dt 
rpart.plot(model_dt) 
summary(model_dt) 

##Decision tree predictions## 

trainpreds_dt <- predict(model_dt, 
                         train_data, 
                         type = "class") 

testpreds_dt <- predict(model_dt, 
                        test_data, 
                        type = "class") 

#Training Data curves 

library(PRROC) 
train_roc_dt <- roc.curve(scores.class0 = trainpreds_dt, #predicted probabilities 
                          weights.class0 = as.numeric(as.character(train_data$Churn)), #actual flag,  
                          curve = T)

print(train_roc_dt) 
plot(train_roc_dt) 

train_prcurve_dt <- pr.curve(scores.class0 = trainpreds_dt,  
                             weights.class0 = as.numeric(as.character(train_data$Churn)),  
                             curve = T)

print(train_prcurve_dt) 
plot(train_prcurve_dt) 

#Testing Data curves 
library(PRROC) 
test_roc_dt <- roc.curve(scores.class0 = testpreds_dt, #predicted probabilities  
                         weights.class0 = as.numeric(as.character(test_data$Churn)), #actual flag,  
                         curve = T)

print(test_roc_dt) 
plot(test_roc_dt) 

test_prcurve_dt <- pr.curve(scores.class0 = testpreds,  
                            weights.class0 = as.numeric(as.character(test_data$Churn)),  
                            curve = T) 

print(test_prcurve_dt) 
plot(test_prcurve_dt)