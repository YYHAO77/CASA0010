# Load package
library(readxl)
library(dplyr)
library(zoo)
library(stats)
library(ggplot2)
library(car)
library(glmnet)
library(corrplot)
library(factoextra)
library(modelr)
library(pheatmap)
library(caret)
library(randomForest)


##################Data clean and merge
##1
# For electricity consumption data
# Read data

LSOA_electricity_consumption <- read_excel("LSOA_domestic_elec_2010-21.xlsx", 
                                         sheet = "2021")

# Select London area
LSOA_electricity_consumption <- LSOA_electricity_consumption[28014:32848, ]

# Check dimension
dim(LSOA_electricity_consumption)

# Select useful column
LSOA_electricity_consumption <- LSOA_electricity_consumption[, c(1, 2, 5, 6, 8, 9)]

# Rename column
colnames(LSOA_electricity_consumption) <- c("Local authority code", "Local authority", "LSOA code",
                                       "Lower layer super output area", "Total consumption (kWh)", 'Mean consumption (kWh per meter)')


##2 Accommodation type
# For accommodation type 
# Read data
accommodation_type <- read_excel("accommodation type.xlsx", 
                                 sheet = "2021")

# Keep only the columns that need
accommodation_type <- accommodation_type[, c(1, 4:11)]

# setdiff(accommodation_type[ "LSOA code"], LSOA_electricity_consumption[, "LSOA code"])

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, accommodation_type, by = "LSOA code")


##3 Tenure households
# Read data 
tenure_households <- read_excel("tenure - households.xlsx", sheet = "2021")

# Select the columns that need
tenure_households <- tenure_households[, c(1, 5:12)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, tenure_households, by = "LSOA code")


##4 Occupation data
# Read data
occupation <- read_excel("occupation.xlsx", sheet = "2021")

# Select columns
occupation <- occupation[, 3:13]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, occupation, by = "LSOA code")


##5
# Occupancy rating bedrooms 
# Read data 
occupancy_rating_bedrooms <- read_excel("occupancy rating - bedrooms.xlsx", sheet = "2021")

# Select columns
occupancy_rating_bedrooms <- occupancy_rating_bedrooms[, c(1, 5:9)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, occupancy_rating_bedrooms, by = "LSOA code")


##6
# Number of rooms 
# Load data 
number_of_rooms <- read_excel("number of rooms.xlsx", sheet = "2021")

# Select columns
number_of_rooms <- number_of_rooms[, c(1, 5:13)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, number_of_rooms, by = "LSOA code")


##7
# Household composition
# Load data 
Household_composition <- read_excel("Household composition.xlsx", sheet = "2021")

# Select columns
Household_composition <- Household_composition[, c(1, 5:17)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, Household_composition, by = "LSOA code")


## 8 Ethnic group
# Read data
Ethnic_group <- read_excel("Ethnic group.xlsx", sheet = "2021")

# Select columns
Ethnic_group <- Ethnic_group[, c(1, 5:23)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, Ethnic_group, by = "LSOA code")


##9 Five year age bands
# Read data
Five_year_age_bands <- read_excel("Five year age bands.xlsx", sheet = "2021")

# Select columns
Five_year_age_bands <- Five_year_age_bands[, c(1, 5:22)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, Five_year_age_bands, by = "LSOA code")


## 10 Economic activity
# Read data
Economic_Activity <- read_excel("Economic Activity.xlsx", sheet = "2021")

# Select columns
Economic_Activity <- Economic_Activity[, c(3, 5:17)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, Economic_Activity, by = "LSOA code")


## 11 NSSEC (National Statistics Socio-Economic Classification)
# Read data
NSSEC <- read_excel("NSSEC.xlsx", sheet = "2021")

# Select columns
NSSEC <- NSSEC[, c(3, 5:12)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, NSSEC, by = "LSOA code")


## 12 Hours worked
# Read data
hours_worked <- read_excel("hours worked.xlsx", sheet = "2021")

# Select columns
hours_worked <- hours_worked[, c(3, 5:8)]

# Merge data
LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, hours_worked, by = "LSOA code")
dim(LSOA_electricity_consumption)

# ## 13 Method of travel to work
# # Read data
# Method_of_travel_to_work <- read_excel("Method of travel to work.xlsx", sheet = "2021")
# 
# # Select columns
# Method_of_travel_to_work <- Method_of_travel_to_work[, c(3, 5:12)]
# 
# # Merge data
# LSOA_electricity_consumption <- merge(LSOA_electricity_consumption, hours_worked, by = "LSOA code")

#Write data
write.csv(LSOA_electricity_consumption, file = "LSOA_electricity_consumption.csv")


# electricity_consumption <- round(electricity_consumption, 4)

################################################################################# Start Here

####### 3. The Data 

# Read data
LSOA_electricity_consumption <- read.csv("LSOA_electricity_consumption.csv")
LSOA_electricity_consumption<- LSOA_electricity_consumption[,-1]

# Check basic 
dim(LSOA_electricity_consumption)
summary(LSOA_electricity_consumption)
sum(is.na(LSOA_electricity_consumption))

####### 3.3 Data Pre-processing

####### 3.3.1 Basic Data Processing
# As numeric
LSOA_electricity_consumption[, 5:121] <- apply(LSOA_electricity_consumption[, 5:121], 2, as.numeric)
summary(LSOA_electricity_consumption)

# Digits
LSOA_electricity_consumption[, 5:121] <- round(LSOA_electricity_consumption[, 5:121], digits = 4)

# Check variables name
colnames(LSOA_electricity_consumption)

# Rename variables
colnames(LSOA_electricity_consumption)<- c("LSOA code", "Local authority code", "Local authority", "Lower layer super output area", "Total consumption kWh",
                                           "Mean consumption kWh per meter","All households (Accommodation type)", "Detached (Accommodation type)",
                                           "Semi detached (Accommodation type)", "Terraced (Accommodation type)", "Purpose built flat (Accommodation type)",
                                           "Flat in a converted or shared house (Accommodation type)", "Flat in a commercial building (Accommodation type)",
                                           "Caravan or other mobile or temporary structure (Accommodation type)", "Owned outright (Tenure household)",
                                           "Owned with a mortgage or loan (Tenure household)", "Shared ownership (Tenure household)", "Rented from Local Authority (Tenure household)",
                                           "Other social rented (Tenure household)", "Private landlord or letting agency (Tenure household)", "Other private rented (Tenure household)",
                                           "Rent free (Tenure household)", "All usual residents aged 16 and over in employment (Occupation)", "Managers directors and senior officials (Occupation)",
                                           "Professional occupations (Occupation)", "Associate professional and technical occupations (Occupation)", "Adminis trative and secretarial occupations (Occupation)",
                                           "Skilled trades occupations (Occupation)", "Caring leisure and other service occupations (Occupation)", "Sales and customer service occupations (Occupation)",
                                           "Process plant and machine operatives (Occupation)", "Elementary occupations (Occupation)", "Occupancy rating +2 (Occupancy bedroom)",
                                           "Occupancy rating +1 (Occupancy bedroom)", "Occupancy rating +0 (Occupancy bedroom)", "Occupancy rating -1 (Occupancy bedroom)", "Occupancy rating -2 (Occupancy bedroom)",
                                           "1 Room (number of rooms)", "2 Rooms (number of rooms)", "3 Rooms (number of rooms)", "4 Rooms (number of rooms)", "5 Rooms (number of rooms)",
                                           "6 Rooms (number of rooms)", "7 Rooms (number of rooms)", "8 Rooms (number of rooms)", "9+ Rooms (number of rooms)", "One person Aged 66+ (Household composition)",
                                           "One person Aged up to 65 (Household composition)", "Family all aged 66+ (Household composition)", "Married or civil partnership couple:No children (Household composition)" ,
                                           "Married or civil partnership couple:Dependent children (Household composition)", "Married or civil partnership couple:non dependent children (Household composition)",
                                           "Cohabiting couple:No children (Household composition)","Cohabiting couple:Dependent children (Household composition)", "Cohabiting couple:Non dependent children (Household composition)",
                                           "Lone parent:dependent children (Household composition)", "Lone parent:non dependent children (Household composition)", "Other with dependent children (Household composition)", 
                                           "All other types (Household composition)", "White British (Ethnic group)", "White Irish (Ethnic group)", "White Gypsy /Irish Traveller (Ethnic group)", "White Roma (Ethnic group)", 
                                           "White Other (Ethnic group)", "Mixed White and Asian (Ethnic group)", "Mixed White and Black African (Ethnic group)", "Mixed White and Black Caribbean (Ethnic group)",
                                           "Mixed Other (Ethnic group)", "Asian Bangladeshi (Ethnic group)", "Asian Chinese (Ethnic group)", "Asian Indian (Ethnic group)", "Asian Pakistani (Ethnic group)", "Asian Other (Ethnic group)",
                                           "Black African (Ethnic group)", "Black Caribbean (Ethnic group)", "Black Other (Ethnic group)", "Other Arab (Ethnic group)", "Other Any other (Ethnic group)", "Aged 4 and under (Five year age bands)",
                                           "Aged 5 to 9 (Five year age bands)", "Aged 10 to 14 (Five year age bands)", "Aged 15 to 19 (Five year age bands)", "Aged 20 to 24 (Five year age bands)", "Aged 25 to 29 (Five year age bands)",
                                           "Aged 30 to 34 (Five year age bands)", "Aged 35 to 39 (Five year age bands)", "Aged 40 to 44 (Five year age bands)", "Aged 45 to 49 (Five year age bands)", "Aged 50 to 54 (Five year age bands)", 
                                           "Aged 55 to 59 (Five year age bands)", "Aged 60 to 64 (Five year age bands)", "Aged 65 to 69 (Five year age bands)", "Aged 70 to 74 (Five year age bands)", "Aged 75 to 79 (Five year age bands)", 
                                           "Aged 80 to 84 (Five year age bands)", "Aged 85 and over (Five year age bands)", "Employee Full time (Economic activity)", "Employee Part time (Economic activity)", "Full time student (Economic activity)",
                                           "Self employed with employees Full time (Economic activity)", "Self employed with employees Part time (Economic activity)", "Self employed without employees Full time (Economic activity)", 
                                           "Self employed without employees Part time (Economic activity)", "Unemployed (Economic activity)", "Long term sick or disabled (Economic activity)", "Looking after home or family (Economic activity)",
                                           "Other (Economic activity)", "Economically inactive: Retired (Economic activity)","Economically inactive:  Full-time students (Economic activity)", "Higher managerial admin and professional (NSSEC)",
                                           "Lower managerial admin and professional (NSSEC)", "Intermediate (NSSEC)", "Small employers own account workers (NSSEC)", "Lower supervisory and technical (NSSEC)", "Semi routine (NSSEC)",
                                           "Routine (NSSEC)", "Never worked and long term unemployed (NSSEC)", "15 hours or less (Hours worked)", "16 to 30 hours (Hours worked)", "31 to 48 hours (Hours worked)", "49 or more hours (Hours worked)")

# col name
colnames(LSOA_electricity_consumption)

# check dimesion
dim(LSOA_electricity_consumption)

# check na
sum(is.na(LSOA_electricity_consumption))

####### 3.3.2 Outlier Handling

# Check the need to remove the outliers (the multiple model)
# Select Y
outlier_multiple_model_Y <- LSOA_electricity_consumption[, 6]

# Select X
outlier_multiple_model_X <- LSOA_electricity_consumption[, 7:121]

# The model
outlier_multiple_model <- lm(outlier_multiple_model_Y ~ ., data = outlier_multiple_model_X)

# get the output
outlier_multiple_model_summary<- capture.output(summary(outlier_multiple_model))

# Plot the residuals of a multiple linear model
par(mfrow = c(2, 2))
plot(outlier_multiple_model, which = 1:4)

# remove outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR
  
  x[x < lower_fence | x > upper_fence] <- NA
  
  return(x)
}

for (col in 5:121) {
  LSOA_electricity_consumption[, col] <- remove_outliers(LSOA_electricity_consumption[, col])
}

sum(is.na(LSOA_electricity_consumption))

summary(LSOA_electricity_consumption)

# Test for remove all outliers
LSOA_electricity_consumption1<- LSOA_electricity_consumption

LSOA_electricity_consumption1<- na.omit(LSOA_electricity_consumption1)
dim(LSOA_electricity_consumption1)

# Fill NA values with mean
for (col in colnames(LSOA_electricity_consumption[,5:121])) {
  col_mean <- mean(LSOA_electricity_consumption[, col], na.rm = TRUE)  
  LSOA_electricity_consumption[is.na(LSOA_electricity_consumption[, col]), col] <- col_mean 
}

sum(is.na(LSOA_electricity_consumption))
dim(LSOA_electricity_consumption)

####### 3.3.3 Data Standardization

# Scale data
Scaled_LSOA_electricity_consumption<- LSOA_electricity_consumption

Scaled_LSOA_electricity_consumption[,5:121] <- scale(Scaled_LSOA_electricity_consumption[,5:121])

####### 3.3.4 Multicollinearity Check

# VIF for outlier_multiple_model
vif(outlier_multiple_model) # Aliased coefficients exist

# check correlation without scale
plot_cor<- cor(LSOA_electricity_consumption[, 7:121])
plot_cor <- round(plot_cor, digits = 4)
class(plot_cor)
corrplot(plot_cor, method = "color", tl.cex = 0.2, tl.col = "black")

# look variables with high correlation
high_correlation <- which(plot_cor > 0.7 | plot_cor < -0.7, arr.ind = TRUE)

high_correlation<- as.data.frame(high_correlation)

high_correlation <- high_correlation[high_correlation$row!= high_correlation$col, ]

# check correlation with scale
plot_cor_scale<- cor(Scaled_LSOA_electricity_consumption[, 7:121])
plot_cor_scale <- round(plot_cor_scale, digits = 4)
corrplot(plot_cor_scale, method = "color", tl.cex = 0.2, tl.col = "black")

# look variables with high correlation
high_correlation_scale <- which(plot_cor_scale > 0.7 | plot_cor_scale < -0.7, arr.ind = TRUE)

####### 3.3.5 Dataset Segmentation

# Divide the data into training data and validation data
set.seed(123)

# for normal data
splitIndex <- createDataPartition(LSOA_electricity_consumption$`Mean consumption kWh per meter`, p = 0.75, list = FALSE)
train_data <- LSOA_electricity_consumption[splitIndex, ]
test_data <- LSOA_electricity_consumption[-splitIndex, ]

# for scaled data
splitIndex_scaled <- createDataPartition(Scaled_LSOA_electricity_consumption$`Mean consumption kWh per meter`, p = 0.75, list = FALSE)
train_data_scaled <- Scaled_LSOA_electricity_consumption[splitIndex_scaled, ]
test_data_scaled <- Scaled_LSOA_electricity_consumption[-splitIndex_scaled, ]

####### 3.3.6 Elimination of Variables

# get the correlation including y
cor_with_y <- cor(LSOA_electricity_consumption[,6:121])
cor_with_y<- abs(cor_with_y)
cor_with_y<- as.data.frame(cor_with_y)

# find out that the correlation is less than 0.3 (for `Mean consumption kWh per meter`)
cor_need_remove<- which(cor_with_y$`Mean consumption kWh per meter` < 0.3)
cor_need_remove

# do this in new dataset
Elimination_LSOA_electricity_consumption<- LSOA_electricity_consumption[,6:121]
Elimination_LSOA_electricity_consumption<- Elimination_LSOA_electricity_consumption[,-c(cor_need_remove)]
dim(Elimination_LSOA_electricity_consumption)

# also divide this data into training data and validation data
set.seed(123)

# for normal data
splitIndex <- createDataPartition(Elimination_LSOA_electricity_consumption$`Mean consumption kWh per meter`, p = 0.75, list = FALSE)
Elimination_train_data <- Elimination_LSOA_electricity_consumption[splitIndex, ]
Elimination_test_data <- Elimination_LSOA_electricity_consumption[-splitIndex, ]


###################################################################################
########## Ridge regression Model
set.seed(123)

# build model
# test data
test_Ridge_model_X<- test_data_scaled[, 7:121]
test_Ridge_model_Y<- test_data_scaled[, 6]

# Select Y
Ridge_model_Y <- Scaled_LSOA_electricity_consumption[, 6]

# Select X
Ridge_model_X <- Scaled_LSOA_electricity_consumption[, 7:121]
test_Ridge_model_X<- as.matrix(test_Ridge_model_X)

# the model
ridge_model <- glmnet(Ridge_model_X, Ridge_model_Y, alpha = 0) # alpha = 0 means select ridge regression
ridge_model

# get the output
ridge_model_framework<- capture.output(ridge_model)

# write.csv(ridge_model_framework, file = "ridge_model_framework.csv")

# Cross-validation, choose optimal ridge regression parameters
Ridge_model_X <- as.matrix(Ridge_model_X)
cv_ridge <- cv.glmnet(Ridge_model_X, Ridge_model_Y, alpha = 0)
print(cv_ridge)

# Lower point in the curve indicates the optimal lambda
plot(cv_ridge)

# Ridge Trace Plot
plot(cv_ridge$glmnet.fit, xvar = "lambda", label = TRUE)


# select best lambda
best_lambda <- 0.4194
best_lambda

# Final model
final_ridge_model <- glmnet(Ridge_model_X, Ridge_model_Y, alpha = 0, lambda = best_lambda)
final_ridge_model
summary(final_ridge_model)

# get coef()
final_ridge_model_coefficients<- coef(final_ridge_model)
print(final_ridge_model_coefficients)
final_ridge_model_coefficients<-round(final_ridge_model_coefficients, 4)
final_ridge_model_coefficients

final_ridge_model_coefficients<- as.matrix(final_ridge_model_coefficients)
head(final_ridge_model_coefficients)

y_predicted_1 <- predict(final_ridge_model, s = best_lambda, newx = test_Ridge_model_X)

# Sum of Squares Total and Error
sst_1 <- sum(test_Ridge_model_Y^2)
sse_1 <- sum((y_predicted_1 - test_Ridge_model_Y)^2)

# R squared
rsq_1 <- 1 - sse_1 / sst_1
rsq_1

# MSE
mse_1 <- mean((test_Ridge_model_Y - y_predicted_1)^2)
mse_1

######################### Random forest 
# set seed
set.seed(123)

# Change the variable name into a format that the random forest can execute
rf_train_data<- train_data
rf_test_data<- test_data

names(rf_train_data) <- make.names(names(rf_train_data))

# also test_data 
names(rf_test_data) <- make.names(names(rf_test_data))

# build the model
rf_model <- randomForest(Mean.consumption.kWh.per.meter ~ ., 
                         data = rf_train_data[, 6:121], importance = TRUE)

rf_model

# check accuracy for each number of tree
accuracy <- c()
for (i in 1:rf_model$ntree) {
  accuracy <- append(accuracy, rf_model$rsq[i])
}

accuracy

# visualization accuracy
plot(1:rf_model$ntree, accuracy, type = "l", xlab = "Number of Trees", ylab = "R-square")

# plot tree 
plot(rf_model)

# final model
final_rf_model <- randomForest(rf_train_data$Mean.consumption.kWh.per.meter ~ ., 
                         data = rf_train_data[, 6:121], importance = TRUE, ntree= 100)

final_rf_model
summary(final_rf_model)

# get feature importance
feature_importance <- final_rf_model[["importance"]]

feature_importance

# write.csv(feature_importance, file = "feature_importance.csv")

# visualization top 15 variables
varImpPlot(final_rf_model, n.var = min(15, nrow(final_rf_model$importance)), 
           main = 'Top 15 - variable importance')


y_predicted_2 <- predict(final_rf_model, newdata = rf_test_data[, 6:121])

# Sum of Squares Total and Error
sst_2 <- sum(rf_test_data$Mean.consumption.kWh.per.meter^2)
sse_2 <- sum((y_predicted_2 - rf_test_data$Mean.consumption.kWh.per.meter)^2)

# R squared
rsq_2 <- 1 - sse_2 / sst_2
rsq_2

# MSE
mse_2 <- mean((rf_test_data$Mean.consumption.kWh.per.meter - y_predicted_2)^2)
mse_2

# Find out why the MSE is too large, but the R square is close to 1
# Use whole scale data
Scaled_LSOA_electricity_consumption11<- Scaled_LSOA_electricity_consumption

# adjust col name
names(Scaled_LSOA_electricity_consumption11) <- make.names(names(Scaled_LSOA_electricity_consumption11))

# test model
y_predicted_2.1 <- predict(final_rf_model, newdata = Scaled_LSOA_electricity_consumption11[, 6:121])
mse_2.1 <- mean((Scaled_LSOA_electricity_consumption11$Mean.consumption.kWh.per.meter - y_predicted_2.1)^2)
mse_2.1

sst_2.1 <- sum(Scaled_LSOA_electricity_consumption11$Mean.consumption.kWh.per.meter^2)
sse_2.1 <- sum((y_predicted_2.1 - Scaled_LSOA_electricity_consumption11$Mean.consumption.kWh.per.meter)^2)

rsq_2.1 <- 1 - sse_2.1 / sst_2.1
rsq_2.1

# Remodeling prepare
Elimination_train_data1<- Elimination_train_data
Elimination_test_data1<- Elimination_test_data

# change variables names form
names(Elimination_train_data1) <- make.names(names(Elimination_train_data1))


# also test_data 
names(Elimination_test_data1) <- make.names(names(Elimination_test_data1))

# rename duplicate column names
colnames(Elimination_train_data1)[15] <- "Occupancy.rating..minus2..Occupancy.bedroom."
colnames(Elimination_test_data1)[15] <- "Occupancy.rating..minus2..Occupancy.bedroom."

# new model
rf_model_new <- randomForest(Elimination_train_data1$Mean.consumption.kWh.per.meter ~ ., 
                         data = Elimination_train_data1[,2:30], importance = TRUE)

rf_model_new

########################################### PCA
# select independent variable for pca
pca_result <- prcomp(train_data_scaled[,7:121])
pca_result

# Plot the Variance Contribution
fviz_eig(pca_result, addlabels = TRUE,ncp = 15)

# Plot about principal component variables (features)
fviz_pca_var_plot1<- fviz_pca_var(pca_result, select.var = list(cos2 = 0.5),
                                 col.var = "cos2", repel=T)
fviz_pca_var_plot1

# increase cos2 plot again
fviz_pca_var_plot2<- fviz_pca_var(pca_result, select.var = list(cos2 = 0.65), 
                                  col.var = "cos2", repel=T)
fviz_pca_var_plot2


# Top 15 variables with the highest cos2
fviz_pca_var_plot3<- fviz_pca_var(pca_result, select.var= list(cos2 = 15), repel=T, col.var = "contrib")

fviz_pca_var_plot3

# get variable detail
fviz_pca_var_plot3$data

# Choose the suitable number of PCs
summary(pca_result)

summary_pca_result<- summary(pca_result)
summary_pca_result<- summary_pca_result$importance

# get relevant information
summary_pca_result<- t(summary_pca_result)
summary_pca_result<- round(summary_pca_result, 4)
# write.csv(summary_pca_result, file = "summary_pca_result.csv")

# ML model
PCApc40 <- pca_result$x
PCApc40<- as.data.frame(PCApc40[,1:40])

Ypca<- as.data.frame(train_data_scaled$`Mean consumption kWh per meter`)

PCA_merged_data <- cbind(Ypca, PCApc40)

PCA_ml_model <- lm(train_data_scaled$`Mean consumption kWh per meter` ~ ., 
                   data = PCA_merged_data[, c(2:41)])

summary(PCA_ml_model)

# select desired information
summary_PCA_ml_model<- summary(PCA_ml_model)
round(summary_PCA_ml_model$coefficients,4)

# View the coefficient of the variable in pc
loadings<- abs(pca_result$rotation)

# model evaluation
pca_result_test <- prcomp(test_data_scaled[,7:121])
summary(pca_result_test)


test_PCApc38 <- pca_result_test$x
test_PCApc38<- as.data.frame(test_PCApc40[,1:38])

test_Ypca<- as.data.frame(test_data_scaled$`Mean consumption kWh per meter`)

test_PCA_merged_data <- cbind(test_Ypca, test_PCApc40)

# predict 
colnames(test_PCA_merged_data)[1]<- "y"

y_predicted_3 <- predict(PCA_ml_model, newdata = test_PCA_merged_data)

# Sum of Squares Total and Error
sst_3 <- sum(test_PCA_merged_data$y^2)
sse_3 <- sum((y_predicted_3 - test_PCA_merged_data$y)^2)

# R squared
rsq_3 <- 1 - sse_3/sst_3
rsq_3

# MSE
mse_3 <- mean((test_PCA_merged_data$y - y_predicted_3)^2)
mse_3


# # Check for variable duplication
# Table_5_7 <- read_excel("Table 5.7.xlsx")
# 
# sum(Table_5_7 == 'Terraced (Accommodation type)', na.rm = TRUE)


# # Prepare Matrix Multiplication
# # operate on pca results
# # select Proportion of Variance for each principal component
# summary_pca_result<- summary_pca_result[,2]
# summary_pca_result<- as.matrix(summary_pca_result)
# colnames(summary_pca_result)<- c('Proportion of Variance')
# 
# # Get the relationship between the principal components and the original variables
# PCAloadings<- abs(pca_result$rotation)
# 
# PCAloadings<- as.matrix(PCAloadings)
# PCAloadings<- round(PCAloadings, 4)
# # write.csv(PCAloadings, file = "PCAloadings.csv")
# vif(PCAloadings)
# 
# dim(PCAloadings)
# dim(summary_pca_result)
# 
# PCAFIANLresult <- PCAloadings %*% summary_pca_result
# colnames(PCAFIANLresult)<- c('Importance')
