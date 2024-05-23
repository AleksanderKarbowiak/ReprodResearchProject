################### INIT #########################
library(dplyr)
library(ecce) #for translation
library(geosphere)  #for distance calculation
library(ggplot2)
library(corrplot)
library(reshape2)
library(caret)
library(randomForest)

#set directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read csv as dataframe
housing_prices_data <- as.data.frame(read.csv("new.csv",fileEncoding="gbk", header = TRUE)) #fileEncoding='gbk' is chinese signs encoding

class(housing_prices_data)
head(housing_prices_data)
########### Data cleaning #################

#drop unnecessary columns
drop_cols <- c("url","id")
housing_prices_data <- housing_prices_data[ , !(names(housing_prices_data) %in% drop_cols)]

#summary of all cols
fulldata_summary <- as.data.frame(summary(housing_prices_data)) %>% filter(!is.na(Freq))

# finds the count of missing values as % of the whole dataset
colMeans(is.na(housing_prices_data))*100

#Variable DOM has null values in 49.5% of observations. It will be dropped.

housing_prices_data <- housing_prices_data[ , !(names(housing_prices_data) %in% "DOM")]

#Now we are going to check how many rows have missing values

obs_with_nulls <- housing_prices_data[!complete.cases(housing_prices_data),]

# check summary of obs_with_nulls if they differs from whole set summary
nullRows_summary <- as.data.frame(summary(obs_with_nulls)) %>% filter(!is.na(Freq))

#compare_summaries <- cbind(fulldata_summary, nullRows_summary)

#Distributions of full dataset and null-rows-dataset are similar. Also there is only 2403 obs of rows with null values. 
#Then we can delete rows with NULLs.
housing_prices_data_clean <- na.omit(housing_prices_data)


################### Data preprocessing and feature engineering #########################

summary(housing_prices_data_clean)
str(housing_prices_data_clean)
#converting some variables to number
typeof(housing_prices_data_clean$floor)
housing_prices_data_clean$livingRoom <- as.integer(housing_prices_data_clean$livingRoom)
housing_prices_data_clean$drawingRoom <- as.integer(housing_prices_data_clean$drawingRoom)
housing_prices_data_clean$bathRoom <- as.integer(housing_prices_data_clean$bathRoom)

#Division of signs and numbers (floor type and height)
housing_prices_data_clean$floorType <- substring(housing_prices_data_clean$floor,1,2)
housing_prices_data_clean$floorNum <- as.integer(substring(housing_prices_data_clean$floor,3,length(housing_prices_data_clean$floor)-2))
housing_prices_data_clean <- housing_prices_data_clean[ , !(names(housing_prices_data_clean) %in% "floor")]

#housing_prices_data_clean$floorTypeENG <- translate(housing_prices_data_clean$floorType, from = "auto", to = "auto")
#we need API key so we wont translate the floor types

# Floor types translation - is like a group of floorNum then it can be removed
# 高 - High
# 未知 - Unknown
# 顶  - Top
# 低 - Low
# 底 - Bottom
# 中 - Medium

# drop floor type and cid
columns_to_drop <- c("Cid","floorType")
housing_prices_data_clean <- housing_prices_data_clean[ , !(names(housing_prices_data_clean) %in% columns_to_drop)]

#Calculation of distance between home and Beijing city center (Forbidden City coordinates in our case)
BeijingCenterLat <- 39.91690639140218
BeijingCenterLng <- 116.39716443298232
#Haversine distance to get result in kilometers
housing_prices_data_clean$distance <- distHaversine(p1=housing_prices_data_clean[,c("Lng","Lat")],c(BeijingCenterLng,BeijingCenterLat))/1000

#Age -> construction time - current year
current_year <- as.numeric(format(Sys.Date(),"%Y"))

#Check frequency
table(housing_prices_data_clean$constructionTime)

#If unknown then use "Average" OR Maybe we should get rid off AGE/Construction Time variable
meanConcstrTime <- round(mean(as.integer(housing_prices_data_clean$constructionTime),na.rm = TRUE))

housing_prices_data_clean$age <- ifelse(housing_prices_data_clean$constructionTime=='未知',current_year-meanConcstrTime,current_year - as.integer(housing_prices_data_clean$constructionTime))
housing_prices_data_clean <- housing_prices_data_clean[ , !(names(housing_prices_data_clean) %in% "constructionTime")]

##changing numeric to categories factors
housing_prices_data_clean$buildingType <- ifelse(housing_prices_data_clean$buildingType==1,"Tower",ifelse(housing_prices_data_clean$buildingType==2,"Bungalow",ifelse(housing_prices_data_clean$buildingType==3,"Plate and Tower","Plate")))
#Bungalow building Types will be deleted from dataset since they are outliers and bungalow is completely different than other types
housing_prices_data_clean <- housing_prices_data_clean[housing_prices_data_clean$buildingType != "Bungalow", ]
housing_prices_data_clean$buildingType <- as.factor(housing_prices_data_clean$buildingType)

housing_prices_data_clean$renovationCondition <- ifelse(housing_prices_data_clean$renovationCondition==1,"Other",ifelse(housing_prices_data_clean$renovationCondition==2,"Rough",ifelse(housing_prices_data_clean$renovationCondition==3,"Simplicity","Hardcover")))
# Rough renovation condition will be assign to Hardcover
housing_prices_data_clean$renovationCondition <- as.factor(ifelse(housing_prices_data_clean$renovationCondition=="Rough","Hardcover",housing_prices_data_clean$renovationCondition))
  
housing_prices_data_clean$buildingStructure <- ifelse(housing_prices_data_clean$buildingStructure==1,"Unknow",ifelse(housing_prices_data_clean$buildingStructure==2,"Mixed",ifelse(housing_prices_data_clean$buildingStructure==3,"Brick and wood",ifelse(housing_prices_data_clean$buildingStructure==4,"Brick and concrete",ifelse(housing_prices_data_clean$buildingStructure==5,"Steel","Steel-concrete composite")))))
##Dealing with outliers
housing_prices_data_clean$buildingStructure <- as.factor(ifelse(housing_prices_data_clean$buildingStructure=="Steel","Steel-concrete composite",ifelse(housing_prices_data_clean$buildingStructure=="Brick and wood","Mixed",ifelse(housing_prices_data_clean$buildingStructure=="Unknow","Mixed",housing_prices_data_clean$buildingStructure))))

#housing_prices_data_clean$floorType <- as.factor(housing_prices_data_clean$floorType)

housing_prices_data_clean$elevator <- ifelse(housing_prices_data_clean$elevator==1,"Elevator","noElevator")
housing_prices_data_clean$elevator <- as.factor(housing_prices_data_clean$elevator)
housing_prices_data_clean$fiveYearsProperty <- ifelse(housing_prices_data_clean$fiveYearsProperty==1,"isFiveYProp","noFiveYProp")
housing_prices_data_clean$fiveYearsProperty <- as.factor(housing_prices_data_clean$fiveYearsProperty)
housing_prices_data_clean$subway <- ifelse(housing_prices_data_clean$subway==1,"Subway","NoSubway")
housing_prices_data_clean$subway <- as.factor(housing_prices_data_clean$subway)
housing_prices_data_clean$district <- as.factor(housing_prices_data_clean$district)

#char to Date
housing_prices_data_clean$tradeTime <- as.Date(housing_prices_data_clean$tradeTime)

table(housing_prices_data_clean$district)
str(housing_prices_data_clean)

##New feature - average size of a room
housing_prices_data_clean$avgRoomSize <- housing_prices_data_clean$square/(housing_prices_data_clean$livingRoom + housing_prices_data_clean$drawingRoom + housing_prices_data_clean$kitchen + housing_prices_data_clean$bathRoom)

housing_prices_data_clean$totalPrice <- housing_prices_data_clean$totalPrice * 10000 #real scale



#########
##INFO:
## totalPrice is price (average price by sqrt * square) * 10 000
##Currency is yuan
########

#checking factors counts
lapply(housing_prices_data_clean[sapply(housing_prices_data_clean,is.factor)],table)
##
#
# $BuildingType = "Bungalow" is an outlier.
# $renovationCondition = "Rough" is an outlier
# $BuildingStructure = "Brick and wood", "Steel" and "Unknown" are outliers.
#
##

##Dealing with outliers
#housing_prices_data_clean$buildingStructure <- ifelse(housing_prices_data_clean$buildingStructure=="Steel","Steel-concrete composite",ifelse(housing_prices_data_clean$buildingStructure=="Brick and wood","Mixed",ifelse(housing_prices_data_clean$buildingStructure=="Unknow","Mixed",housing_prices_data_clean$buildingStructure)))


#density of totalPrice
ggplot(housing_prices_data_clean, aes(x = totalPrice)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()

ggplot(housing_prices_data_clean, aes(x = followers)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()

ggplot(housing_prices_data_clean, aes(x = floorNum)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()

ggplot(housing_prices_data_clean, aes(x = communityAverage)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()

ggplot(housing_prices_data_clean, aes(x = square)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()
               
ggplot(housing_prices_data_clean, aes(x = price)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()                   

boxplot(housing_prices_data_clean$totalPrice)

#Looking for outliers in numerical variables using IQR
q<-NULL
iqr<-NULL
upper<-NULL
lower<-NULL
IQRcutData <- function(data, lower_quantile = 0.25, upper_quantile = 0.75)
{
  q <<- quantile(data, probs=c(lower_quantile, upper_quantile),na.rm=TRUE)
  iqr <<- q[2]-q[1]
  upper <<- q[2] + 1.5*iqr
  lower <<-q[1] - 1.5*iqr
  #outliers_totalPrice <- data > upper | data < lower
  
  data_cut <- data
  data_cut[data < lower] <-lower
  data_cut[data > upper] <- upper
  
  return(data_cut)
  
}



########################################################## Outliers IQR
cut_totalPrice <-IQRcutData(housing_prices_data_clean$totalPrice)
housing_prices_data_clean$totalPrice <- cut_totalPrice

cut_followers <- IQRcutData(housing_prices_data_clean$followers)
housing_prices_data_clean$followers <- cut_followers

cut_floorNum <- IQRcutData(housing_prices_data_clean$floorNum)
housing_prices_data_clean$floorNum <- cut_floorNum

cut_communityAverage <- IQRcutData(housing_prices_data_clean$communityAverage)
housing_prices_data_clean$communityAverage <- cut_communityAverage

cut_square <- IQRcutData(housing_prices_data_clean$square)
housing_prices_data_clean$square <- cut_square

cut_price <- IQRcutData(housing_prices_data_clean$price)
housing_prices_data_clean$price <- cut_price
#######################################################################

save(housing_prices_data_clean, file="housing_prices_data_clean.Rdata")

##### Data Analysis


#Correlation matrix for numerics
corrData <- housing_prices_data_clean
drop_colsCorr2 <- c("tradeTime","Cid","Lng","Lat","floorType")
drop_colsCorr <- c("Cid","Lng","Lat","tradeTime","buildingType","renovationCondition","buildingStructure","floorType","elevator","fiveYearsProperty","subway","district")
corrData <- corrData[ , !(names(corrData) %in% drop_colsCorr)]

str(corrData)

res <- cor(corrData)
corrplot(res,method="number")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE , number.cex = 0.4, tl.cex = 0.5
)


#################
# Anova test for factor variables 
data_factor_vars <- 
  sapply(housing_prices_data_clean, is.factor) %>% 
  which() %>% 
  names()

data_F_anova <- function(factor_var) {
  anova_ <- aov(housing_prices_data_clean$totalPrice ~ 
                  housing_prices_data_clean[[factor_var]]) 
  
  return(summary(anova_))
}

sapply(data_factor_vars,
       data_F_anova) -> data_anova_all_categorical

data_anova_all_categorical

#################

#One-hot encoding - correlation matrix of all vars

#housing_prices_data_clean <- housing_prices_data_clean[ , !(names(housing_prices_data_clean) %in% drop_colsCorr2)]
# Convert factor variables to dummy variables
#dummy_vars <- lapply(housing_prices_data_clean[, sapply(housing_prices_data_clean, is.factor)], function(x) model.matrix(~ x - 1, data = housing_prices_data_clean))

# Combine dummy variables with numeric variables
#housing_prices_data_clean_onehot <- cbind(housing_prices_data_clean[, !sapply(housing_prices_data_clean, is.factor)], do.call(cbind, dummy_vars))
# Calculate correlation matrix
#correlation_matrix <- cor(housing_prices_data_clean_onehot)

# Visualize correlation matrix as heatmap
#heatmap(correlation_matrix, symm = TRUE)
#ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
#  geom_tile() +
#  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name="Correlation") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  labs(title = "Correlation Plot")


### Add trade time
#load("housing_prices_data_clean.RData")
#restoring tradeTime
#housing_prices_data_clean_onehot$tradeTime <- housing_prices_data_clean$tradeTime

## Data for modeling

#str(housing_prices_data_clean_onehot)

# Custom function to standardize numeric and integer columns
#standardize_cols <- function(x) {
#  if (is.numeric(x) || is.integer(x)) {
#    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
#  } else {
#    return(x)
#  }
#}

# Standardize numeric and integer columns
#housing_prices_data_clean_stand <- housing_prices_data_clean %>%
#  mutate(across(where(is.numeric) | where(is.integer), standardize_cols))
#one-hot encoding once again

# Convert factor variables to dummy variables
#dummy_vars <- lapply(housing_prices_data_clean_stand[, sapply(housing_prices_data_clean_stand, is.factor)], function(x) model.matrix(~ x - 1, data = housing_prices_data_clean_stand))

# Combine dummy variables with numeric variables
#housing_prices_data_clean_stand_onehot <- cbind(housing_prices_data_clean_stand[, !sapply(housing_prices_data_clean_stand, is.factor)], do.call(cbind, dummy_vars))

#save(housing_prices_data_clean_stand_onehot, file="housing_prices_data_clean_stand_onehot.Rdata")




# one-hot encoding for ordinal factors
options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors

### Splitting the dataset fot train and test

set.seed(123456789)

data_which_train <- createDataPartition(housing_prices_data_clean$totalPrice , # target variable
                                        
                                        p = 0.8, 
                                        list = FALSE) 

data_train <- housing_prices_data_clean[data_which_train,]
data_test <- housing_prices_data_clean[-data_which_train,]

# function for the model evaluation
regressionMetrics <- function(data,
                              lev = NULL,
                              model = NULL) {
  real <- data$obs
  predicted <- data$pred
  
  # Mean Square Error
  MSE <- mean((real - predicted)^2)
  # Root Mean Square Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - predicted))
  # Mean Absolute Percentage Error
  MAPE <- mean(abs(real - predicted)/real)
  # Median Absolute Error
  MedAE <- median(abs(real - predicted))
  # Mean Logarithmic Absolute Error
  MSLE <- mean((log(1 + real) - log(1 + predicted))^2)
  # Root Mean Squared Logarithmic Error (RMSLE)
  RMSLE <- sqrt(MSLE)
  # R2
  R2 <- cor(predicted, real)^2
  
  result <- c(MSE = MSE, RMSE = RMSE, MAE = MAE, MAPE = MAPE, MedAE = MedAE,
              MSLE = MSLE, RMSLE = RMSLE, R2 = R2)
  return(result)
}

# 10-fold cross validation 
ctrl_cv10 <- trainControl(method = "cv",
                          number = 10,
                          savePredictions = "final",
                          summaryFunction = regressionMetrics)

###Random Forest

set.seed(123456789)
data_rf <- 
  train(totalPrice ~ ., 
        data = data_train,
        method = "ranger",
        num.trees = 900,
        num.threads = 10,
        importance = "impurity",
        preProcess = c("center", "scale"),
        trControl = ctrl_cv10,
        tuneGrid = expand.grid(mtry = 20,
                               splitrule = "variance",
                               min.node.size = 100))

data_rf

# MAPE - 0.255103
# RMSLE - 0.159295


tibble(
  pred = predict(data_rf, data_test),
  actual = data_test$totalPrice
) %>% 
  ggplot(aes(pred, actual)) +
  geom_point(col = "pink") +
  geom_smooth(method = "lm") +
  labs(title = "Price Predictions using Random Forest", x = "Random Forest Prediction", y = "Total Price") +
  theme_minimal()


### eXtreme Gradient Boosting
set.seed(123456789)

data_xgboost <- 
  train(totalPrice ~ ., 
        data = data_train,
        method = "xgbTree",
        preProcess = c("center", "scale"),
        trControl = ctrl_cv10,
        tuneGrid = expand.grid(nrounds = 200,           
                               max_depth = 5,          
                               eta = 0.1,              
                               gamma = 0.5,            
                               colsample_bytree = 0.8,
                               min_child_weight = 2,   
                               subsample = 1))

data_xgboost

#MAPE - 0.08307353
#RMSLE - NaN

tibble(
  pred = predict(data_xgboost, data_test),
  actual = data_test$totalPrice
) %>% 
  ggplot(aes(pred, actual)) +
  geom_point(col = "pink") +
  geom_smooth(method = "lm") +
  labs(title = "Price Predictions using eXtreme Gradient Boosting", x = "eXtreme Gradient Boosting Prediction", y = "Total Price") +
  theme_minimal()
