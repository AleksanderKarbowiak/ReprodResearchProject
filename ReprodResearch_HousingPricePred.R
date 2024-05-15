################### INIT #########################
library(dplyr)
library(ecce) #for translation
library(geosphere)  #for distance calculation

#read csv as dataframe
housing_prices_data <- as.data.frame(read.csv("new.csv",fileEncoding="gbk", header = TRUE)) #fileEncoding='gbk' is chinese signs encoding

class(housing_prices_data)


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

compare_summaries <- cbind(fulldata_summary, nullRows_summary)

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

# Floor types translation
# 高 - High
# 未知 - Unknown
# 顶  - Top
# 低 - Low
# 底 - Bottom
# 中 - Medium

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
housing_prices_data_clean$buildingType <- as.factor(ifelse(housing_prices_data_clean$buildingType==1,"Tower",ifelse(housing_prices_data_clean$buildingType==2,"Bungalow",ifelse(housing_prices_data_clean$buildingType==3,"Plate and Tower","Plate"))))
housing_prices_data_clean$renovationCondition <- as.factor(ifelse(housing_prices_data_clean$renovationCondition==1,"Other",ifelse(housing_prices_data_clean$renovationCondition==2,"Rough",ifelse(housing_prices_data_clean$renovationCondition==3,"Simplicity","Hardcover"))))
housing_prices_data_clean$buildingStructure <- as.factor(ifelse(housing_prices_data_clean$buildingStructure==1,"Unknow",ifelse(housing_prices_data_clean$buildingStructure==2,"Mixed",ifelse(housing_prices_data_clean$buildingStructure==3,"Brick and wood",ifelse(housing_prices_data_clean$buildingStructure==4,"Brick and concrete",ifelse(housing_prices_data_clean$buildingStructure==5,"Steel","Steel-concrete composite"))))))
housing_prices_data_clean$floorType <- as.factor(housing_prices_data_clean$floorType)
housing_prices_data_clean$elevator <- as.factor(housing_prices_data_clean$elevator)
housing_prices_data_clean$fiveYearsProperty <- as.factor(housing_prices_data_clean$fiveYearsProperty)
housing_prices_data_clean$subway <- as.factor(housing_prices_data_clean$subway)
housing_prices_data_clean$district <- as.factor(housing_prices_data_clean$district)
housing_prices_data_clean$elevator <- as.factor(housing_prices_data_clean$elevator)

table(housing_prices_data_clean$district)
str(housing_prices_data_clean)

##New feature - average size of a room
housing_prices_data_clean$avgRoomSize <- housing_prices_data_clean$square/(housing_prices_data_clean$livingRoom + housing_prices_data_clean$drawingRoom + housing_prices_data_clean$kitchen + housing_prices_data_clean$bathRoom)

#########
##INFO:
## totalPrice is price (average price by sqrt * square) * 10 000
##Currency is yuan
########



                                                 
