library(dplyr)

#read csv as dataframe
housing_prices_data <- as.data.frame(read.csv("new.csv",fileEncoding="gbk", header = TRUE)) #fileEncoding='gbk' is chinese signs encoding

class(housing_prices_data)

#drop unnecessary columns
drop_cols <- c("url")
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



