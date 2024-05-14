library(dplyr)

#read csv as dataframe
housing_prices_data <- as.data.frame(read.csv("new.csv",fileEncoding="gbk", header = TRUE)) #fileEncoding='gbk' is chinese signs encoding

#typeof(housing_prices_data)

#drop unnecessary columns
drop_cols <- c("url")
housing_prices_data <- housing_prices_data[ , !(names(housing_prices_data) %in% drop_cols)]



