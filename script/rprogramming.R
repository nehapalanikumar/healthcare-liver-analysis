# Healthcare Data Analysis Using R
#
# This script demonstrates an end-to-end data analytics workflow,
# including data import, data preparation, data cleaning,
# feature engineering, exploratory data analysis (EDA),
# statistical summarisation and data visualisation.
#
# Skills demonstrated:
# - R Programming
# - Data Cleaning and Preparation
# - Missing Value Treatment
# - Duplicate Detection and Removal
# - Outlier Detection (IQR Method)
# - Data Transformation and Feature Engineering
# - Exploratory Data Analysis (EDA)
# - Statistical Analysis
# - Data Visualisation using ggplot2
# - Data Quality Assessment
# - Reproducible Data Analytics Workflows
#
# Tools and Libraries:
# - RStudio
# - tidyverse
# - dplyr
# - ggplot2

# load the libraries
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

#load dataset
mydata = read.csv("indian_liver_patient")

# Data Cleaning and Preparation
# variable, data type consistencies and dimensions check
sapply(mydata, class)
sapply(mydata, function(col) length(unique(sapply(col, class))) == 1)
dim(mydata)

# checking and omitting rows with missing values
sum(is.na(mydata))
mydata = na.omit(mydata)
#checking and removing rows of duplicated data (assuming they are miswritten inputs)
sum(duplicated(mydata))
mydata = mydata %>% distinct()

# right join of Globulin column calculated from 2 other variables
mydata = mydata %>%
  mutate(mydata, Globulin = Albumin * Albumin_and_Globulin_Ratio)
# deletes dataset column
mydata = subset(mydata, select = -Dataset)
#excludes age column
exclude_col = c("age")
# checking and removing any outliers asuming they are mistypes
check = setdiff(names(mydata)[sapply(mydata, is.numeric)], exclude_col)
is_outlier = function(out) {
	q1 = quantile(out, 0.25, na.rm = TRUE)
	q3 = quantile(out, 0.75, na.rm = TRUE)
	iqr =  q3-q1
	out <(q1-1.5*iqr) | out > (q3+1.5*iqr)
}
out_matrix = sapply(mydata[check], is_outlier)
rows_with_out = apply(out_matrix, 1, any)
mydata_cleaned = mydata[!rows_with_out, ]

# Data Visualisation and Descriptive Analysis
# averages and standard deviation of cleaned dataset
summary(mydata_cleaned$age)
sd(mydata_cleaned$age)

# Albumin and globulin ratio boxplot
boxplot(
  mydata_cleaned$Albumin_and_Globulin_Ratio, horizontal=TRUE, 
  col='#00FF00', main = "Boxplot"
)

# Bivariate line graph: instances comparing frequency of both gender attributes male nd female
ggplot(mydata_cleaned, aes(x = Age, color = Gender)) +
  geom_line(stat = "count") +
  labs(y = "Frequency", title = "Frequency of Age, by Gender")

# Bivariate scatter graph: Total bilirubin and Direct bilirubin
ggplot(mydata_cleaned, aes(x = Total_Bilirubin, y = Direct_Bilirubin)) +
	geom_point() +
	labs(title = "Total vs Direct Bilirubi")

# Barchart for mean values of enzymes
bar = mydata_cleaned %>%
	select(Alkaline_Phosphotase, Alamine_Aminotransferase,Aspartate_Aminotransferase) %>%
	gather(key = "Enzyme", value = "Value")
ggplot(bar, aes(x = Enzyme, y = Value, fill = Enzyme)) + 
geom_bar(stat = "summary", fun = "mean") +
labs(title = "Comparison of ALP, ALT, and AST", x= "Enzyme", y= "Mean Value")
