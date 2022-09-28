#importing dataset
dataset = read.csv('Position_Salaries.csv')

#check do we have any null value in our dataset or no
print("Number of null values in each column:")
for(i in colnames(dataset))
{
  cat( i," : ",sum(is.na(dataset[i])), "\n")
}
dataset=dataset[2:3]
#we dont have any null value in this dataset
#Note: because for each position we have 1 specific level so instead of using position and 
# encoding that categorical data we will use level as our independent variable 
#fitiing simple linear regression to the whole dataset
linear_regressor = lm(formula = Salary~Level,data = dataset)
summary(linear_regressor)

#fitting polynomial regression model on the whole dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_regressor = lm(formula = Salary~.,data = dataset)
summary(poly_regressor)

#visualizing linear regreesion model
library(ggplot2)

#visualizing polynomial regression model
