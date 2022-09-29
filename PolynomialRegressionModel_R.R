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
ggplot()+
  geom_point(aes(x = dataset$Level,y = dataset$Salary),colour='blue')+
  geom_line(aes(x = dataset$Level,y = predict(linear_regressor,newdata = dataset)),colour='red')+
  ggtitle("Simple Linear Regression")+
  xlab('Levels')+
  ylab('Salary')
#visualizing polynomial regression model
ggplot()+
  geom_point(aes(x = dataset$Level,y = dataset$Salary),colour='blue')+
  geom_line(aes(x = dataset$Level,y = predict(poly_regressor,newdata = dataset)),colour='red')+
  ggtitle("Polynomial Regression")+
  xlab('Levels')+
  ylab('Salary')

#predicting a new result by using linear regression model
y_linear_pred = predict(linear_regressor,data.frame(Level = 6.5))
print(y_linear_pred)
#predictiong a new result by using polynomiad regression model
y_polynomial_pred = predict(poly_regressor,data.frame(Level = 6.5,
                                                      Level2 =6.5^2,
                                                      Level3 = 6.5 ^3,
                                                      Level4 = 6.5^4))
print(y_polynomial_pred)
