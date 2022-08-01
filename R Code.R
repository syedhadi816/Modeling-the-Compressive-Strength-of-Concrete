#importing data
df <- read.csv('Concrete_Data.csv')

#loading library
library('ggplot2')



#visualizing data

# histogram of Target Variable
hist(df$Concrete.compressive.strength,
     main="Concrete Compressive Strength Distribution in Dataset",
     xlab="Concrete Compressive Strength",
     xlim=c(0,90),
     col="grey",
     freq=FALSE
)

#scatterplots
plot(df$Cement, df$Concrete.compressive.strength, main = "Compressive Strength vs Cement",
     xlab = "Cement", ylab = "concrete.compressive.strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Cement, data = df), col = "blue")


plot(df$Blast.Furnace.Slag, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Blast.Furnace.Slag",
     xlab = "Blast.Furnace.Slag", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Blast.Furnace.Slag, data = df), col = "blue")



plot(df$Fly.Ash, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Fly.Ash",
     xlab = "Fly.Ash", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Fly.Ash, data = df), col = "blue")



plot(df$Superplasticizer, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Superplasticizer",
     xlab = "Superplasticizer", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Superplasticizer, data = df), col = "blue")



plot(df$Water, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Water",
     xlab = "Water", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Water, data = df), col = "blue")


plot(df$Water, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Water",
     xlab = "Water", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Water, data = df), col = "blue")


plot(df$Fine.Aggregate, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Fine.Aggregate",
     xlab = "Fine.Aggregate", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Fine.Aggregate, data = df), col = "blue")


plot(df$Coarse.Aggregate, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Coarse.Aggregate",
     xlab = "Coarse.Aggregate", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Coarse.Aggregate, data = df), col = "blue")



plot(df$Age, df$Concrete.compressive.strength, 
     main = "Compressive Strength vs Age",
     xlab = "Age", 
     ylab = "Concrete Compressive Strength",
     pch = 19, frame = FALSE) +
  abline(lm(df$Concrete.compressive.strength ~ df$Age, data = df), col = "blue")


#Simple regression 
mod1 <- lm(Concrete.compressive.strength~., data=df)
summary(mod1)

#Studying Regression with Interactions 
mod2 <- lm(Concrete.compressive.strength~(.)^2, data=df)
summary(mod2)

#Multivariate Adaptive Regression Splines 
library(earth)
MARS <- earth(Concrete.compressive.strength~., df)
print(MARS)
summary(MARS)
plot(MARS, which = 1)

#MARS with Interactions 
MARS2 <- earth(Concrete.compressive.strength~., df, degree = 2)
print(MARS2)
summary(MARS2)
plot(MARS2, which = 1)




#Building a Random Forest Model for Concrete Compressive Strength 
library(randomForest)
#Using default mtry (2 variables tested at each split)
rf1 <- randomForest(Concrete.compressive.strength~., data=df, importance=TRUE)
rf1

plot(sqrt(rf$mse), main="RMSE vs #_of_Trees")

#Varying the number of variables tested at each split
j <- list()
mtry <- list()
for(i in 2:8) {
  rf <- randomForest(Concrete.compressive.strength~., data=df, importance=TRUE, mtry = i)
  rmse <- mean(sqrt(rf$mse))
  j <- c(j, rmse)
  mtry <- c(mtry, i)
}
#plotting results
plot(mtry, j, main = 'Number of variables at each split vs RMSE', 
     xlab = 'Number of Variables',
     ylab = 'RMSE')

#lowest rmse is obtained when mtry = 4

#Best Model
rf2 <- randomForest(Concrete.compressive.strength~., data=df, importance=TRUE, mtry = 4)
rf2

#92.3% of the variance is explained. 

#variables importance
importance(rf2)
varImpPlot(rf, main="Plot of Variable Importance")

#Most importance variables are Age, Cement, Water, Superplasticizer


#Random Forest offers the best results for predicting compressive strength of concrete. 

