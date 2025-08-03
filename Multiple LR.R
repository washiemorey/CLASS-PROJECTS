#...........Created by Washie Morey.........................#
...........On 30 April 2025.............................#
#...........Premiums Linear Regression Model..................#
#Importing data into R
library(readxl)
data <- read_excel("C:\\Users\\hp\\OneDrive\\Desktop\\3.2 Units\\Multiple LR.xlsx")

#Creating the model 
linear_model <- lm(Premium ~., data = data)

#Prerdicting premiums using the model 
pred_premium <- predict(linear_model,data)

#Creating Dataframe for Actual and predicted premiums
Data <- data.frame(Actual_Premium = data$Premium,Predicted_Premium = pred_premium)

#Plotting Actual and predicted premiums 
plot(Data$Actual_Premium,type = "l",lwd = 2,ylab = "Premium",
     main = "ACTUAL AND PREDICTED PREMIUMS VISUALIZATION", ylim = c(25,100))
points(Data$Predicted_Premium,type = "l",col = "red",lwd = 2)
abline(v = seq(1,12,1))

#Adding legend to the plot
legend("topright",
       legend = c("Actual","Predicted"),
       col = c("black", "red"),
       lty = 1,
       lwd = 2)

#Cheking for Error in the data 
library(caret)
library(e1071)
Error <- sum(abs(Data$Actual_Premium-Data$Predicted_Premium)^2)

postResample(Data$Actual_Premium,Data$Predicted_Premium)

plot(linear_model$fitted.values, resid(linear_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 19, col = "darkblue")
abline(h = 0, lty = 2, col = "red")

plot(resid(linear_model),linear_model$fitted.values,xlab = "Residuals",
     ylab = "Fitted Values",pch = 15)
abline(v = 0 , col = "red",lty = 2)


#Tranforming the data
# 1....Adding polynomial
Experience2 <- data$Experience^2

#New dataframe 
data1 <- cbind(data,Experience2)

#Building Model
linear_model1 <- lm(Premium ~.,data = data1)

plot(resid(linear_model),linear_model1$fitted.values,
     xlab = "Residuals",ylab = "Fitted Values",pch = 18)
abline(v = 0 , col = "red",lty = 2)

#..2 Log skewed predictor 
model_log <- lm(log(Premium) ~ Experience + Violations, data = data)
plot(resid(model_log),model_log$fitted.values,
     xlab = "Residuals",ylab = "Fitted Values",pch = 18)
abline(v = 0 , col = "red",lty = 2)

pred_Prem_log <- predict(model_log,data)
pred_prem <- exp(pred_Prem_log)
df <- data.frame(Actual = data$Premium,Predicted = pred_prem)




#....Interaction
model_interact <- lm(Premium ~ Experience * Violations, data = data)
