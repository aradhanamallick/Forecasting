#SVR 
#data = read.csv('F://TRAFFIC/TRAFFIC24.csv')
#plot(data)
# linear model ==============================================
model <- lm(n ~ M, burg_ym)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

predictedY <- predict(model, burg_ym) 
#points(burg_ym$M, predictedY, col = "blue", pch=4)   


error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)# 5.703778
predictionRMSE
# end of linear model =======================================


plot(burg_ym)

# svr model ==============================================
#if(require(e1071)){
  
  
  model1 <- svm(n~M , burg_ym)
summary(model1)
  
  predictedY1 <- predict(model1, burg_ym)
  predictedY1
  points(burg_ym$M, predictedY1, col = "red", pch=17)
  
  
  error <- burg_ym$n - predictedY1  # /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
  svmPredictionRMSE <- rmse(error)  # 3.157061
  svmPredictionRMSE
  
  
  tuneResult <- tune(svm, burg_ym$n ~ burg_ym$M,  data = burg_ym, 
                     ranges = list(epsilon = seq(0,1,0.1), cost = 2^(0:2))
  ) 
  print(tuneResult) # best performance: MSE = 8.371412, RMSE = 2.89  epsilon  1e-04   cost 4
  
  # Draw the first tuning graph 
  plot(tuneResult) 
  
  # On the first tuning graph, we can see that the graph is darker on the leftside when epsilon is small,
  # so we adjust the tuning to go in this direction 
  
  # Draw the second tuning graph
  tuneResult <- tune(svm, burg_ym$n ~ burg_ym$M,  data = burg_ym, 
                     ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(0:2))
  ) 
  
  print(tuneResult) 
  plot(tuneResult)
  
  plot(burg_ym, pch=16)
  tunedModel <- tuneResult$best.model
  tunedModelY <- predict(tunedModel, burg_ym) 
  
  points(burg_ym$M, predictedY1, col = "red", pch=4)
  lines(burg_ym$M, predictedY1, col = "red", pch=4)
  
  points(burg_ym$M, tunedModelY, col = "blue", pch=4)
  lines(burg_ym$M, tunedModelY, col = "blue", pch=4)
  
  error <- burg_ym$n - tunedModelY  
  
  # this value can  be different because the best model is determined by cross-validation over randomly shuffled data 
  tunedModelRMSE <- rmse(error)  # 2.219642 
  tunedModelRMSE
#} 


# end of svr model ======================================= 
legend("topleft", c("Actual","Predicted"), border="black",fill = c("red","blue"))
  
  