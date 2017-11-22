model_3 <- function(price_zone){
  
  data = NULL
  if(price_zone < 2){
    data = data_pr1
    data_train = data_train_1
    data_test = data_test_1
  }
  else{
    data = data_pr2
    data_train = data_train_2
    data_test = data_test_2
  }
  
  ######### MODEL ##########
  regression = lm(Price ~ `Peak (08:00-22:00)` + `Weekend (Sat & Sun)` + Winter + Spring + Summer + Jump + Price_previous, data_train)
  
  ###### Prediction ########
  real_vs_predicted <- cbind(data_test,`Predicted_price` = predict(regression, data_test))
  
  ###### Errors (Relative, SSE & MSE) ########
  sse = 0.0
  relative_error = c()
  for(i in 1:length(real_vs_predicted$Price)){
    dif = real_vs_predicted$Price[i] - real_vs_predicted$Predicted_price[i]
    rel_err = abs(dif/real_vs_predicted$Price[i])
    sse <- sse + dif^2
    relative_error <- c(relative_error, rel_err)
  }
  mse = sse/length(real_vs_predicted$Price)
  mean_relative_error = mean(relative_error, na.rm = TRUE)
  
  real_vs_predicted <- cbind(real_vs_predicted, `SSE` = sse, `MSE` = mse, `Mean_relative_error` = mean_relative_error)
  return(real_vs_predicted)
}


###### Main Program ######
model_3_1 = model_3(1)
model_3_2 = model_3(2)

##################
descripiption  = paste("MSE = ", model_3_1$MSE[1], ". Mean relative error = ", model_3_1$Mean_relative_erro[1], sep = '')
plot(model_3_1$Price, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(model_3_1$Predicted_price, type = 'l', col = 'red', lty = 2,
     main = 'Prediction, model 3. Price zone 1', sub = descripiption,
     xlab = 'Hour', ylab = 'Price, RUB', font.lab=2 )
grid(nx = length(model_3_1$Price), ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topright', c('Actual data', 'Predicted data') ,
       lty=c(1,2), col=c('blue',' red'), cex=.75,
       title="Price", text.font=3,
       box.lty=1, box.lwd=1, box.col="black", bg = 'white')
###################
descripiption  = paste("MSE = ", model_3_2$MSE[1], ". Mean relative error = ", model_3_2$Mean_relative_erro[1], sep = '')
plot(model_3_2$Price, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(model_3_2$Predicted_price, type = 'l', col = 'red', lty = 2,
     main = 'Prediction, model 3. Price zone 2', sub = descripiption,
     xlab = 'Hour', ylab = 'Price, RUB', font.lab=2 )
grid(nx = length(model_3_1$Price), ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topright', c('Actual data', 'Predicted data') ,
       lty=c(1,2), col=c('blue',' red'), cex=.75,
       title="Price", text.font=3,
       box.lty=1, box.lwd=1, box.col="black", bg = 'white')