############### Data sets completion ############
#Price at 31.12.2014 23:00
pre_value_1 = 950.54
pre_value_2 = 992.87

prices_previous_1 <- c(pre_value_1, data_pr1$Price) #Shift price array back for 1 step
prices_previous_1 <- head(prices_previous_1, -1)

prices_previous_2 <- c(pre_value_2, data_pr2$Price) #Shift price array back for 1 step
prices_previous_2 <- head(prices_previous_2, -1)

data_pr1["Price_previous"] <- NA
data_pr2["Price_previous"] <- NA
data_pr1$Price_previous <- prices_previous_1
data_pr2$Price_previous <- prices_previous_2

####LAMBDAS####
add_lambda_column <- function(data){
  get_lambda_for_dummy <- function(dummy){
    sample = c()
    for(i in 1:length(data$Jump)){
      if(dummy[i] > 0){
        sample <- c(sample, data$Jump[i])
      }
    }
    return(mean(sample))
  }
  
  lambda_0 = mean(data$Jump);
  lambda_peak = get_lambda_for_dummy(data$`Peak (08:00-22:00)`)
  lambda_weekend = get_lambda_for_dummy(data$`Weekend (Sat & Sun)`)
  lambda_winter = get_lambda_for_dummy(data$Winter)
  lambda_spring = get_lambda_for_dummy(data$Spring)
  lambda_summer = get_lambda_for_dummy(data$Summer)
  
  lambda = c()
  for(i in 1:length(data$Price)){
    lambda[i] <- lambda_0 + 
      lambda_peak*data$`Peak (08:00-22:00)`[i]+
      lambda_weekend*data$`Weekend (Sat & Sun)`[i] +
      lambda_winter*data$Winter[i] +
      lambda_spring*data$Spring[i] +
      lambda_summer*data$Summer[i]
  }
  
  data["Lambda"] <- NA
  data$Lambda <- lambda
  
  return(data)
}

data_pr1 = add_lambda_column(data_pr1)
data_pr2 = add_lambda_column(data_pr2)
