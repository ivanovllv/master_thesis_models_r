cv_plot <- function(result){
  for(z in 1:2){
    if(z < 2){
      zone = result$Zone_1
    }
    else{
      zone = result$Zone_2
    }
    for(i in 1:4){
      model_result = zone[which(zone$Model == i), ]
      
      mse = round(model_result$Results.MSE_mean[1], 2)
      # mre = round(model_result$Results.MRE_min[1], 4) * 100
      
      plot_title = paste("Prediction, model ", i, ". Price zone ", z, sep = '')
      descripiption  = paste("MSE = ", mse, ".", sep = '')
      
      plot(model_result$Results.Price, type = 'l', col = 'blue', lty = 1,
           axes = FALSE, ann = FALSE)
      par(new = TRUE)
      plot(model_result$Results.Predicted_price, type = 'l', col = 'red', lty = 2,
           main = plot_title, sub = descripiption,
           xlab = 'Hour', ylab = 'Price, RUB / MWt*hr', font.lab=2, xaxt='n')
      grid(nx = 24, ny = 10, col = "lightgray", lty = "dotted",
           lwd = par("lwd"), equilogs = TRUE)
      legend('bottomright', c('Actual data', 'Predicted data') ,
             lty=c(1,2), col=c('blue',' red'), cex=.75,
             title="Price", text.font=3,
             box.lty=1, box.lwd=1, box.col="black", bg = 'white')
      axis(1, at=1:24, labels = TRUE)
    }
  }
}

cross_validation <- function() {
  results_1 = NULL
  results_2 = NULL
  
  ############ Data indexes preparation ###############
  row_numbers = nrow(data_pr1)
  used_row_numbers = c()
  
  sample_idx_test = c()
  sample_idx_train = c()
  for(i in 1:k_fold_num){
    sample_idx_test[[i]] <- sample(row_numbers[!row_numbers %in% used_row_numbers], fold_size)
    used_row_numbers <- c(used_row_numbers, sample_idx_test[i])
    if (length(row_numbers[!row_numbers %in% sample_idx_test[i]])== 0) break
    sample_idx_train[[i]] <- sample(row_numbers[!row_numbers %in% sample_idx_test[i]], train_size)
  }
  ######### end of data indexes preparation #########
  
  for(zone in 1:2){
    data = NULL
    if(zone < 2){
      data = data_pr1
    }
    else{
      data = data_pr2
    }

    ######### MODELS ##########
    model_1_formula <- "Price ~ Price_previous"
    model_2_formula <- "Price ~ `Peak (08:00-22:00)` + `Weekend (Sat & Sun)` + Winter + Spring + Summer + Price_previous"
    model_3_formula <- "Price ~ `Peak (08:00-22:00)` + `Weekend (Sat & Sun)` + Winter + Spring + Summer + Jump + Price_previous"
    model_4_formula <- "Price ~ `Peak (08:00-22:00)` + Winter + Spring + Summer + Lambda + Price_previous"
    
    models <- list()
    models["1"] = model_1_formula
    models["2"] = model_2_formula
    models["3"] = model_3_formula
    models["4"] = model_4_formula
    
    run_cv_test_for_model <- function(model_formula){
      test_cases_price <- data.frame(Index = integer(), Value = double())
      test_cases_predicted_price <- data.frame(Index = integer(), Value = double())
      test_cases_mse <- data.frame(Index = integer(), Value = double())
      test_cases_mre <- data.frame(Index = integer(), Value = double())
      
      for(i in 1:k_fold_num){
        data_train <- data[sample_idx_train[[i]], ]
        data_test <- data[sample_idx_test[[i]], ]
        
        model = do.call("lm", list(as.formula(model_formula), data_train)) 
        
        ######## Prediction #############
        real_vs_predicted <- data.frame(`Price` = data_test$Price,`Predicted_price` = predict(model, data_test))
        
        ###### Errors (Relative, SSE & MSE) ########
        sse = 0.0
        relative_error = c()
        for(j in 1:length(real_vs_predicted$Price)){
          dif = real_vs_predicted$Price[j] - real_vs_predicted$Predicted_price[j]
          rel_err = abs(dif/real_vs_predicted$Price[j])
          sse <- sse + dif^2
          relative_error <- c(relative_error, rel_err)
        }
        mse = sse/length(real_vs_predicted$Price)
        mean_relative_error = mean(relative_error, na.rm = TRUE)
        
        test_cases_price <- rbind(test_cases_price, data.frame(Index = i, Value = real_vs_predicted$Price))
        test_cases_predicted_price <- rbind(test_cases_predicted_price, data.frame(Index = i, Value = real_vs_predicted$Predicted_price))
        test_cases_mse <- rbind(test_cases_mse, data.frame(Index = i, Value = mse))
        test_cases_mre <- rbind(test_cases_mre, data.frame(Index = i, Value = mean_relative_error))
      }
      
      compare_folds <- function(){
        min_mse = test_cases_mse[which.min(test_cases_mse$Value),]
        max_mse = test_cases_mse[which.max(test_cases_mse$Value),]
        mean_mse = mean(test_cases_mse$Value)
        
        min_mre = test_cases_mre[which.min(test_cases_mre$Value),]
        max_mre = test_cases_mre[which.max(test_cases_mre$Value),]
        # do.call(data.frame, lapply(test_cases_mre, function(Value) replace(Value, is.infinite(Value),NA))) # Cleaning out Infinite RE (zero prices)
        quantile_mre = quantile(test_cases_mre$Value, c(seq(0.8, 0.95, by=0.01)))
        print(model_formula)
        print(quantile_mre)
        
        price_best = test_cases_price[which(test_cases_price$Index == min_mse$Index),]
        predicte_price_best = test_cases_predicted_price[which(test_cases_predicted_price$Index == min_mse$Index),]
        
        result <- list(`Price` = price_best$Value,
                       `Predicted_price` = predicte_price_best$Value,
                       `MSE_max` = max_mse$Value,
                       `MSE_min` = min_mse$Value,
                       `MSE_mean` = mean_mse,
                       `MRE_max` = max_mre$Value,
                       `MRE_min` = min_mre$Value)
        return(result)  
      }
      
      return(compare_folds())
    }
    
    ##### Models loop ######
    models_results <- data.frame(Model = integer(), Results = list())
    for(m in 1:length(models)){
      key = names(models)[m]
      model_formula = models[[key]]
      result_for_model <- run_cv_test_for_model(model_formula)
      models_results <- rbind(models_results, data.frame(Model = m, Results = result_for_model))
    }
    
    
    if(zone < 2){
      results_1 = models_results
    }
    else{
      results_2 = models_results
    }
  }
  
  final_result <- list(`Zone_1` = results_1, `Zone_2` = results_2)
  return(final_result)
}

#### Run program ######
cv_test = cross_validation()
cv_plot(cv_test)