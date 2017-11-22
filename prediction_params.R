###### Prediction parameters ########
forecasting_horizon = 24
split_percentage = 1 - forecasting_horizon/length(data_pr1$Price)
sample_idx <- sample(nrow(data_pr1), nrow(data_pr1)*split_percentage)

data_train_1 <- data_pr1[sample_idx, ]
data_test_1 <- data_pr1[-sample_idx, ]

data_train_2 <- data_pr2[sample_idx, ]
data_test_2 <- data_pr2[-sample_idx, ]


##### CV params #####
k_fold_num = floor(length(data_pr1$Price)/forecasting_horizon)
fold_size = forecasting_horizon
train_size = length(data_pr1$Price) - forecasting_horizon