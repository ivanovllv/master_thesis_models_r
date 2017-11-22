combine_data_for_anova <- function(){
  
  data <- data.frame(Season = integer(), Price = double())
  
  for(i in 1:length(data_pr1$Price)){
    season = 1 #defualt for winter
    if(data_pr1$Spring[i] == 1){
      season = 2
    }
    else if(data_pr1$Summer[i] == 1){
      season = 3
    }
    data <- rbind(data, data.frame(Season = season, Price = data_pr1$Price[i]))
  }
  
  return(data)
}

#av_data = combine_data_for_anova()
av_test = oneway.test(Price ~ Season, data = av_data, var.equal = TRUE)

aov_test = aov(Price ~ Season, data = av_data)

boxplot(Price ~ Season, av_data, main = 'Seasonal price',
        xlab = 'Season', ylab = 'Price, RUB / MWt*hr', font.lab=2, xaxt='n')
axis(1, at=1:3, labels = c("Winter", "Spring", "Summer"))