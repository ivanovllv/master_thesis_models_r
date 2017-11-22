winter_2015_1 = data_pr1[1:1416, ]
spring_2015_1 = data_pr1[1417:3624, ]
summer_2015_1 = data_pr1[3625:5832, ]
autumn_2015_1 = data_pr1[5833:8016, ]
winter_2016_1 = data_pr1[8017:10200, ]
spring_2016_1 = data_pr1[10201:12408, ]
summer_2016_1 = data_pr1[12409:14616, ]
autumn_2016_1 = data_pr1[14617:16800, ]
winter_2015_2 = data_pr2[1:1416, ]
spring_2015_2 = data_pr2[1417:3624, ]
summer_2015_2 = data_pr2[3625:5832, ]
autumn_2015_2 = data_pr2[5833:8016, ]
winter_2016_2 = data_pr2[8017:10200, ]
spring_2016_2 = data_pr2[10201:12408, ]
summer_2016_2 = data_pr2[12409:14616, ]
autumn_2016_2 = data_pr2[14617:16800, ]


############Seasonly Means##################
get_seasonly_mean <- function(prices){
  output = c()
  for(j in 1:24){
    sample = c()
    for(k in 1:2){
      if(k < 2){
        for(i in seq(j,length(prices), 24)){
          sample <- c(sample, prices[i])
        }
      }
      else{
        output[j] <- mean(sample)
      }   
    }
  }
  return (output)
}
# Plot settings
opar <- par(pch = 19) #use filled dot
seasons = c('Winter', 'Spring', 'Summer', 'Autumn')
hours = c('0:00', '1:00', '2:00','3:00','4:00','5:00','6:00',
          '7:00', '8:00', '9:00','10:00','11:00','12:00','13:00',
          '14:00', '15:00', '16:00','17:00','18:00','19:00','20:00',
          '21:00', '22:00', '23:00')
#
winter_2015_1_means = get_seasonly_mean(winter_2015_1$Price)
spring_2015_1_means = get_seasonly_mean(spring_2015_1$Price)
summer_2015_1_means = get_seasonly_mean(summer_2015_1$Price)
autumn_2015_1_means = get_seasonly_mean(autumn_2015_1$Price)
plot(winter_2015_1_means, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(spring_2015_1_means, type = 'l', col = 'green', lty = 2,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(summer_2015_1_means, type = 'l', col = 'red', lty = 4,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(autumn_2015_1_means, type = 'l', col = 'orange', lty = 5,
     main = 'Average seasonal price level, 2015. 1st price zone',
     xlab = 'Hour', ylab = 'Price, RUB / MWt*hr', xaxt='n')
grid(nx = 24, ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topleft', seasons ,
       lty=c(1,2,4,5), col=c('blue', 'green',' red', 'orange'), cex=.75,
       title="Seasons", text.font=3,
       box.lty=1, box.lwd=1, box.col="black")

axis(1, at=1:24, labels=hours)
############################
winter_2015_2_means = get_seasonly_mean(winter_2015_2$Price)
spring_2015_2_means = get_seasonly_mean(spring_2015_2$Price)
summer_2015_2_means = get_seasonly_mean(summer_2015_2$Price)
autumn_2015_2_means = get_seasonly_mean(autumn_2015_2$Price)
plot(winter_2015_2_means, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(spring_2015_2_means, type = 'l', col = 'green', lty = 2,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(summer_2015_2_means, type = 'l', col = 'red', lty = 4,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(autumn_2015_2_means, type = 'l', col = 'orange', lty = 5,
     main = 'Average seasonal price level, 2015. 2st price zone',
     xlab = 'Hour', ylab = 'Price, RUB / MWt*hr', xaxt='n')
grid(nx = 24, ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topleft', seasons ,
       lty=c(1,2,4,5), col=c('blue', 'green',' red', 'orange'), cex=.75,
       title="Seasons", text.font=3,
       box.lty=1, box.lwd=1, box.col="black")

axis(1, at=1:24, labels=hours)
##############################
winter_2016_1_means = get_seasonly_mean(winter_2016_1$Price)
spring_2016_1_means = get_seasonly_mean(spring_2016_1$Price)
summer_2016_1_means = get_seasonly_mean(summer_2016_1$Price)
autumn_2016_1_means = get_seasonly_mean(autumn_2016_1$Price)
plot(winter_2016_1_means, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(spring_2016_1_means, type = 'l', col = 'green', lty = 2,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(summer_2016_1_means, type = 'l', col = 'red', lty = 4,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(autumn_2016_1_means, type = 'l', col = 'orange', lty = 5,
     main = 'Average seasonal price level, 2016. 1st price zone',
     xlab = 'Hour', ylab = 'Price, RUB / MWt*hr', xaxt='n')
grid(nx = 24, ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topleft', seasons ,
       lty=c(1,2,4,5), col=c('blue', 'green',' red', 'orange'), cex=.75,
       title="Seasons", text.font=3,
       box.lty=1, box.lwd=1, box.col="black")

axis(1, at=1:24, labels=hours)
############################
winter_2016_2_means = get_seasonly_mean(winter_2016_2$Price)
spring_2016_2_means = get_seasonly_mean(spring_2016_2$Price)
summer_2016_2_means = get_seasonly_mean(summer_2016_2$Price)
autumn_2016_2_means = get_seasonly_mean(autumn_2016_2$Price)
plot(winter_2016_2_means, type = 'l', col = 'blue', lty = 1,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(spring_2016_2_means, type = 'l', col = 'green', lty = 2,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(summer_2016_2_means, type = 'l', col = 'red', lty = 4,
     axes = FALSE, ann = FALSE)
par(new = TRUE)
plot(autumn_2016_2_means, type = 'l', col = 'orange', lty = 5,
     main = 'Average seasonal price level, 2016. 2st price zone',
     xlab = 'Hour', ylab = 'Price, RUB / MWt*hr', xaxt='n')
grid(nx = 24, ny = 10, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend('topleft', seasons ,
       lty=c(1,2,4,5), col=c('blue', 'green',' red', 'orange'), cex=.75,
       title="Seasons", text.font=3,
       box.lty=1, box.lwd=1, box.col="black")

axis(1, at=1:24, labels=hours)
##############################