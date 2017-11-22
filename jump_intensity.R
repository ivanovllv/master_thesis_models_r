get_jump_intensity <- function(prices){
    return(sum(prices^2))
}

prices_1 = data_pr1$Price
prices_2 = data_pr2$Price
jump_intensity_1 = get_jump_intensity(prices_1)
jump_intensity_2 = get_jump_intensity(prices_2)