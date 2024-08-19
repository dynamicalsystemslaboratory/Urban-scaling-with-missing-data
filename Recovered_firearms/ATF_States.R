future::plan("cluster", workers = 10)
options(future.globals.maxSize = 850 * 1024^2)

library(dplyr)


# Data for the analyses
source("Recovered_firearms/relevant_data.R")


merged_data <- merged_data[!is.na(merged_data$ESTIMATESBASE2020),]

stateList <- unique(merged_data$state)  # Get unique state names


results <- future.apply::future_lapply(stateList, function(state_name){
  
  # Filter data for the current state and date
  state_top10 <- merged_data %>%
    filter(year == 2022 & state == state_name)
  Pop_state <- city_popData %>%
    filter(STNAME == state_name)
  Pop_state_rest <- Pop_state %>%
    anti_join(select(state_top10, city), by = c("NAME" = "city"))
  
  
  x <- c(state_top10$ESTIMATESBASE2020, Pop_state_rest$ESTIMATESBASE2020)
  xr <- Pop_state_rest$ESTIMATESBASE2020
  
  # Initial conditions
  z0 <- rep(1, length(xr)) 
  steps <- state_top10$all_firearms_recovered[1] - sum(state_top10$firearm_by_city) - sum(z0) #y_left updated
  
  if(steps < 1){
    
    lm10 <- lm(log(firearm_by_city) ~ log(ESTIMATESBASE2020), data = state_top10)
    ci10 = confint(lm10)
    Y_ratio = (sum(state_top10$firearm_by_city))/(state_top10$all_firearms_recovered[1])
    
    return(
      data.frame(
        State = state_name,
        Y_ratio = Y_ratio,
        lmBeta_10 = lm10$coefficients[2],
        lmBeta_10_ll = ci10[2, 1],
        lmBeta_10_ul = ci10[2, 2],
        beta_min = NA,
        beta_max = NA
      )
    )
  }
  
  Y_ratio = (sum(state_top10$firearm_by_city))/(state_top10$all_firearms_recovered[1])
  maxZ <- min(state_top10$firearm_by_city)
  
  z_starMAX <- topkOptim::optim(
    xr = Pop_state_rest$ESTIMATESBASE2020,
    x = x,
    z0 = z0,
    steps = steps,
    maxZ = maxZ,
    maximize = 1,
    verbose = 1
  )
  
  z_starMIN <- topkOptim::optim(
    xr = Pop_state_rest$ESTIMATESBASE2020,
    x = x,
    z0 = z0,
    steps = steps,
    maxZ = maxZ,
    maximize = -1,
    verbose = 1
  )
  
  # Output
  y_starMAX <- c(state_top10$firearm_by_city, z_starMAX)
  x_starMAX <- x
  model_max <- lm(log(y_starMAX) ~ log(x_starMAX))
  
  y_starMIN <- c(state_top10$firearm_by_city, z_starMIN)
  x_starMIN <- x
  model_min <- lm(log(y_starMIN) ~ log(x_starMIN))
  
  lm10 <- lm(log(firearm_by_city) ~ log(ESTIMATESBASE2020), data = state_top10)
  ci10 <- confint(lm10)
  
  return(
    data.frame(
      State = state_name,
      Y_ratio = Y_ratio,
      lmBeta_10 = lm10$coefficients[2],
      lmBeta_10_ll = ci10[2, 1],
      lmBeta_10_ul = ci10[2, 2],
      beta_min = model_min$coefficients[2],
      beta_max = model_max$coefficients[2]
    )
  )
}, future.seed = TRUE)


resultsATF <- do.call(rbind, results)
row.names(resultsATF) <- NULL










