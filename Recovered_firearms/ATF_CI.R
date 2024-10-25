future::plan("cluster", workers = 10)

library(dplyr)


# Function to make scenario
makeScenario <- function(x, beta_0, beta_1, sigma){
  eps <- rnorm(length(x), 0, sigma)
  trialData <- data.frame(
    x = x,
    eps = eps,
    y = exp(beta_0) *  x^beta_1 * exp(eps)
  )
  return(trialData)
}

#------------------------
# Data for the analyses
source("Recovered_firearms/relevant_data.R")   #Run relevant_data



stateList <- unique(merged_data$state)    # Get unique state names

# States where is not possible to apply the greedy algorithm
excluded_states <- c("Alaska", "District of Columbia", "Iowa", "Kansas", "Maine","Minnesota","Nebraska","New Hampshire","North Dakota","South Dakota","Vermont","Wyoming") 

results_list <- list() 


for (state_name in stateList) {
  if (state_name %in% excluded_states) {
    results_list[[state_name]] <- c(NA, NA)
    next  
  }
  
  state_top10 <- merged_data %>%
    filter(year == 2022 & state == state_name)
  
  Pop_state <- city_popData %>%
    filter(STNAME == state_name)
  
  Pop_state_rest <- Pop_state %>%
    anti_join(select(state_top10, city), by = c("NAME" = "city"))
  
  x <- c(state_top10$ESTIMATESBASE2020, Pop_state_rest$ESTIMATESBASE2020)
  xr <- Pop_state_rest$ESTIMATESBASE2020
  
  # Parameters
  z0 <- rep(1, length(xr)) # Initial conditions
  steps <- state_top10$all_firearms_recovered[1] - sum(state_top10$firearm_by_city) - sum(z0) # Calculate steps
  
  if (steps < 1) {
    results <- data.frame(
      State = state_name,
      lmAlpha_10 = lm10$coefficients[1],
      lmBeta_10 = lm10$coefficients[2],
      lmBeta_10_ll = ci10[2, 1],
      lmBeta_10_ul = ci10[2, 2],
      alpha_max = NA,
      alpha_min = NA,
      beta_min = NA,
      beta_max = NA
    )
  } else {
    maxZ <- min(state_top10$firearm_by_city)
    
    z_starMAX <- topkOptim::optim(
      xr = Pop_state_rest$ESTIMATESBASE2020,
      x = x,
      z0 = z0,
      steps = steps,
      maxZ = maxZ,
      maximize = 1,
      verbose = -1
    )
    z_starMIN <- topkOptim::optim(
      xr = Pop_state_rest$ESTIMATESBASE2020,
      x = x,
      z0 = z0,
      steps = steps,
      maxZ = maxZ,
      maximize = -1,
      verbose = -1
    )
    
    # Model fitting for maximum case
    y_starMAX <- c(state_top10$firearm_by_city, z_starMAX)
    x_starMAX <- x
    model_max <- lm(log(y_starMAX) ~ log(x_starMAX))
    
    # Model fitting for minimum case
    y_starMIN <- c(state_top10$firearm_by_city, z_starMIN)
    x_starMIN <- x
    model_min <- lm(log(y_starMIN) ~ log(x_starMIN))
    
    # Model fitting for the top 10 cities
    lm10 <- lm(log(firearm_by_city) ~ log(ESTIMATESBASE2020), data = state_top10)
    
    # Get t-statistic confidence intervals
    ci10 <- confint(lm10)
    
    results <- data.frame(
      State = state_name,
      lmAlpha_10 = lm10$coefficients[1],
      lmBeta_10 = lm10$coefficients[2],
      lmBeta_10_ll = ci10[2, 1],
      lmBeta_10_ul = ci10[2, 2],
      alpha_max = model_max$coefficients[1],
      alpha_min = model_min$coefficients[1],
      beta_min = model_min$coefficients[2],
      beta_max = model_max$coefficients[2]
    )
  }
  
  print(results)
  
  # Variance of top10
  residuals <- residuals(lm10)
  
  sigma_square <- (1 / (length(state_top10$ESTIMATESBASE2020) - 2)) * sum(residuals^2)
  err_sd <- sqrt(sigma_square)
  
  Y_ratio = (sum(state_top10$firearm_by_city))/(state_top10$all_firearms_recovered[1])
  
  
  # Make design
  design <- expand.grid(
    beta = results$lmBeta_10,
    sigma = err_sd,
    n = nrow(state_top10)  # Generate only top-10 data
  )
  
  simData <- data.frame()
  
  valid_rows <- 0  # Initialize count of valid rows
  
  # Keep adding rounds until simData has 1000 non-NA rows
  while (valid_rows < 1000) {
    # Simulate
    rounds <- 100
    new_simData <- lapply(1:nrow(design), function(i) {
      # Get design parameters
      pars <- design[i, ]
      x <- state_top10$ESTIMATESBASE2020
      
      # For each design, do many rounds to marginalize over epsilon
      roundRes <- future.apply::future_lapply(1:rounds, function(j) {
        # Generate data
        trialData <- makeScenario(
          x = x,
          beta_0 = results$lmAlpha_10,
          beta_1 = pars$beta,
          sigma = pars$sigma
        )
        
        x <- c(trialData$x, Pop_state_rest$ESTIMATESBASE2020)
        xr <- Pop_state_rest$ESTIMATESBASE2020
        
        # Round y values
        trialData$y <- round(trialData$y)
        
        # Optimize parameters
        z0 <- rep(1, length(xr))
        
        steps_aux <- state_top10$all_firearms_recovered[1] - sum(trialData$y) - sum(z0) # Method 1   # Assume total number of recovered firearms is constant (to be used with dif selection)
        #steps_aux = ((sum(trialData$y))/Y_ratio) - sum(trialData$y) - sum(z0)          # Method 2   # Assume ratio is constant
        
        #if ((steps_aux > 0)) {                                                # Method 2
          if ((steps_aux > 0) & (abs(steps - steps_aux) / steps < 0.05)) {     # Method 1     # Check if the difference between steps and steps_aux is less than 5%
          tryCatch({
            z_starMAX <- topkOptim::optim(
              xr = xr,
              x = x,
              z0 = z0,
              steps = steps_aux,
              maxZ = min(trialData$y),
              maximize = 1
            )
            z_starMIN <- topkOptim::optim(
              xr = xr,
              x = x,
              z0 = z0,
              steps = steps_aux,
              maxZ = min(trialData$y),
              maximize = -1
            )
            
            y_starMAX <- c(trialData$y, z_starMAX)
            x_starMAX <- c(trialData$x, xr)
            model_max <- lm(log(y_starMAX) ~ log(x_starMAX))
            
            y_starMIN <- c(state_top10$firearm_by_city, z_starMIN)
            x_starMIN <- c(trialData$x, xr)
            model_min <- lm(log(y_starMIN) ~ log(x_starMIN))
            
            lm10 <- lm(log(trialData$y) ~ log(trialData$x))
            
            # Get t-statistic confidence interval
            ci10 <- confint(lm10)
            
            return(
              data.frame(
                design[i, ],
                round = j,
                beta_10 = lm10$coefficients[2],
                beta_max = model_max$coefficients[2],
                beta_min = model_min$coefficients[2],
                dif = abs(steps - steps_aux) / steps
              )
            )
          }, error = function(e) {
            return(
              data.frame(
                design[i, ],
                round = j,
                beta_10 = NA,
                beta_max = NA,
                beta_min = NA,
                dif = NA
              )
            )
          })
        } else {
          return(
            data.frame(
              design[i, ],
              round = j,
              beta_10 = NA,
              beta_max = NA,
              beta_min = NA,
              dif = abs(steps - steps_aux) / steps
            )
          )
        }
      }, 
      future.seed = TRUE)

      return(do.call(rbind, roundRes))
    })
    
    simData <- rbind(simData, do.call(rbind, new_simData))

    valid_rows <- sum(complete.cases(simData))
  }
  
  # Trim simData to 1000 rows without NAs
  simData <- simData[complete.cases(simData), ][1:1000, ]
  
  varianceBetaMax <- var(simData$beta_max)
  varianceBetaMin <- var(simData$beta_min)
  
  results_list[[state_name]] <- c(varianceBetaMax, varianceBetaMin, simData)
}



#-----------------------------------
## Save results_list
#saveRDS(results_list, file = "...")

# Read results_list
results_list <- readRDS(".../var_results_list_M1_it1000.rds")
#-----------------------------------




# ---------------------------- Create table for all states--------------------------------------------------------
table_results <- data.frame(
  state = character(),
  var_beta_min = numeric(),
  var_beta_max = numeric(),
  mean_beta_min = numeric(),
  mean_beta_max = numeric(),
  ci_margin_min = numeric(),
  ci_margin_max = numeric()
)

for (state_name in names(results_list)) {
  relevant <- results_list[[state_name]]
  
  if (is.list(relevant) && !is.null(relevant$beta_min) && !is.null(relevant$beta_max)) {
    if (all(is.na(relevant$beta_min)) && all(is.na(relevant$beta_max))) {
      var_beta_min <- NA
      var_beta_max <- NA
      mean_beta_min <- NA
      mean_beta_max <- NA
      ci_margin_min  <- NA
      ci_margin_max  <- NA
    } else {
      var_beta_min <- var(relevant$beta_min, na.rm = TRUE)
      var_beta_max <- var(relevant$beta_max, na.rm = TRUE)
      mean_beta_min <- mean(relevant$beta_min, na.rm = TRUE)
      mean_beta_max <- mean(relevant$beta_max, na.rm = TRUE)
      ci_margin_min  <- 1.96 * (sqrt(var_beta_min) / sqrt(1000))
      ci_margin_max  <- 1.96 * (sqrt(var_beta_max) / sqrt(1000))
    
    }
  } else {
    var_beta_min <- NA
    var_beta_max <- NA
    mean_beta_min <- NA
    mean_beta_max <- NA
    ci_margin_min  <- NA
    ci_margin_max  <- NA
  }
  
  new_row <- data.frame(
    state = state_name,
    var_beta_min = var_beta_min,
    var_beta_max = var_beta_max,
    mean_beta_min =  mean_beta_min,
    mean_beta_max = mean_beta_max,
    ci_margin_min = ci_margin_min,
    ci_margin_max = ci_margin_max
  )
  
  table_results <- rbind(table_results, new_row)
}



table_results$ci_min <- paste0("[", 
                               round(table_results$mean_beta_min - table_results$ci_margin_min, 3), 
                               "; ", 
                               round(table_results$mean_beta_min + table_results$ci_margin_min, 3), 
                               "]")

table_results$ci_max <- paste0("[", 
                               round(table_results$mean_beta_max - table_results$ci_margin_max, 3), 
                               "; ", 
                               round(table_results$mean_beta_max + table_results$ci_margin_max, 3), 
                               "]")


print(table_results)









