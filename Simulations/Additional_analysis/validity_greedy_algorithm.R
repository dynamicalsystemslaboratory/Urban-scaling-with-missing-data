library(dplyr)



#----------------------- Utilities ---------------------------------------------------------------------

# Define the test_permutations function
test_permutations <- function(z_starMAX, x, maxZ, iterations, moves) {
  
  beta_max_original <- results$beta_max
  best_beta <- 0
  best_z_starMAX <- z_starMAX
  
  beta_values <- numeric(iterations)
  euclid_distances <- numeric(iterations)
  
  for (i in 1:iterations) {
    
    # # Create a copy of z_starMAX for this iteration
    # z_starMAX_perm <- z_starMAX
    
    
    # Instead of doing iterations, do successive permutations
    if (i == 1) {
      z_starMAX_perm <- z_starMAX
    } else {
      z_starMAX_perm <- z_starMAX_perm    #
    }                                     #it will save beta_max every 10 moves and then start where it left off  (results_list_seq)
    

    
    valid_moves <- 0  # Counter for valid moves
    
    while (valid_moves < moves) {
      # Choose two random indices from z_starMAX for the permutation
      idx_subtract <- sample(seq_along(z_starMAX_perm), 1)  # Random index to subtract from
      idx_add <- sample(setdiff(seq_along(z_starMAX_perm), idx_subtract), 1)  # Random index to add to
      
      # Ensure the permutation respects maxZ and try again if invalid
      if (z_starMAX_perm[idx_add] + 1 <= maxZ && z_starMAX_perm[idx_subtract] - 1 > 0) {
        # Perform the permutation (subtract one unit and add it to another)
        z_starMAX_perm[idx_subtract] <- z_starMAX_perm[idx_subtract] - 1
        z_starMAX_perm[idx_add] <- z_starMAX_perm[idx_add] + 1
        valid_moves <- valid_moves + 1  # Only count valid moves
      }
    }
    
    # After valid moves, recompute the model with the updated z_starMAX
    y_starMAX_perm <- c(results$y_top10, z_starMAX_perm)
    model_perm <- lm(log(y_starMAX_perm) ~ log(results$x))
    beta_perm <- model_perm$coefficients[2]
    
    # Compute the Euclidean distance between the original and permuted vectors
    euclid_distances[i] <- sqrt(sum((z_starMAX - y_starMAX_perm)^2))
    
    # Store the beta for this iteration
    beta_values[i] <- beta_perm
    
    # Update if this beta is larger than the current best
    if (beta_perm > best_beta) {
      best_beta <- beta_perm
      best_z_starMAX <- z_starMAX_perm
    }
  }
  
  # Return the best beta found, the corresponding z_starMAX, and all beta values
  return(list(
    euclid_distances = euclid_distances,
    best_beta = best_beta,               # biggest beta max
    best_z_starMAX = best_z_starMAX,     # biggest beta max after the permutations
    all_betas = beta_values
  ))
}

#--------------------------------------------------------------------------------------------


stateList <- unique(merged_data$state)   # Get unique state names

# Exclude states from the experiment
excluded_states <- c("Alaska", "District of Columbia", "Iowa", "Kansas", 
                     "Maine", "Minnesota", "Nebraska", "New Hampshire", 
                     "North Dakota", "South Dakota", "Vermont", "Wyoming")

stateList <- setdiff(stateList, excluded_states)

results_list <- list()


for (state_name in stateList) {
  
  # Filter data for the selected state and year
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
    results_list[[state_name]] <- data.frame(
      State = state_name,
      lmAlpha_10 = NA,
      lmBeta_10 = NA,
      lmBeta_10_ll = NA,
      lmBeta_10_ul = NA,
      alpha_max = NA,
      alpha_min = NA,
      beta_min = NA,
      beta_max = NA,
      steps = steps
    )
  } else {
    maxZ <- min(state_top10$firearm_by_city)
    
    z_starMAX <-  topkOptim::optim(
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
    
    results <- list(
      z_starMAX = z_starMAX,
      z_starMIN = z_starMIN,
      y_top10 = state_top10$firearm_by_city,
      x = x,
      xr = xr,
      beta_min = model_min$coefficients[2],
      beta_max = model_max$coefficients[2]
    )
    
    # Run the permutation test
    results_perm <- test_permutations(results$z_starMAX, results$x, maxZ, iterations = 10000, moves = 10)
    
    # Store results including permutation results
    results_list[[state_name]] <- list(
      results = results,
      results_perm = results_perm
    )
  }
}



#-----------------------------------
## Save results_list
#saveRDS(results_list, file =  ".../additional_analyses/results_list_seq_1015max_it10000_m10_.rds")

# Read results_list
results_list <- readRDS(".../additional_analyses/results_list_min_it10000_m10_.rds")
#-----------------------------------


# ---------------------------- Create table for all states--------------------------------------------------------

table_results <- data.frame(
  state = character(),
  nr_iterations = numeric(),
  success_ratio = numeric(),
  max_euclid = numeric(),
  greedy_beta_min = numeric(),
  perm_beta_min = numeric(),
  N_k = numeric()
)

for (state_name in names(results_list)) {
  relevant <- results_list[[state_name]]

  beta_min <- relevant$results$beta_min
  results_perm <- relevant$results_perm
  
  
  nr_iterations <- length(results_perm$all_betas)
  
  # Compute the percentage of successes (where beta_max > results_perm$all_betas)
  success_count <- sum(beta_min < results_perm$all_betas)
  perc_success <- success_count / nr_iterations
  
  # Compute the maximum Euclidean distance
  max_euclid <- max(results_perm$euclid_distances)
  
  N_k <- length(relevant$results$z_starMAX)
  
  new_row <- data.frame(
    state = state_name,
    nr_iterations = nr_iterations,
    success_ratio = perc_success,
    max_euclid = max_euclid,
    greedy_beta_min = beta_min,
    perm_beta_min = min(results_perm$all_betas),
    N_k = N_k
  )
  table_results <- rbind(table_results, new_row)
}

print(table_results)

































#---------------------- Plots --------------------------------------------------------------------------------
relevant <- results_list[[state_name]]
beta_max <- relevant$results$beta_max
results_perm <- relevant$results_perm




plot(results_perm$all_betas, 
     type = "p",                   # p for points (scatter plot)
     col = "lightblue",            # Color for all points
     main = "Scatter Plot of Beta Values", 
     xlab = "Iteration", 
     ylab = "Beta Value",
     pch = 16)                     # pch = 16 for solid circles

# Highlight the best beta in dark red
points(which.max(results_perm$best_beta), 
       results_perm$best_beta,     # Use best beta for the y coordinate
       col = "darkred", 
       pch = 16,                   # Solid circle
       cex = 1.5)   

best_index <- which.max(results_perm$all_betas) # Index of the maximum beta
best_beta_value <- results_perm$all_betas[best_index]  # Value of the maximum beta

points(best_index,               # X position (iteration index)
       best_beta_value,          # Y position (maximum beta value)
       col = "red", 
       pch = 16,                  # Solid circle
       cex = 1.5)                # Increase the size of the point





























