library(dplyr)
library(future)
library(future.apply)


# Data for the analyses
source("Recovered_firearms/relevant_data.R") #Run relevant_data


future::plan("cluster", workers = 10)

process_state <- function(state_name) {

  state_top10 <- merged_data %>%
    filter(year == 2022 & state == state_name)

  Pop_state <- city_popData %>%
    filter(STNAME == state_name)

  Pop_state_rest <- Pop_state %>%
    anti_join(select(state_top10, city), by = c("NAME" = "city"))

  x <- c(state_top10$ESTIMATESBASE2020, Pop_state_rest$ESTIMATESBASE2020)
  xr <- Pop_state_rest$ESTIMATESBASE2020

  # Parameters
  z0 <- rep(1, length(xr))  # Initial conditions
  steps <- state_top10$all_firearms_recovered[1] - sum(state_top10$firearm_by_city) - sum(z0)  # Calculate steps

  if (steps < 1) {
    results <- data.frame(
      State = state_name,
      beta_min = NA,
      linear_beta_min = NA,
      beta_max = NA,
      linear_beta_max = NA,
      unknowns = NA
    )
  } else {
    maxZ <- min(state_top10$firearm_by_city)

    # Perform optimization
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

    maxZv <- rep(maxZ, length(z0))

    #------------------------ Give the max to the highest populations -----------------------------------------------
    xr_decreasing <- sort(xr, decreasing = TRUE)
    linear_z_starMAX <- z0
    for (i in seq_along(maxZv)) {
      while (steps > 0 && linear_z_starMAX[i] != maxZv[i]) {
        if (linear_z_starMAX[i] < maxZv[i]) {
          linear_z_starMAX[i] <- linear_z_starMAX[i] + 1  # Increment the value in linear_z_starMAX
          steps <- steps - 1  # Decrease the number of steps
        }
      }
    }

    #------------------------ Give the max to the lowest populations -----------------------------------------------
    xr_increasing <- sort(xr, decreasing = FALSE)
    linear_z_starMIN <- z0
    for (i in seq_along(maxZv)) {
      while (steps > 0 && linear_z_starMIN[i] != maxZv[i]) {
        if (linear_z_starMIN[i] < maxZv[i]) {
          linear_z_starMIN[i] <- linear_z_starMIN[i] + 1  # Increment the value in linear_z_starMIN
          steps <- steps - 1  # Decrease the number of steps
        }
      }
    }

    # Model fitting for maximum case
    y_starMAX <- c(state_top10$firearm_by_city, z_starMAX)
    x_starMAX <- x
    model_max <- lm(log(y_starMAX) ~ log(x_starMAX))

    # Model fitting for minimum case
    y_starMIN <- c(state_top10$firearm_by_city, z_starMIN)
    x_starMIN <- x
    model_min <- lm(log(y_starMIN) ~ log(x_starMIN))

    # Model fitting for linear maximum case
    linear_y_starMAX <- c(state_top10$firearm_by_city, linear_z_starMAX)
    linear_x_starMAX <- c(state_top10$ESTIMATESBASE2020, xr_decreasing)
    linear_model_max <- lm(log(linear_y_starMAX) ~ log(linear_x_starMAX))

    # Model fitting for linear minimum case
    linear_y_starMIN <- c(state_top10$firearm_by_city, linear_z_starMIN)
    linear_x_starMIN <- c(state_top10$ESTIMATESBASE2020, xr_increasing)
    linear_model_min <- lm(log(linear_y_starMIN) ~ log(linear_x_starMIN))

    results <- data.frame(
      State = state_name,
      beta_min = model_min$coefficients[2],
      linear_beta_min = linear_model_min$coefficients[2],
      beta_max = model_max$coefficients[2],
      linear_beta_max = linear_model_max$coefficients[2],
      unknowns = length(xr)
    )
  }

  return(results)
}

stateList <- unique(merged_data$state)

# Apply the process_state function to all states in stateList in parallel
results_list <- future_lapply(stateList, process_state)
final_results <- do.call(rbind, results_list)
print(final_results)


final_results$dif <- final_results$beta_max - final_results$linear_beta_max






