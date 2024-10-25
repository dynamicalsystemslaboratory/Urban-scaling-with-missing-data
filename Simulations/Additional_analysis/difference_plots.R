library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)


stateList <- unique(merged_data$state)
state_name <- stateList[28]
state_name <- "Rhode Island"

state_top10 <- merged_data %>%
  filter(year == 2022 & state == state_name)

Pop_state <- city_popData %>%
  filter(STNAME == state_name)

Pop_state_rest <- Pop_state %>%
  anti_join(select(state_top10, city), by = c("NAME" = "city"))

x <- c(state_top10$ESTIMATESBASE2020, Pop_state_rest$ESTIMATESBASE2020)
xr <- Pop_state_rest$ESTIMATESBASE2020

# Parameters
z0 <- rep(1, length(xr)) 
steps <- state_top10$all_firearms_recovered[1] - sum(state_top10$firearm_by_city) - sum(z0) 

if (steps < 1) {
  results <- data.frame(
    State = state_name,
    beta_min = NA,
    linear_beta_min = NA,
    beta_max = NA,
    linear_beta_max = NA
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
  
  
  
  RHODE_ISLAND_results <- data.frame(
    State = state_name,
    beta_min = model_min$coefficients[2],
    linear_beta_min = linear_model_min$coefficients[2],
    beta_max = model_max$coefficients[2],
    linear_beta_max = linear_model_max$coefficients[2]
  )
}


nonlinear_data <- data.frame(
  x = log(x_starMAX),
  y = log(y_starMAX),
  type = "Non-Linear"
)

top10_indices <- order(y_starMAX, decreasing = TRUE)[1:10]

nonlinear_data$is_top10 <- FALSE
nonlinear_data$is_top10[top10_indices] <- TRUE

linear_data <- data.frame(
  x = log(linear_x_starMAX),
  y = log(linear_y_starMAX),
  type = "Linear",
  is_top10 = FALSE  # No top 10 flag for linear data
)
RHODE_ISLAND_data <- rbind(nonlinear_data, linear_data)




#----------------------------------------------------------------------------------------------------------
#--------------------------------------------- Texas ------------------------------------------------------
stateList <- unique(merged_data$state)
state_name <- stateList[43]


state_top10 <- merged_data %>%
  filter(year == 2022 & state == state_name)

Pop_state <- city_popData %>%
  filter(STNAME == state_name)

Pop_state_rest <- Pop_state %>%
  anti_join(select(state_top10, city), by = c("NAME" = "city"))

x <- c(state_top10$ESTIMATESBASE2020, Pop_state_rest$ESTIMATESBASE2020)
xr <- Pop_state_rest$ESTIMATESBASE2020

# Parameters
z0 <- rep(1, length(xr)) 
steps <- state_top10$all_firearms_recovered[1] - sum(state_top10$firearm_by_city) - sum(z0) 

if (steps < 1) {
  results <- data.frame(
    State = state_name,
    beta_min = NA,
    linear_beta_min = NA,
    beta_max = NA,
    linear_beta_max = NA
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
  
  
  
  TEXAS_results <- data.frame(
    State = state_name,
    beta_min = model_min$coefficients[2],
    linear_beta_min = linear_model_min$coefficients[2],
    beta_max = model_max$coefficients[2],
    linear_beta_max = linear_model_max$coefficients[2]
  )
}


# Create a data frame for the non-linear case (y_starMAX and x_starMAX)
nonlinear_data <- data.frame(
  x = log(x_starMAX),
  y = log(y_starMAX),
  type = "Non-Linear"
)

# Identify top 10 points in terms of y_starMAX (non-linear case)
top10_indices <- order(y_starMAX, decreasing = TRUE)[1:10]

# Add a column to flag the top 10 points in the non-linear data
nonlinear_data$is_top10 <- FALSE
nonlinear_data$is_top10[top10_indices] <- TRUE

# Create a data frame for the linear case (linear_y_starMAX and linear_x_starMAX)
linear_data <- data.frame(
  x = log(linear_x_starMAX),
  y = log(linear_y_starMAX),
  type = "Linear",
  is_top10 = FALSE  # No top 10 flag for linear data
)

# Combine both datasets
TEXAS_data <- rbind(nonlinear_data, linear_data)





#----------------------------------------------------------------------------------------------------------
RHODE_ISLAND_data$x <- exp(RHODE_ISLAND_data$x)
RHODE_ISLAND_data$y <- exp(RHODE_ISLAND_data$y)

TEXAS_data$x <- exp(TEXAS_data$x)
TEXAS_data$y <- exp(TEXAS_data$y)
#--------------------------------------------- Plots ------------------------------------------------------
RHODE_ISLAND_plot <- ggplot(RHODE_ISLAND_data, aes(x = x, y = y, color = type, shape = type)) +
  geom_point(aes(alpha = ifelse(is_top10 == TRUE, 0.5, 1)), size = 1, show.legend = c(alpha = FALSE)) +  
  geom_point(data = subset(RHODE_ISLAND_data, is_top10 == TRUE), 
             aes(x = x, y = y), 
             shape = 16,  # Closed circle for top 10 points
             color = "black", size = 1.3, show.legend = FALSE) +  
  labs(x = "Population", y = "Optimized Y") +
  scale_color_manual(values = c("Non-Linear" = "red", "Linear" = "#3366CC"),
                     labels = c("Non-Linear" = "Greedy Algorithm", "Linear" = "Linearized System")) +  
  scale_shape_manual(values = c("Non-Linear" = 16, "Linear" = 1),
                     labels = c("Non-Linear" = "Greedy Algorithm", "Linear" = "Linearized System")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10, angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.position = c(0.65, 0.81),  # Position inside the plot area
    legend.justification = c("right", "bottom"),
    legend.title = element_blank(), 
    legend.text = element_text(size = 11), 
    legend.key.height = unit(0.5, "cm"),  # Reduce vertical space between legend rows
    axis.ticks = element_line(size = 0.4, linewidth = 0.8, color = "black"),
    # axis.ticks = element_line(size = 0.5, color = "black"),         # ATF PLOT
    axis.line = element_line(color = "black")
  ) +
  scale_x_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^1, 10^7),
    breaks = 10^seq(1, 7, by = 1),  # Set the breaks from 10^-1.5 to 10^1.5
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^-1, 10^5),
    breaks = 10^seq(-1, 5, by = 1),  # Set the breaks from 10^-1.5 to 10^1.5
    expand = c(0, 0)
  )+
  coord_cartesian(clip = 'off')



Texas_plot <- ggplot(TEXAS_data, aes(x = x, y = y, color = type, shape = type)) +
  geom_point(aes(alpha = ifelse(is_top10 == TRUE, 0.5, 1)), size = 1, show.legend = c(alpha = FALSE)) +  
  geom_point(data = subset(TEXAS_data, is_top10 == TRUE), 
             aes(x = x, y = y), 
             shape = 16,  # Closed circle for top 10 points
             color = "black", size = 1.3, show.legend = FALSE) +  
  labs(x = "Population", y = "Optimized Y") +
  scale_color_manual(values = c("Non-Linear" = "red", "Linear" = "#3366CC"),
                     labels = c("Non-Linear" = "Greedy Algorithm", "Linear" = "Linearized System")) +  
  scale_shape_manual(values = c("Non-Linear" = 16, "Linear" = 1),
                     labels = c("Non-Linear" = "Greedy Algorithm", "Linear" = "Linearized System")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10, angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.position = c(0.65, 0.81),  # Position inside the plot area
    legend.justification = c("right", "bottom"),
    legend.title = element_blank(), 
    legend.text = element_text(size = 11), 
    legend.key.height = unit(0.5, "cm"),  # Reduce vertical space between legend rows
    axis.ticks = element_line(size = 0.4, linewidth = 0.8, color = "black"),
    # axis.ticks = element_line(size = 0.5, color = "black"),         # ATF PLOT
    axis.line = element_line(color = "black")
  ) +
  scale_x_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^1, 10^7),
    breaks = 10^seq(1, 7, by = 1),  # Set the breaks from 10^-1.5 to 10^1.5
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^-1, 10^5),
    breaks = 10^seq(-1, 5, by = 1),  # Set the breaks from 10^-1.5 to 10^1.5
    expand = c(0, 0)
  )+
  coord_cartesian(clip = 'off')




pdf(file = "dif_linear_nonlinear.pdf",width = 7, height = 3.94)
comb_plot <- RHODE_ISLAND_plot + Texas_plot +
  plot_annotation(
    tag_levels = list(c('A', 'B')),
    tag_prefix = "",
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold"),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
      #plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
    )
  )
print(comb_plot)
dev.off()
















