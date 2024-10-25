library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)


# Data for the analyses
source("Recovered_firearms/relevant_data.R")


stateList <- unique(merged_data$state)
dataList <- lapply(stateList, function(state_name){
  state_top10 <- merged_data %>%
    filter(year == 2022 & state == state_name)
  
  Pop_state <- city_popData %>%
    filter(STNAME == state_name)
  
  return(
    list(
      Pop_state_rest = Pop_state[!Pop_state$NAME %in% state_top10$city,],
      state_top10 = state_top10
    )
  )
})
Pop_state_rest <- lapply(dataList, function(x){x$Pop_state_rest})
Pop_state_rest <- do.call(rbind, Pop_state_rest)
state_top10 <- lapply(dataList, function(x){x$state_top10})
state_top10 <- do.call(rbind, state_top10)

stateList <- unique(merged_data$state)  # Get unique state names
countList <- state_top10$all_firearms[!is.na(state_top10$all_firearms)]

results <- list()
for (i in 1:length(stateList)) {
  results[[i]] = list(
    state = stateList[i],
    all_firearms = countList[i],
    xr = Pop_state_rest$ESTIMATESBASE2020[Pop_state_rest$STNAME == stateList[i]],
    xk = state_top10$ESTIMATESBASE2020[state_top10$state == stateList[i]],
    yk = state_top10$firearm_by_city[state_top10$state == stateList[i]],
    z = rep(1, length(Pop_state_rest$ESTIMATESBASE2020[Pop_state_rest$STNAME == stateList[i]]))
  )
}

index <- lapply(results, function(x){
  data.frame(
    state = x$state,
    nr_cities = length(x$xr),
    steps = x$all_firearms - sum(x$yk) - sum(x$z),
    maxZ = min(x$yk)
  )
})
index <- do.call(rbind, index)
index$count <- 0

# Check if steps<0 or all_firearms can't be distributed and remove those states
index <- index[index$steps > 0,]   # more cities than firearms left
index <- index[index$maxZ * index$nr_cities >= index$steps,] # more cities than firearms left

stateList <- index$state


#--------------------------------------------------------------------------------------------
# Create dataframe with population
df_xk <- state_top10 %>%
  select(state, ESTIMATESBASE2020) %>%
  mutate(isTopK = 1)

df_xr <- Pop_state_rest %>%
  select(STNAME, ESTIMATESBASE2020) %>%
  mutate(isTopK = 0)

df_xr <- df_xr %>% rename(state = STNAME)

df_x <- rbind(df_xk, df_xr)

df_x <- df_x[df_x$state %in% stateList, ] #Remove the invalid states
names(df_x)[2] <- 'x'


# Apply the optimization
z_starMAX <- topkOptim::optim_aggregate(
  x_data = df_x,
  index = index,
  maximize = 1,
  verbose = 1
)
z_starMIN <- topkOptim::optim_aggregate(
  x_data = df_x,
  index = index,
  maximize = -1,
  verbose = 1
)

# Get complete y and x vectors
yk <- state_top10$firearm_by_city[state_top10$state %in% stateList]
y_starMAX <- c(yk, z_starMAX)
y_starMIN <- c(yk, z_starMIN)

xk <- df_x$x[df_x$isTopK == 1]
xr <- df_x$x[df_x$isTopK == 0]
x <- c(xk, xr)



# Add D.C
x = c(x, 689546)
y_starMAX = c(y_starMAX, 1345)  
y_starMIN = c(y_starMIN, 1345)


combined_results <- data.frame(x = x, y_starMAX = y_starMAX, y_starMIN = y_starMIN)
#saveRDS(combined_results_ATF, file = "combined_results.rds")


# Read RDS
combined_results <- readRDS("...\combined_results.rds")   


lmUS_MAX <- lm(log(y_starMAX) ~ log(x), data = combined_results)
lmUS_MIN <- lm(log(y_starMIN) ~ log(x), data = combined_results)


eq_max <- paste("β =", round(coef(lmUS_MAX)[2], 3), " \nR² =", round(summary(lmUS_MAX)$r.squared, 3))
eq_min <- paste("β =", round(coef(lmUS_MIN)[2], 3), " \nR² =", round(summary(lmUS_MIN)$r.squared, 3))

beta_max <- round(coef(lmUS_MAX)[2], 3)
beta_min <- round(coef(lmUS_MIN)[2], 3)

r_squared_max <- round(summary(lmUS_MAX)$r.squared, 3)
r_squared_min <- round(summary(lmUS_MIN)$r.squared, 3)


combined_results <- combined_results[combined_results$x > 1, ]


#---------------

plot_max <- ggplot(combined_results, aes(x = x, y = y_starMAX)) +
  geom_point(size = 0.2, color = "black", alpha = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#e66101", ymin = 10, size = 0.7) +
  labs(x = "Population", y = "Optimized Y") +
  annotate("text", x = (min(combined_results$x) + 2), y = 10000, 
           label = bquote(atop(hat(beta)[max] == .(beta_max),R^2 == .(r_squared_max))),
           hjust = 0, size = 3, color = "black") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10, angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.position = "none",
    axis.ticks = element_line(size = 0.4, linewidth = 0.8, color = "black"),
    axis.line = element_line(color = "black")
  ) +
  scale_x_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^0, 10^8),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^-1, 10^5),
    expand = c(0, 0)
  )+
  coord_cartesian(clip = 'off')

plot_min <- ggplot(combined_results, aes(x = x, y = y_starMIN)) +
  geom_point(size = 0.2, color = "black", alpha = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#fdb863", ymin = 0, size = 0.7) +
  labs(x = "Population", y = "Optimized Y") +
  annotate("text", x = (min(combined_results$x) + 2), y = 10000, 
           label = bquote(atop(hat(beta)[min] == .(beta_min),R^2 == .(r_squared_min))),
           hjust = 0, size = 3, color = "black") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10, angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.position = "none",
    axis.ticks = element_line(size = 0.4, linewidth = 0.8, color = "black"),
    axis.line = element_line(color = "black")
  ) +
  scale_x_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^0, 10^8),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    trans = log10_trans(), 
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^-1, 10^5),
    expand = c(0, 0)
  )+
  coord_cartesian(clip = 'off')



#Save the plot
png(file = "ATF_combined.png", width = 180, height = 100, units = "mm", res = 300)

comb_plot <- plot_min + plot_max +
  plot_annotation(
    tag_levels = list(c('A', 'B')),
    tag_prefix = "",
    theme = theme(
      plot.tag = element_text(size = 16, face = "bold"),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
    )
  )
print(comb_plot)

dev.off()
































