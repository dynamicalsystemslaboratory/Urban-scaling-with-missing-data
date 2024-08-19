library(dplyr)


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



# Add D.C.
x = c(x, 689546)
y_starMAX = c(y_starMAX, 1345)  
y_starMIN = c(y_starMIN, 1345)


combined_results <- data.frame(x = x, y_starMAX = y_starMAX, y_starMIN = y_starMIN)


lmUS_MAX <- lm(log(y_starMAX) ~ log(x), data = combined_results)
lmUS_MIN <- lm(log(y_starMIN) ~ log(x), data = combined_results)

beta_min <- lmUS_MIN$coefficients[2]
beta_max <- lmUS_MAX$coefficients[2]














































