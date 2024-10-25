library(dplyr)

# Data
# Run for each year
df <- read.table("Firearm_homicides/Data/FirearmHomicides 2016.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
aux <- read.table("Firearm_homicides/Data/FirearmHomicides 2016MSA.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------

total_deaths <- as.numeric(tail(df$Deaths, 1))
df <- df[-nrow(df), ]
sum_deaths <- sum(as.numeric(df$Deaths), na.rm = TRUE)
deaths_nonMSA <- as.numeric(tail(aux$Deaths, 2)[1])

#Aggregare the counties in MSA
df_MSA <- left_join(df, MSA %>% select(`County Code`, `MSA Code`), by = c("County.Code" = "County Code"))
df_MSA <- df_MSA %>%
  dplyr::filter(!is.na(`MSA Code`))


df_MSA <- dplyr::group_by(df_MSA, `MSA Code`) %>%
  dplyr::summarise(
    deaths = sum(as.numeric(Deaths), na.rm = TRUE),
    maxZ = 9 * sum(Deaths == "Suppressed"),
    minZ = 1 * sum(Deaths == "Suppressed"),
    Population = sum(as.numeric(Population), na.rm = TRUE),
    hasSuppressed = ifelse(any(Deaths == "Suppressed"), 1, 0)
  ) %>%
  dplyr::mutate(deaths = ifelse((hasSuppressed == 1) & (deaths == 0), NA, deaths)) %>%
  as.data.frame()

df_MSA_rest <- df_MSA[df_MSA$hasSuppressed == 1,] 
df_MSA_TopK <- df_MSA[df_MSA$hasSuppressed == 0,] 
df_MSA_TopK <- df_MSA_TopK[df_MSA_TopK$deaths != 0, ]


x <- c(df_MSA_TopK$Population, df_MSA_rest$Population)
xr <- df_MSA_rest$Population

df_MSA_rest$deaths[is.na(df_MSA_rest$deaths)] <- 0


#------------------------ Give the max to the highest populations --------------
# # Comment to get b_min

# Order df_MSA_rest in decreasing order by population
df_MSA_rest <- df_MSA_rest[order(-df_MSA_rest$Population), ]

# Parameters
z0 <- ifelse(is.na(df_MSA_rest$deaths), df_MSA_rest$minZ, (df_MSA_rest$deaths + df_MSA_rest$minZ))
steps <- total_deaths - deaths_nonMSA - sum(df_MSA_TopK$deaths) - sum(z0)   #y_left updated

maxZ = df_MSA_rest$maxZ+df_MSA_rest$deaths

xr_increasing <- sort(xr, decreasing = TRUE)

linear_z_star <- z0
for (i in seq_along(maxZ)) {
  while (steps > 0 && linear_z_star[i] != maxZ[i]) {
    if (linear_z_star[i] < maxZ[i]) {
      linear_z_star[i] <- linear_z_star[i] + 1  
      steps <- steps - 1                        
    }
  }
}



#------------------------ Give the max to the lowest populations ---------------
# #Uncomment to get b_min

# # Order df_MSA_rest in decreasing order by population
# df_MSA_rest <- df_MSA_rest[order(df_MSA_rest$Population), ]
# 
# # Parameters
# z0 <- ifelse(is.na(df_MSA_rest$deaths), df_MSA_rest$minZ, (df_MSA_rest$deaths + df_MSA_rest$minZ))
# steps <- total_deaths - deaths_nonMSA - sum(df_MSA_TopK$deaths) - sum(z0)   #y_left updated
# 
# maxZ = df_MSA_rest$maxZ+df_MSA_rest$deaths
# 
# xr_increasing <- sort(xr, decreasing = FALSE)
# 
# linear_z_star <- z0
# for (i in seq_along(maxZ)) {
#   while (steps > 0 && linear_z_star[i] != maxZ[i]) {
#     if (linear_z_star[i] < maxZ[i]) {
#       linear_z_star[i] <- linear_z_star[i] + 1  # Increment the value in linear_z_star
#       steps <- steps - 1                         # Decrease the number of steps
#     }
#   }
# }
# 
# 
# 
# maxZ
# linear_z_star


#------------------------ Calculation ------------------------------------------
linear_y_star <- c(df_MSA_TopK$deaths, linear_z_star)
linear_x_star <- c(df_MSA_TopK$Population, xr_increasing)
linear_model_max <- lm(log(linear_y_star) ~ log(linear_x_star))

linear_beta_max = linear_model_max$coefficients[2]
linear_beta_max


