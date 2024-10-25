future::plan("cluster", workers = 10)
options(future.globals.maxSize = 850 * 1024^2)

library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(patchwork)

MSA <- read_excel("Firearm_homicides/Data/qcew-county-msa-csa-crosswalk.xlsx", sheet = 3)                      


# Data for Firearm homicides retrieved from the CDC website
fileList_df <- c(
  "Firearm_homicides/Data/FirearmHomicides 2016.txt",
  "Firearm_homicides/Data/FirearmHomicides 2017.txt",
  "Firearm_homicides/Data/FirearmHomicides 2018.txt",
  "Firearm_homicides/Data/FirearmHomicides 2019.txt",
  "Firearm_homicides/Data/FirearmHomicides 2020.txt")


fileList_aux <- c(
  "Firearm_homicides/Data/FirearmHomicides 2016MSA.txt",
  "Firearm_homicides/Data/FirearmHomicides 2017MSA.txt",
  "Firearm_homicides/Data/FirearmHomicides 2018MSA.txt",
  "Firearm_homicides/Data/FirearmHomicides 2019MSA.txt",
  "Firearm_homicides/Data/FirearmHomicides 2020MSA.txt")




results <- future.apply::future_lapply(1:length(fileList_df), function(i) {
  df <- read.table(fileList_df[i], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  aux <- read.table(fileList_aux[i], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  
  # Extract total deaths
  total_deaths <- as.numeric(tail(df$Deaths, 1))

  df <- df[-nrow(df), ]
  sum_deaths <- sum(as.numeric(df$Deaths), na.rm = TRUE)
  deaths_nonMSA <- as.numeric(tail(aux$Deaths, 2)[1])
  
  
  # Aggregare the counties in MSA
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
  
  # Initial conditions
  z0 <- ifelse(is.na(df_MSA_rest$deaths), df_MSA_rest$minZ, (df_MSA_rest$deaths + df_MSA_rest$minZ))
  
  
  
  steps <- total_deaths - deaths_nonMSA - sum(df_MSA_TopK$deaths) - sum(z0)   #y_left updated

  
    z_starMAX <- topkOptim::optim(
      xr = xr,
      x = x,
      z0 = z0,
      steps = steps,
      maxZ = df_MSA_rest$maxZ+df_MSA_rest$deaths,
      maximize = 1,
      verbose = -1
    )
    
    
    z_starMIN <- topkOptim::optim(
      xr = xr,
      x = x,
      z0 = z0,
      steps = steps,
      maxZ = df_MSA_rest$maxZ+df_MSA_rest$deaths,
      maximize = -1,
      verbose = -1
    )
    
    # Output
    y_starMAX <- c(df_MSA_TopK$deaths, z_starMAX)
    x_starMAX <- x
    model_max <- lm(log(y_starMAX) ~ log(x_starMAX))
    
    y_starMIN <- c(df_MSA_TopK$deaths, z_starMIN)
    x_starMIN <- x
    model_min <- lm(log(y_starMIN) ~ log(x_starMIN))
    
    
    
    lm10 <- lm(log(deaths) ~ log(Population), data = df_MSA_TopK)
    # Get t-statistic confidence interval
    ci10 = confint(lm10)

    
    
    return(data.frame(
      total_deathsMSA = total_deaths - deaths_nonMSA,
      Sum_known = total_deaths - deaths_nonMSA - sum(df_MSA_TopK$deaths),
      R2_k = summary(lm10)$r.squared,
      lmBeta_K = lm10$coefficients[2],
      lmBeta_K_ll = ci10[2, 1],
      lmBeta_K_ul = ci10[2, 2],
      beta_max = model_max$coefficients[2],
      beta_min = model_min$coefficients[2]
    ))
}, future.seed = TRUE)

results <- do.call(rbind, results)

results$Year <- c(2016,2017,2018,2019,2020)
results$Year <- factor(results$Year, levels = unique(results$Year)) # Convert Year column to factor with correct levels
row.names(results) <- NULL





















