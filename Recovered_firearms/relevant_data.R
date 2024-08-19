library(dplyr)
library(stringr)



# Read ATF reported data that was manually collected
data10_ATF <- read.csv("Recovered_firearms/Data/hand_atf.csv")  
data10_ATF <- data10_ATF[!is.na(data10_ATF$year), 1:5]

# Remove Hawaii and other territories
data10_ATF <- data10_ATF %>%
  filter(!(grepl("Guam|Puerto Rico|Islands|Hawaii", state, ignore.case = TRUE)))


# Load the population data (Census Incorporated Places and Minor Civil Divisions)
popData <- read.csv("Recovered_firearms/Data/sub-est2022.csv",
  sep = ",",
  encoding = "latin1"
)


# Remove the state populations row
city_popData <- popData %>%
  filter(NAME != STNAME)

# Remove the last word in popData
city_popData <- city_popData %>%
  mutate(NAME = str_remove(NAME, "\\s\\w+$"))


# (ALTERNATIVE) Consider only the places that have "city" as last word - leads to fewer correspondences
# city_popData <- city_popData %>%
#   filter(str_detect(NAME, "\\scity$")) %>%  # Keep only rows where NAME ends with " city"
#   mutate(NAME = str_remove(NAME, "\\s\\w+$"))  # Remove the last word in NAME


# Remove rows with zero population cities
city_popData <- city_popData %>%
  filter(ESTIMATESBASE2020 > 0) 


# Keep only the first appearance of each unique name within each state in 'popData' after filtering
city_popData <- city_popData %>%
  group_by(STNAME, NAME) %>%
  filter(ESTIMATESBASE2020 == max(ESTIMATESBASE2020)) %>%
  ungroup() %>%
  distinct(STNAME, NAME, .keep_all = TRUE)


# Change some names in data10_ATF so that everything matches
data10_ATF <- data10_ATF %>%
  mutate(
    city = str_replace(city, "(?i)Saint", "St."),
    city = str_replace(city, "Juneau", "Juneau city and"),
    city = str_replace(city, "(?i)north\\s*glenn", "Northglenn"),
    city = str_replace(city, "(?i)washington,?\\s*dc", "Washington"),
    city = str_replace(city, "Ft\\. Myers", "Fort Myers"),
    city = str_replace(city, "(?i)augusta", "Augusta-Richmond County consolidated"),
    city = str_replace(city, "Coeur D'Alene", "Coeur d'Alene"),
    city = str_replace(city, "(?i)east\\s*saint\\s*louis", "East St. Louis"),
    city = str_replace(city, "(?i)lexington", "Lexington-Fayette urban"),
    city = str_replace(city, "(?i)louisville", "Louisville/Jefferson County metro"),
    city = str_replace(city, "New York City", "New York"),
    city = str_replace(city, "Carson City", "Carson"),
    city = str_replace(city, "Nashville", "Nashville-Davidson metropolitan government (balance)"),
    city = str_replace(city, "Athens", "Athens-Clarke County unified government (balance)"),
    city = str_replace(city, "Convington", "Athens-Clarke County unified government (balance)"),
    city = ifelse(state == "Montana", str_replace(city, "Butte", "Butte-Silver Bow (balance)"), city),
    city = ifelse(state == "Nebraska", str_replace(city, "Lexington-Fayette urban", "Lexington"), city),
    city = ifelse(state == "Massachusetts", str_replace(city, "Lexington-Fayette urban", "Lexington"), city),
    city = ifelse(state == "Maine", str_replace(city, "Augusta-Richmond County consolidated", "Augusta"), city),
    
  )

# Perform a left join 
merged_data <- left_join(
  x = data10_ATF,
  y = city_popData,
  by = c("state" = "STNAME", "city" = "NAME")
)


# Remove commas and convert to numeric
merged_data$firearm_by_city <- as.numeric(gsub(",", "", merged_data$firearm_by_city))


#Select only relevant columns
merged_data <- merged_data %>%
  select(year, state, all_firearms_recovered, city, firearm_by_city, ESTIMATESBASE2020, POPESTIMATE2020,
         POPESTIMATE2021, POPESTIMATE2022)


city_popData <- city_popData %>%
  select(NAME, STNAME,ESTIMATESBASE2020 ,POPESTIMATE2020, POPESTIMATE2021, POPESTIMATE2022)


#Remove the rows where firearm_by_city is NA in merged_data
merged_data <- merged_data %>%
  filter(!is.na(firearm_by_city))


# Remove rows for Pennsylvania in the year 2020
merged_data <- merged_data %>%
  filter(!(state == "Pennsylvania" & year == 2020))

# Remove the rows where ESTIMATESBASE2020 is NA in merged_data
merged_data <- merged_data %>%
  filter(!is.na(ESTIMATESBASE2020))





















