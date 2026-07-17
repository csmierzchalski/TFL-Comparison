# COMP5007 Coursework
# loading libraries
library(dplyr)
library(readr)
library(ggplot2)

# importing data
temp_raw     <- read_csv("lu-average-monthly-temperatures 2013-2024.csv")
journeys_raw <- read_csv("tfl-journeys-type.csv", locale = locale(encoding = "latin1"))
cc_raw       <- read_csv("tfl-vehicles-c-charge-zone.csv", locale = locale(encoding = "latin1"))

# exploring structure of data
str(temp_raw)
summary(temp_raw)
ncol(temp_raw)
nrow(temp_raw)

str(journeys_raw)
summary(journeys_raw)
ncol(journeys_raw)
nrow(journeys_raw)

str(cc_raw)
summary(cc_raw)
ncol(cc_raw)
nrow(cc_raw)

## cleaning data
# temperature data
# Keeping first 10 columns
temp_clean <- temp_raw[, 1:10]

# Creating a usable date
temp_clean$Date <- as.Date(paste(temp_clean$Year, temp_clean$Month, "01", sep = "-"),
                           format = "%Y-%B-%d")

# Average across all Underground lines (column 3 to 10)
temp_clean$Avg_Underground_Temp <- rowMeans(temp_clean[, 3:10], na.rm = TRUE)

# Keep only Date and the average
monthly_temp <- temp_clean[, c("Date", "Avg_Underground_Temp")]

# jounrye data
# shortening column names
colnames(journeys_raw) <- c("FinYear", "ReportPeriod", "Days",
                            "PeriodBegin", "PeriodEnd",
                            "Bus", "Underground", "DLR", "Tram",
                            "Overground", "CableCar", "TfLRail")

# Remove the empty bottom row
journeys_clean <- journeys_raw[!is.na(journeys_raw$PeriodBegin) & journeys_raw$PeriodBegin != "", ]

# turning character date into r date
journeys_clean$Period_start <- as.Date(journeys_clean$PeriodBegin, format = "%d-%b-%y")

# extracting month and year from data
journeys_clean$Year  <- as.numeric(format(journeys_clean$Period_start, "%Y"))
journeys_clean$Month <- as.numeric(format(journeys_clean$Period_start, "%m"))

# converting to numerical data
journeys_clean$Bus         <- as.numeric(journeys_clean$Bus)
journeys_clean$Underground <- as.numeric(journeys_clean$Underground)
journeys_clean$DLR         <- as.numeric(journeys_clean$DLR)
journeys_clean$Tram        <- as.numeric(journeys_clean$Tram)
journeys_clean$Overground  <- as.numeric(journeys_clean$Overground)
journeys_clean$CableCar    <- as.numeric(journeys_clean$CableCar)
journeys_clean$TfLRail     <- as.numeric(journeys_clean$TfLRail)

# Total public transport journeys (millions)
journeys_clean$Total_Public <- journeys_clean$Bus + journeys_clean$Underground +
  journeys_clean$DLR + journeys_clean$Tram + journeys_clean$Overground +
  journeys_clean$CableCar + journeys_clean$TfLRail

# Sum to monthly totals (each row is a 4/5‑week period, so this is approximate)
monthly_journeys <- journeys_clean %>%
  group_by(Year, Month) %>%
  summarise(
    Underground    = sum(Underground, na.rm = TRUE),
    Total_Public   = sum(Total_Public, na.rm = TRUE),
    .groups = "drop"
  )

# making a monthly Date for merging
monthly_journeys$Date <- as.Date(paste(monthly_journeys$Year,
                                       monthly_journeys$Month,
                                       "01", sep = "-"),
                                 format = "%Y-%m-%d")

# congestion charge data
# converting format of date "Month-YY" to proper date
cc_raw$Date <- as.Date(paste0("01-", cc_raw$Month), format = "%d-%b-%y")

# keeping rows with confirmed vehicle observed
cc_clean <- cc_raw[!is.na(cc_raw$`CC Confirmed Vehicles observed during Charging Hours`) &
                     cc_raw$`CC Confirmed Vehicles observed during Charging Hours` != "", ]

# shortening column name
colnames(cc_clean)[colnames(cc_clean) == "CC Confirmed Vehicles observed during Charging Hours"] <- "Confirmed_Vehicles"
colnames(cc_clean)[colnames(cc_clean) == "Number of Charging Day in Month"] <- "Charging_Days"

# converting to numeric
cc_clean$Confirmed_Vehicles <- as.numeric(cc_clean$Confirmed_Vehicles)
cc_clean$Charging_Days      <- as.numeric(cc_clean$Charging_Days)

# average private vehicles per charging day
cc_clean$Daily_Private <- cc_clean$Confirmed_Vehicles / cc_clean$Charging_Days

# extracting month and year from date
cc_clean$Year  <- as.numeric(format(cc_clean$Date, "%Y"))
cc_clean$Month <- as.numeric(format(cc_clean$Date, "%m"))

# keeping columns needed for merging 
monthly_cc <- cc_clean[, c("Date", "Year", "Month", "Daily_Private")]

# merging datasets
# merging monthly journeys to monthly temp
analysis_monthly <- merge(monthly_journeys, monthly_temp, by = "Date", all = TRUE)

# adding seasons for seasonal analysis
analysis_monthly <- analysis_monthly %>%
  mutate(
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% 3:5        ~ "Spring",
      Month %in% 6:8        ~ "Summer",
      Month %in% 9:11       ~ "Autumn"
    )
  )

# merging congestion charge data (some fields are missing so will be NA but no issue)
analysis_monthly <- merge(analysis_monthly, monthly_cc[, c("Date", "Daily_Private")],
                          by = "Date", all.x = TRUE)

# saving merged datasets
write.csv(analysis_monthly, "analysis_monthly.csv", row.names = FALSE)

# statistical summary

# Overall summaries
summary(temp_clean$Avg_Underground_Temp)
summary(monthly_journeys$Underground)
summary(monthly_journeys$Total_Public)
summary(monthly_cc$Daily_Private)


# Standard deviation of temperature
sd(temp_clean$Avg_Underground_Temp, na.rm = TRUE)

# Mean temperature by month (to find coldest/warmest)
monthly_temp$Month_num <- as.numeric(format(monthly_temp$Date, "%m"))
mean_temp_by_month <- tapply(monthly_temp$Avg_Underground_Temp, monthly_temp$Month_num, mean,)
print(round(mean_temp_by_month, 1))

# Proportion of Underground in total public transport
underground_proportion <- mean(monthly_journeys$Underground[monthly_journeys$Total_Public > 0] / monthly_journeys$Total_Public[monthly_journeys$Total_Public > 0]) * 100
round(underground_proportion, 1)

# Daily private vehicles from October 2016 onward
cc_after2016 <- cc_clean[cc_clean$Date >= as.Date("2016-10-01") & !is.na(cc_clean$Daily_Private), ]
mean(cc_after2016$Daily_Private)
sd(cc_after2016$Daily_Private)

# Median Underground journeys by season
season_medians <- tapply(analysis_monthly$Underground, analysis_monthly$Season, median, na.rm = TRUE)
cat("Median Underground journeys by season:\n")
print(season_medians)

# Histogram to see distribution shape of temperature
hist(monthly_temp$Avg_Underground_Temp,
     main = "Distribution of Monthly Average Underground Temperatures",
     xlab = "Temperature (°C)", col = "lightblue", breaks = 15)


# find patterns

# Filter to rows with temperature available
temp_analysis <- analysis_monthly[!is.na(analysis_monthly$Avg_Underground_Temp), ]

# Correlation between temperature and usage

cor(temp_analysis$Avg_Underground_Temp, temp_analysis$Underground)
cor(temp_analysis$Underground, temp_analysis$Avg_Underground_Temp)
cat("Correlation (temperature vs total public transport):",
    cor(temp_analysis$Avg_Underground_Temp, temp_analysis$Total_Public, use = "complete.obs")

# Scatter plot with trend line
ggplot(temp_analysis, aes(x = Avg_Underground_Temp, y = Underground)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(title = "Underground Journeys vs Average Underground Temperature",
       x = "Average Underground Temperature (°C)",
       y = "Underground Journeys (millions)")

# Seasonal patterns: boxplot of Underground by season
ggplot(analysis_monthly, aes(x = Season, y = Underground)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Underground Journeys by Season",
       x = "Season", y = "Underground Journeys (millions)")

# Line chart: monthly trends in public transport and private vehicles
ggplot(analysis_monthly[!is.na(analysis_monthly$Daily_Private), ],
       aes(x = Date)) +
  geom_line(aes(y = Total_Public, colour = "Total Public Transport")) +
  geom_line(aes(y = Daily_Private * 0.01, colour = "Daily Private Vehicles x100")) +
  labs(title = "Public Transport vs Private Vehicles",
       y = "Journeys (millions) / Vehicles",
       colour = "Legend")

# Policy impact: private vehicles with ULEZ milestones
policy_dates <- as.Date(c("2019-04-08",   # ULEZ Central London
                          "2021-10-25",   # ULEZ expansion to inner London
                          "2023-08-29"))  # ULEZ expansion to all London

ggplot(analysis_monthly[!is.na(analysis_monthly$Daily_Private), ],
       aes(x = Date, y = Daily_Private)) +
  geom_line(colour = "darkorange", size = 1) +
  geom_vline(xintercept = policy_dates, linetype = "dashed", colour = "red") +
  labs(title = "Private Vehicles in Congestion Zone and Policy Dates",
       y = "Average Daily Confirmed Vehicles")

# Policy impact: public transport with same milestones
ggplot(analysis_monthly[!is.na(analysis_monthly$Daily_Private), ],
       aes(x = Date, y = Total_Public)) +
  geom_line(colour = "darkgreen", size = 1) +
  geom_vline(xintercept = policy_dates, linetype = "dashed", colour = "red") +
  labs(title = "Total Public Transport and Policy Dates",
       y = "Total Public Journeys (millions)")

# Before/after comparison for the October 2021 ULEZ expansion
analysis_monthly$Post_Oct2021 <- analysis_monthly$Date >= as.Date("2021-10-25")

policy_data <- analysis_monthly[!is.na(analysis_monthly$Daily_Private) &
                                  !is.na(analysis_monthly$Total_Public), ]

before_after_public <- tapply(policy_data$Total_Public, policy_data$Post_Oct2021, mean)
before_after_private <- tapply(policy_data$Daily_Private, policy_data$Post_Oct2021, mean, na.rm = TRUE)

#Before/after Oct 2021 ULEZ - Mean total public transport (millions)
print(before_after_public)
#Before/after Oct 2021 ULEZ - Mean daily private vehicles
print(before_after_private)

# Percentage change (if both groups exist)
if (length(before_after_public) == 2) {
  pct_change_public <- (before_after_public["TRUE"] - before_after_public["FALSE"]) / before_after_public["FALSE"] * 100
  pct_change_private <- (before_after_private["TRUE"] - before_after_private["FALSE"]) / before_after_private["FALSE"] * 100 }
#Percentage change in total public transport:
  round(pct_change_public, 1)
#Percentage change in daily private vehicles
  round(pct_change_private, 1)
