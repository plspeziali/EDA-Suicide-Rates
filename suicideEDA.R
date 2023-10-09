## Loading dependencies

library(kableExtra)
library(car)
library(dplyr)
library(reshape2)
library(ggplot2)
library(vioplot)
library(sf)
library(gridExtra)

library(tidyverse)
library(hrbrthemes)
library(viridis)

library(pgirmess)
library(stats)

## Changing Working Directory and Reading Dataset

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sc_data <- read.csv("./dataset/suicide.csv", header = TRUE, sep = ",")
head(sc_data)
names(sc_data)
summary(sc_data)

# Check if some columns contain interesting information

sc_data_ValueType <- unique(sc_data$ValueType)
length_of_ValueType <- length(sc_data_ValueType)
# ValueType doesn't

sc_data_Period <- unique(sc_data$Period.type)
length_of_Period <- length(sc_data_Period)
# Period.type doesn't

sc_data_Location <- unique(sc_data$Location.type)
length_of_Location <- length(sc_data_Location)
# Location.type doesn't

sc_data_Dim1 <- unique(sc_data$Dim1.type)
length_of_Dim1 <- length(sc_data_Dim1)
# Dim1.type doesn't

# sc_data_Dim2 <- unique(sc_data$Dim2.type)
# length_of_Dim2 <- length(sc_data_Dim2)
# # Dim1.type doesn't
# # We can remove all five

# Remove uninteresting columns

to_select <-
  c("ParentLocation","SpatialDimValueCode","Location","Period","IsLatestYear","Dim1","FactValueNumeric");

sc_data_R <- sc_data[ , (names(sc_data) %in% to_select)]
head(sc_data_R)
names(sc_data_R)
summary(sc_data_R)

# Rename columns

sc_data_R <- sc_data_R %>%
  rename(WorldRegion = ParentLocation,
         CountryCode = SpatialDimValueCode,
         Country = Location,
         Year = Period,
         Sex = Dim1,
         Value = FactValueNumeric)
head(sc_data_R)
names(sc_data_R)
summary(sc_data_R)

# Look for null values in the feature 'Value'
any(is.na(sc_data_R$Value))
# No null values

# Calculate no. of years for every country
year_count <- sc_data_R %>%
  group_by(Country) %>%
  summarise(NumYears = n_distinct(Year))

# Check if every country has got the same no. of years
all(year_count$NumYears == max(year_count$NumYears))
# Every country has got the same number of years

# Aggregate Sex values in the same WorldRegion + Country + CountryCode + Year row
sc_data_R <- dcast(sc_data_R, WorldRegion + Country + CountryCode + Year ~ Sex, value.var = "Value", fun.aggregate = sum)

# Sort the dataset by WorldRegion + Country + CountryCode + Year
sc_data_R <- sc_data_R[order(sc_data_R$WorldRegion, sc_data_R$Country, sc_data_R$CountryCode, sc_data_R$Year), ]
sc_data_R <- sc_data_R %>% rename(Both = 'Both sexes')

# ** Exploratory Analysis Begin **

# Hist of both sexes
hist(sc_data_R$Both, breaks = 20, main = "Value distribution", xlab = "Value", cex.main = 1.15, cex.lab = 1.15)


# Female/Male values boxplot for all years
stacked_data <- stack(sc_data_R[c("Female", "Male")])
ggplot(data = stacked_data, aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  labs(title = "Distribution of values by Sex",
       x = "Sex",
       y = "Value") +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  theme_minimal() +  # Add a title
  guides(fill = FALSE)  # Remove the legend

# Female/Male values boxplot only for the latest year (2019)
data_2019 <- sc_data_R[sc_data_R$Year == 2019, ]
# stack features "Female" and "Male"
stacked_data <- stack(data_2019[c("Female", "Male")])
# Create Male/Female boxplot
ggplot(data = stacked_data, aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  labs(title = "Distribuzione di Female e Male",
       x = "Sex",
       y = "Value") +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  theme_minimal()

ggplot(data = sc_data_R, aes(x = factor(Year), y = Both)) +
  geom_boxplot() +
  labs(title = "Distribution of Both values by Year",
       x = "Year", y = "Both") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

mean_gender <- sc_data_R %>%
  group_by(Year) %>%
  summarize(Mean_Both = mean(Both), Mean_Male = mean(Male), Mean_Female = mean(Female))

ggplot(mean_gender, aes(x = Year)) +
  geom_line(aes(y = Mean_Both, color = "Both"), size = 1.0) +
  geom_line(aes(y = Mean_Male, color = "Male"), size = 1.0) +
  geom_line(aes(y = Mean_Female, color = "Female"), size = 1.0) +
  labs(
    title = "Trend of Mean Suicide Rates Over the Years",
    x = "Year",
    y = "Mean Suicide Rate",
    color = "Sex"
  ) +
  scale_color_manual(values = c("Both" = "black", "Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  ylim(0, 25)  # Set the y-axis limits +
  theme(axis.text = element_text(size = 11),  # Imposta la dimensione del testo sugli assi
      axis.title = element_text(size = 12))

top_10_countries <- sc_data_R %>%
  group_by(CountryCode) %>%
  summarise(AvgBoth = mean(Both)) %>%
  arrange(desc(AvgBoth)) %>%
  head(10)

top_10_countries$CountryCode <- factor(top_10_countries$CountryCode, levels = rev(top_10_countries$CountryCode))

ggplot(top_10_countries, aes(x = CountryCode, y = AvgBoth, fill = CountryCode)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Average Both Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggtitle("Average Both Value by Country (Top 10)")


mean_wc <- sc_data_R %>%
  group_by(WorldRegion, Year) %>%
  summarize(Mean_Both = mean(Both))

head(mean_wc)

# Create a ggplot object with your dataset
ggplot(data = mean_wc, aes(x = Year, y = Mean_Both, group = WorldRegion, color = WorldRegion)) +
  geom_line(size = 1.0) +
  labs(x = "Year", y = "Mean Both Value", title = "Trend of Mean Both Value by World Region") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),  # Imposta la dimensione del testo sugli assi
        axis.title = element_text(size = 12), # Imposta la dimensione dei titoli degli assi
        legend.text = element_text(size = 10))

# Filtra il dataset solo per l'anno 2019
data_2019 <- mean_wc[mean_wc$Year == 2019, ]

ggplot(data = data_2019, aes(x = WorldRegion, y = Mean_Both, fill = WorldRegion)) +
  geom_bar(stat = "identity") +
  labs(title = "Istogramma di 'both' per 'WorldRegion' (2019)",
       x = "WorldRegion",
       y = "Both") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


analyze_geographic_area <- function(data, area_name) {
  # Filtra il dataset solo per l'area_name
  data_area <- data[data$WorldRegion == area_name, ]
  
  mean_gender <- data_area %>%
    group_by(Year) %>%
    summarize(Mean_Both = mean(Both), Mean_Male = mean(Male), Mean_Female = mean(Female))
  
  p1 <- ggplot(mean_gender, aes(x = Year)) +
    geom_line(aes(y = Mean_Both, color = "Both")) +
    geom_line(aes(y = Mean_Male, color = "Male")) +
    geom_line(aes(y = Mean_Female, color = "Female")) +
    labs(
      title = paste("Trend of Mean Suicide Percentages Over the Years in", area_name),
      x = "Year",
      y = "Mean Suicide Percentage",
      color = "Gender"
    ) +
    scale_color_manual(values = c("Both" = "black", "Male" = "blue", "Female" = "red")) +
    theme_minimal() +
    ylim(0, 50)  # Set the y-axis limits
  
  print(p1)
  
  # Assuming your dataset is named "sc_data_R"
  top_10_countries <- data_area %>%
    group_by(Country) %>%
    summarise(AvgBoth = mean(Both)) %>%
    arrange(desc(AvgBoth))
  
  top_10_countries$CountryCode <- factor(top_10_countries$Country, levels = rev(top_10_countries$Country))
  
  p2 <- ggplot(top_10_countries, aes(x = Country, y = AvgBoth, fill = Country)) +
    geom_bar(stat = "identity") +
    labs(x = "Country", y = "Average Both Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ggtitle("Average Both Value by Country (Top 10)")
  
  print(p2)
  
}


analyze_geographic_area(sc_data_R,"Africa")
analyze_geographic_area(sc_data_R,"Americas")
analyze_geographic_area(sc_data_R,"Eastern Mediterranean")
analyze_geographic_area(sc_data_R,"Europe")
analyze_geographic_area(sc_data_R,"South-East Asia")
analyze_geographic_area(sc_data_R,"Western Pacific")

# ## Hypothesis test for Sex values
# 
# #Verifying normality
# qqnorm(sc_data_R$Male)
# qqline(sc_data_R$Male)
# qqnorm(sc_data_R$Female)
# qqline(sc_data_R$Female)
# qqnorm(sc_data_R$Both)
# qqline(sc_data_R$Both)
# 
# shapiro_test_male <- shapiro.test(sc_data_R$Male)
# shapiro_test_female <- shapiro.test(sc_data_R$Female)
# shapiro_test_both <- shapiro.test(sc_data_R$Both)
# 
# print("verifying normality- Male:")
# print(shapiro_test_male)
# print("verifying normality - Female:")
# print(shapiro_test_female)
# print("verifying normality- Both:")
# print(shapiro_test_both)
# 
# # Mann-Whitney U test to compare Male and Female across the entire dataset
# wilcox.test(sc_data_R$Male, sc_data_R$Female, paired = FALSE)
# 
# 
# ## Hypothesis test for Geographic Areas
# install.packages("PMCMRplus")
# library(PMCMRplus)
# 
# analyze_single_country <- function(data, countryCode) {
#   
#   # Subset the data for the USA
#   cc_data <- data[data$CountryCode == countryCode, ]
#   
#   p1 <- ggplot(cc_data, aes(x = Year, y = Both)) +
#     geom_line() +
#     labs(
#       x = "Year",
#       y = "Suicide Rates (Both)",
#       title = paste("Trend of Suicide Rates in",cc_data$Country)
#     ) +
#     theme_minimal()
#   
#   print(p1)
#   
#   qqnorm(cc_data$Both)
#   qqline(cc_data$Both)
#   
#   shapiro_test_cc <- shapiro.test(cc_data$Both)
#   print(paste("verifying normality- Both in",countryCode))
#   print(shapiro_test_cc)
#   
# }
# 
# # Americas
# analyze_single_country(sc_data_R,"USA")
# # Europe
# analyze_single_country(sc_data_R,"FIN")
# # Africa
# analyze_single_country(sc_data_R,"ZAF")
# # Western Pacific
# analyze_single_country(sc_data_R,"KOR")
# # South-East Asia
# analyze_single_country(sc_data_R,"PRK")
# # Eastern Mediterranean
# analyze_single_country(sc_data_R,"EGY")
# 
# 
# 
# analyze_single_country(sc_data_R,"ITA")
# 
# 
# 
# # Subset the data for the USA
# cc_data <- sc_data_R[sc_data_R$CountryCode == "USA", ]
# # If normal, use Anova
# cc_data$Year <- factor(cc_data$Year)
# 
# anova_result <- aov(Year ~ Both + Error(Year/CountryCode), data = cc_data)
# # Print the ANOVA table
# summary(anova_result)
# 
# 
# 
# # Filter the data for the specified countries
# selected_countries <- sc_data_R[sc_data_R$CountryCode %in% c("USA", "FIN", "ZAF", "KOR", "PRK", "EGY"), ]
# 
# # Perform repeated measures ANOVA
# repeated_measures_anova <- aov(Both ~ Year + Error(CountryCode/Year), data = selected_countries)
# 
# # Print the ANOVA results
# summary(repeated_measures_anova)
# 
# library(lattice)
# bwplot(Both ~ Year | CountryCode, data = selected_countries)
