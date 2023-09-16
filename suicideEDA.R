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
  labs(title = "Distribuzione di Female e Male",
       x = "Sex",
       y = "Value") +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  theme_minimal()

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
  labs(title = "Boxplot di Total per Anno",
       x = "Anno", y = "Total") +
  theme_minimal()

mean_gender <- sc_data_R %>%
  group_by(Year) %>%
  summarize(Mean_Both = mean(Both), Mean_Male = mean(Male), Mean_Female = mean(Female))

ggplot(mean_gender, aes(x = Year)) +
  geom_line(aes(y = Mean_Both, color = "Both")) +
  geom_line(aes(y = Mean_Male, color = "Male")) +
  geom_line(aes(y = Mean_Female, color = "Female")) +
  labs(
    title = "Trend of Mean Suicide Percentages Over the Years",
    x = "Year",
    y = "Mean Suicide Percentage",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Both" = "black", "Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  ylim(0, 25)  # Set the y-axis limits


mean_wc <- sc_data_R %>%
  group_by(WorldRegion, Year) %>%
  summarize(Mean_Both = mean(Both))

head(mean_wc)

# Create a ggplot object with your dataset
ggplot(data = mean_wc, aes(x = Year, y = Mean_Both, group = WorldRegion, color = WorldRegion)) +
  geom_line() +
  labs(x = "Year", y = "Mean Both Value", title = "Trend of Mean Both Value by World Region") +
  theme_minimal()

# Filtra il dataset solo per l'anno 2019
data_2019 <- mean_wc[mean_wc$Year == 2019, ]

ggplot(data = data_2019, aes(x = WorldRegion, y = Mean_Both, fill = WorldRegion)) +
  geom_bar(stat = "identity") +
  labs(title = "Istogramma di 'both' per 'WorldRegion' (2019)",
       x = "WorldRegion",
       y = "Value") +
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
  
  mean_country <- data_area %>%
    group_by(Country, CountryCode) %>%
    summarize(Mean_Both = mean(Both))# %>%
    #filter(Mean_Both > 15) %>%
    #slice(1:25) %>%
    #arrange(desc(Mean_Both))
  
  p2 <- ggplot(data = mean_country, aes(x = reorder(CountryCode, -Mean_Both), y = Mean_Both, fill = CountryCode)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Istogramma di 'both' per", area_name),
         x = "WorldRegion",
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p2)
  
}


analyze_geographic_area(sc_data_R,"Africa")
analyze_geographic_area(sc_data_R,"Americas")
analyze_geographic_area(sc_data_R,"Eastern Mediterranean")
analyze_geographic_area(sc_data_R,"Europe")
analyze_geographic_area(sc_data_R,"South-East Asia")
analyze_geographic_area(sc_data_R,"Western Pacific")


# Esegui il test di Shapiro-Wilk per la variabile "Male"
shapiro_male <- shapiro.test(sc_data_R$Male)

# Esegui il test di Shapiro-Wilk per la variabile "Female"
shapiro_female <- shapiro.test(sc_data_R$Female)

# Stampare i risultati dei test
print("Shapiro-Wilk Test for Male:")
print(shapiro_male)
print("\nShapiro-Wilk Test for Female:")
print(shapiro_female)



# Calculate confidence intervals for "Male"
ci_male <- sc_data_R %>%
  summarize(
    lower_ci = t.test(Male)$conf.int[1],
    upper_ci = t.test(Male)$conf.int[2]
  )

# Calculate confidence intervals for "Female"
ci_female <- sc_data_R %>%
  summarize(
    lower_ci = t.test(Female)$conf.int[1],
    upper_ci = t.test(Female)$conf.int[2]
  )


if (ci_male$upper_ci < ci_female$lower_ci || ci_female$upper_ci < ci_male$lower_ci) {
  print("The confidence intervals do not overlap")
} else {
  print("The confidence intervals overlap")
}

mean_male <- mean(sc_data_R$value[sc_data_R$Male])
mean_female <- mean(sc_data_R$value[sc_data_R$Female])

cat("Average of new cases per male:", mean_male, "\n")
cat("Average of new cases per female:", mean_female, "\n")

sd_male <- sd(sc_data_R$value[sc_data_R$Male])
sd_female <- sd(sc_data_R$value[sc_data_R$Female])

cat("Standard deviation of new cases for males:", sd_male, "\n")
cat("Standard deviation of new cases for females:", sd_female, "\n")

median_male <- median(sc_data_R$value[sc_data_R$Male])
median_female <- median(sc_data_R$value[sc_data_R$Female])

cat("Median of new cases per males:", median_male, "\n")
cat("Median of new cases per females:", median_female, "\n")

percentage_difference <- ((mean_female - mean_male) / mean_male) * 100

cat("Percentage difference in average new cases between males and females:", percentage_difference, "%\n")

#Verifying normality
qqnorm(sc_data_R$value[sc_data_R$Male])
qqline(sc_data_R$value[sc_data_R$Male], col = "red")
qqnorm(sc_data_R$value[sc_data_R$Female])
qqline(sc_data_R$value[sc_data_R$Female], col = "red")

shapiro_test_male <- shapiro.test(sc_data_R$value[sc_data_R$Male])
shapiro_test_female <- shapiro.test(sc_data_R$value[sc_data_R$Female])

print("verifying normality- Male:")
print(shapiro_test_male)
print("verifying normality - Female:")
print(shapiro_test_female)


