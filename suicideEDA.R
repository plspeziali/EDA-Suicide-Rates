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
  c("ParentLocation","Location","Period","IsLatestYear","Dim1","FactValueNumeric");

sc_data_R <- sc_data[ , (names(sc_data) %in% to_select)]
head(sc_data_R)
names(sc_data_R)
summary(sc_data_R)

# Rename columns

sc_data_R <- sc_data_R %>%
  rename(WorldRegion = ParentLocation,
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

# Aggregate Sex values in the same WorldRegion + Country + Year row
sc_data_R <- dcast(sc_data_R, WorldRegion + Country + Year ~ Sex, value.var = "Value", fun.aggregate = sum)

# Sort the dataset by WorldRegion + Country + Year
sc_data_R <- sc_data_R[order(sc_data_R$WorldRegion, sc_data_R$Country, sc_data_R$Year), ]
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


