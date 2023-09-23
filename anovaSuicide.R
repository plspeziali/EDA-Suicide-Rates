## Loading dependencies

library(kableExtra)
library(dplyr)
library(ggplot2)
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

selected_years <- c(2000,2003,2006,2009,2012,2015,2018)
# Filter the data for the specified countries
selected_countries <- sc_data_R %>%
  filter(CountryCode %in% c("USA", "FIN", "ZAF", "KOR", "PRK", "EGY")) %>%
  filter(Year %in% selected_years) %>%
  select(CountryCode, Year, Both)

# selected_years <- c(2000,2003,2006,2009,2012,2015,2018)
# # Filter the data for the specified countries
# selected_countries <- sc_data_R %>%
#   filter(CountryCode %in% c("USA", "FIN", "JPN", "IRN", "PRK", "EGY")) %>%
#   filter(Year %in% selected_years) %>%
#   select(CountryCode, Year, Both)

for (i in selected_years) {
  print(summary(selected_countries$Both[selected_countries$Year==i]))
}

boxplot(selected_countries$Both ~ selected_countries$Year, 
        xlab="Year",
        ylab="Suicide Rate")

# trasforma variabili in factors (v. qualitative)
country <- as.factor(selected_countries$Country)
year    <- as.factor(selected_countries$Year)


# Pivot the data to create the desired table
table_data <- selected_countries %>%
  pivot_wider(names_from = Year, values_from = Both)

# Print the resulting table
print(table_data)


# Calculate the mean by rows (excluding the CountryCode column)
row_means <- rowMeans(table_data[, -1])

# Create a data frame to store the row means
cc <- unique(selected_countries$CountryCode)
row_means_df <- data.frame(Country = cc, RowMean = row_means)

# Print the row means
print(row_means_df)

# Calculate the mean by columns
col_means <- colMeans(table_data[, -c(1)])

# Create a data frame to store the column means
col_means_df <- data.frame(Year = names(col_means), ColumnMean = col_means)

# Print the column means
print(col_means_df)

vect_data <- (table_data[, -1])

# Overall sample mean
overall_mean <- mean(unlist(vect_data))

MQ_gr <- sum((col_means - overall_mean)^2) * length(row_means)
MQ_sub <- sum((row_means - overall_mean)^2) * length(col_means)

MQ_res <- 0

for(i in 1:length(row_means)){
  for(j in 1:length(col_means)){
    MQ_res <- MQ_res + (vect_data[[i,j]] - row_means[[i]] - col_means[[j]] + overall_mean)^2
  }
}


library(lattice)
bwplot(Both ~ Year | CountryCode, data = selected_countries)

# Set up the plotting grid
par(mfrow = c(3,3))

# Loop through each year
for (year in selected_years) {
  # Filter the data for the current year
  year_data <- table_data[, c("CountryCode", year)]
  
  # Create a QQ-plot for the current year
  qqnorm(unlist(year_data[, -1]), main = paste("QQ-Plot for Year", year))
  
  # Add a Q-line to the QQ-plot
  qqline(unlist(year_data[, -1]), col = "red")
}

# Create an empty data frame to store the Shapiro-Wilk test results
shapiro_results <- data.frame(Year = numeric(), W = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop through each year
for (year in 2:ncol(table_data)) {
  # Extract the data for the current year
  year_data <- table_data[[year]]
  
  # Perform the Shapiro-Wilk test
  shapiro_test_result <- shapiro.test(year_data)
  
  # Store the results in the data frame
  shapiro_results <- rbind(shapiro_results, data.frame(Year = year, W = shapiro_test_result$statistic, p_value = shapiro_test_result$p.value))
}

# Print the Shapiro-Wilk test results
print(shapiro_results)

anova_result <- aov(Both ~ Year + Error(CountryCode/Year), data = selected_countries)

# Print the ANOVA summary
summary(anova_result)


# verifica differenze fra i soggetti: 
# statistica test F_sub = MQ_sub/MQ_res
F_sub <- MQ_sub/MQ_res
F_sub
p_value <- 1 - pf(F_sub, (length(row_means)-1), (length(row_means)-1) * (length(col_means)-1))
p_value
# anche le differenze fra soggetti sono significative

# verifica sfericità 
# statistica Greenhouse-Geisser
S <- var(vect_data) # matrice di covarianza
J <- length(col_means)
numeratore <- J^2*mean(diag(S)-mean(S))^2
denominatore <- (J-1)*(sum(S^2)-2*J*sum(apply(S,1,mean)^2)+
                         +J^2*mean(S)^2)
epsilon <- numeratore/denominatore
epsilon
# piccola deviazione dalla sfericità, non influenza risultato


# analisi post-hoc
pairwise.t.test(selected_countries$Both,selected_countries$Year,paired=T,p.adj="bonferroni")
pairwise.t.test(selected_countries$Both,selected_countries$Year,paired=T,p.adj="BH")



# coefficiente di correlazione intraclasse
library(irr)
icc(vect_data)
# correlazione 0.65 
# fra coppie di osservazioni dello stesso soggetto a dosi diverse
# lo stesso ciclista ottiene prestazioni diverse a seconda della dose


head(table_data)


# test non parametrico di Friedman
friedman.test(selected_countries$Both,selected_countries$Year, selected_countries$CountryCode)

# analisi post-hoc non parametrica
pairwise.wilcox.test(selected_countries$Both,selected_countries$Year,paired=T,p.adj="bonferroni")
pairwise.wilcox.test(selected_countries$Both,selected_countries$Year,paired=T,p.adj="BH")