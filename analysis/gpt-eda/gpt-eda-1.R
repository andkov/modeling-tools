1. describe each variable in the World Bank dataset
2. how can I load this dataset in R via an API?
  

# Load the WDI package
library(WDI)

# Retrieve the GDP per capita for Canada and the United States for the years 2010-2020
data <- WDI(country = c("CA", "US"), indicator = "NY.GDP.PCAP.CD", start = 2010, end = 2020)

# View the first few rows of the data
head(data)


# Load the necessary packages
library(WDI)
library(ggplot2)

# Retrieve the GDP per capita for Canada and the United States for the years 1990-2020
data <- WDI(country = c("CA", "US"), indicator = "NY.GDP.PCAP.CD", start = 1990, end = 2020)

# Plot the data using ggplot2
ggplot(data, aes(x = year, y = NY.GDP.PCAP.CD, color = country)) + 
  geom_line() + 
  labs(x = "Year", y = "GDP per capita (current US$)", color = "Country") + 
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  theme_classic()


# Load the necessary packages
library(WDI)
library(ggplot2)

# Retrieve the GDP per capita for Canada, the United States, and Ukraine for the years 1990-2020
data <- WDI(country = c("CA", "US", "UA"), indicator = "NY.GDP.PCAP.CD", start = 1990, end = 2020)

# Plot the data using ggplot2
ggplot(data, aes(x = year, y = NY.GDP.PCAP.CD, color = country)) + 
  geom_line() + 
  labs(x = "Year", y = "GDP per capita (current US$)", color = "Country") + 
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  theme_classic()


# Load the necessary packages
library(WDI)
library(ggplot2)

# Retrieve the GDP per capita and population data for Canada, the United States, and Ukraine for the years 1990-2020
data <- WDI(country = c("CA", "US", "UA"), indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL"), start = 1990, end = 2020)

# Calculate the total GDP for each country by multiplying the GDP per capita by the total population
data$total_gdp <- data$NY.GDP.PCAP.CD * data$SP.POP.TOTL

# Plot the data using ggplot2
ggplot(data, aes(x = year, y = total_gdp, color = country)) + 
  geom_line() + 
  labs(x = "Year", y = "Total GDP (current US$)", color = "Country") + 
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  theme_classic()
