library(ggplot2)
library(dplyr)

#House Prices vs Average Download Speed by County

housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
housing_data <- housing_data %>%
  select(Postcode, Price, County)

download_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv')
download_data <- download_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  rename(Postcode = postcode_space)

combined_data <- merge(housing_data, download_data, by = "Postcode")

ggplot(data = combined_data, aes(x = Price, y = `Average download speed (Mbit/s)`, color = County)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~County) +
  labs(title = "House Prices vs Average Download Speed", 
       x = "House Price", 
       y = "Average Download Speed (Mbit/s)") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()


#Attainment 8 Score VS House Price

school_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv')
school_data <- school_data %>%
  filter(YEAR == 2022) %>%
  select(PCODE, ATT8SCR) %>%
  rename(Postcode = "PCODE")

housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
housing_data <- housing_data %>%
  select(Postcode, Price, County)

combined_data <- merge(school_data, housing_data, by = "Postcode")

ggplot(data = combined_data, aes(x = ATT8SCR, y = Price, color = County)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~County) +
  labs(title = "Attainment 8 Score vs House Price", 
       x = "Attainment 8 Score", 
       y = "House Price") +
  theme_minimal()



#Average Download Speed VS Attainment 8 Score

download_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv')
download_data <- download_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`, County) %>%
  rename(Postcode = postcode_space)

school_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv')
school_data <- school_data %>%
  filter(YEAR == 2022) %>%
  select(PCODE, ATT8SCR) %>%
  rename(Postcode = "PCODE")

combined_data <- merge(download_data, school_data, by = "Postcode")

ggplot(data = combined_data, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR, color = County)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~County) +
  labs(title = "Average Download Speed vs Attainment 8 Score", 
       x = "Average Download Speed (Mbit/s)", 
       y = "Attainment 8 Score") +
  theme_minimal()



#house price vs Drug Rates
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
crime<-read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")

housing = housing_data %>%
  filter(Year == 2023) %>%
  mutate(Postcode = str_trim((substring(Postcode, 1, 6))))%>%
  distinct()

drug_offences <- crime %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  group_by(postcode) %>%
  summarize(Drug_Rates = sum(count)) %>%
  distinct() %>%
  na.omit()

combined_data=housing %>%
  left_join(drug_offences, by = c("Postcode" = "postcode")) %>%
  distinct()%>%
  na.omit()

View(combined_data)

bristol_data <- combined_data %>%
  filter(County == "CITY OF BRISTOL") %>%
  distinct()

cornwall_data <- combined_data %>%
  filter(County == "CORNWALL") %>%
  distinct()

ggplot(bristol_data, aes(x = Price, y = Drug_Rates)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatterplot of House Prices vs Drug Rates in Bristol (2023)",
    x = "House Prices",
    y = "Drug Rates"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

ggplot(bristol_data, aes(x = Price, y = Drug_Rates)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatterplot of House Prices vs Drug Rates in Cornwall (2023)",
    x = "House Prices",
    y = "Drug Rates"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()













broadband_dataset <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_broadband_data.csv")

drug_offences_2020_2023 <- read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv") %>%
  filter(`Crime type` == "Drugs", `Year`(date) >= 2020 & `Year`(date) <= 2023) 



drug_offences_rate_2020_2023 <- drug_offences_2020_2023 %>%
  # Extract month and year from the date for grouping
  mutate(
    month = floor_date(ymd(date), "month"),
    Year = year(date)
  ) %>%
  # Group by year and county, then count the total number of offences
  group_by(year, County) %>%
  summarize(
    total_offences = n(),
    .groups = "drop"
  ) %>%
  # Join with population data to get population numbers for each county
  left_join(
    tibble(
      County = names(population_2023),
      population = as.numeric(population_2023)
    ),
    by = "County"
  ) %>%
  # Calculate the offence rate per 10,000 population
  mutate(offence_rate = (total_offences / population) * 10000)

# Merge datasets based on common columns, such as city or county
# Here, we'll merge on city and year as an example
merged_dataset <- merge(
  broadband_dataset,
  drug_offences_rate_2020_2023,
  by = c("County", "County"),
  all.x = TRUE,
  all.y = FALSE
)


# Check the structure and summary of the merged dataset
str(merged_dataset)
summary(merged_dataset)

# Step 2: Filter and Clean the Data

# Remove rows with missing values in columns of interest
merged_dataset <- merged_dataset[!is.na(merged_dataset$`Average download speed (Mbit/s)`) & !is.na(merged_dataset$offence_rate), ]

# Ensure that Average download speed and offence_rate are numeric
merged_dataset$`Average download speed (Mbit/s)` <- as.numeric(merged_dataset$`Average download speed (Mbit/s)`)
merged_dataset$offence_rate <- as.numeric(merged_dataset$offence_rate)

# Step 3: Fit the Linear Model

# Fit a linear model
model <- lm(offence_rate ~ `Average download speed (Mbit/s)`, data = merged_dataset)

# Check the model summary
summary(model)

# Step 4: Visualize with Line of Best Fit

# Load ggplot2 for visualization if not already loaded
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot with the line of best fit
ggplot(merged_dataset, aes(x = `Average download speed (Mbit/s)`, y = offence_rate)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of data points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Line of best fit
  labs(
    title = "Relationship between Average Download Speed and Drug Offence Rates",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offence Rate per 1000 Population"
  ) +
  theme_minimal()