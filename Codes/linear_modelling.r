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

ggplot(data = combined_data, aes(x = Drug_Rates, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "House Prices vs Drug Rates", 
       x = "Drug Rates", 
       y = "House Price") +
  theme_minimal()