library(ggplot2)
library(tidyverse)

housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
housing_data <- housing_data %>%
  select(Postcode, Price)

download_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv')
download_data <- download_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  rename(Postcode = postcode_space)

combined_data <- merge(housing_data, download_data, by = "Postcode")

ggplot(data = combined_data, aes(x = Price, y = `Average download speed (Mbit/s)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "House Prices vs Average Download Speed", 
       x = "House Price", 
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()


#Attainment 8 Score VS House Price

school_data<-read_csv('/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv')
#View(school_data)

school_data=school_data%>%
  filter(YEAR==2022)%>%
  select(PCODE,ATT8SCR)%>%
  rename(Postcode="PCODE")

housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
housing_data <- housing_data %>%
  select(Postcode, Price)

combined_data <- merge(school_data, housing_data, by = "Postcode")
#View(combined_data)

ggplot(data = combined_data, aes(x = ATT8SCR, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average attainment 8 score vs House Price", 
       x = "ATT8SCR", 
       y = "Price") +
  theme_minimal()


#Average Download Speed VS Attainment 8 Score

download_data <- read_csv('/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv')
download_data <- download_data %>%
  select(postcode_space, `Average download speed (Mbit/s)`)%>%
  rename(Postcode = postcode_space)

school_data<-read_csv('/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv')
school_data=school_data%>%
  filter(YEAR==2022)%>%
  select(PCODE,ATT8SCR)%>%
  rename(Postcode="PCODE")

combined_data <- merge(download_data, school_data, by = "Postcode")
#View(combined_data)

ggplot(data = combined_data, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average download speed VS Attainment 8 Score", 
       x = "Average download speed", 
       y = "Attainment 8 Score") +
  theme_minimal()