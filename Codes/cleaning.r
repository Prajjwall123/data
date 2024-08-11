library(tidyverse)


column_names=c(
  "ID",
  "Price",
  "Date of Transfer",
  "Postcode",
  "Property Type",
  "Old/New",
  "Duration",
  "PAON",
  "SAON",
  "Street",
  "Locality",
  "Town/City",
  "District",
  "County",
  "PPD Category Type"
)


housing20=read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2020.csv",col_names = column_names)
housing21=read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2021.csv",col_names = column_names)
housing22=read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2022.csv",col_names = column_names)
housing23=read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2023.csv",col_names = column_names)

#View(housing20)

total_housing_data <- bind_rows(housing20, housing21, housing22, housing23)

#View(total_housing_data)

cleaned_data <- total_housing_data %>%
  select(-`PPD Category Type`, 
         -`Property Type`, 
         -`Old/New`, 
         -`Duration`, 
         -PAON, 
         -SAON, 
         -Street, 
         -Locality, 
         -District) %>%
  filter(!is.na(Price) & !is.na(`Date of Transfer`) & !is.na(Postcode) & !is.na(`Town/City`) & !is.na(County) & !is.na(ID))

cleaned_data <- cleaned_data%>%
  mutate(`Date of Transfer` = substr(`Date of Transfer`, 1, 4)) %>%
    rename(Year = `Date of Transfer`)

#View(cleaned_data)

filtered_data <- cleaned_data %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL'))

#View(filtered_data)

output_path <- "/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv"

write_csv(filtered_data, output_path)#house prices cleaned output csv file

#Broadband cleaning

# Load necessary library
library(dplyr)

# Load the dataset
file_path <- "/Users/acer/Desktop/assignment/Obtain/broadbandspeed/201805_fixed_pc_performance_r03.csv"
broadband <- read_csv(file_path)

View(broadband)

# Select and filter the dataset
filtered_broadband <- broadband %>%
  select(postcode_space, 
         `Median download speed (Mbit/s)`, 
         `Median upload speed (Mbit/s)`, 
         `Average upload speed (Mbit/s)`, 
         `Average download speed (Mbit/s)`, 
         `Maximum upload speed (Mbit/s)`, 
         `Maximum download speed (Mbit/s)`) %>%
  filter(!is.na(postcode_space) & 
           !is.na(`Median download speed (Mbit/s)`) & 
           !is.na(`Median upload speed (Mbit/s)`) & 
           !is.na(`Average upload speed (Mbit/s)`) & 
           !is.na(`Average download speed (Mbit/s)`) & 
           !is.na(`Maximum upload speed (Mbit/s)`) & 
           !is.na(`Maximum download speed (Mbit/s)`))

# Display the cleaned data
View(filtered_broadband)


output_path <- "/Users/acer/Desktop/assignment/Cleaned/cleaned_broadband_data.csv"

write_csv(filtered_broadband, output_path)#broadband cleaned output csv file without the postcode job



library(dplyr)

# Load the CSV files
housing_data <- read.csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
broadband_data <- read.csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_broadband_data.csv")

# Merge the datasets based on Postcode and postcode_space
merged_data <- broadband_data %>%
  left_join(housing_data %>% select(Postcode, Town.City, County), 
            by = c("postcode_space" = "Postcode"))

# Filter out rows with NA values
filtered_data <- merged_data %>%
  filter(!is.na(Town.City) & !is.na(County))

# Save the merged data to a new CSV file
write.csv(filtered_data, "/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv", row.names = FALSE)