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
  filter(!is.na(Price) & 
           !is.na(`Date of Transfer`) & 
           !is.na(Postcode) & 
           !is.na(`Town/City`) & 
           !is.na(County) & 
           !is.na(ID))

View(cleaned_data)

filtered_data <- cleaned_data %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL'))

#View(filtered_data)

output_path <- "/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv"

write_csv(filtered_data, output_path)#house prices cleaned output csv file


  