# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Column names(From 2020 dataset)
colm_names = c('ID', 'Price', 'Date', 'PostCode', 'null1', 'null2', 'null3', 
               'POAN', 'SOAN', 'Street', 'Locality', 'Town', 'District', 'County', 'null4', 'null5')

# Read data for each year
housing20 <- read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2020.csv", col_names = colm_names)
housing21 <- read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2021.csv", col_names = colm_names)
housing22 <- read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2022.csv", col_names = colm_names)
housing23 <- read_csv("/Users/acer/Desktop/assignment/Obtain/house price/pp-2023.csv", col_names = colm_names)

# Merge datasets
merged_housing = bind_rows(housing20, housing21, housing22, housing23)

# Clean merged dataset
final_data <- merged_housing %>%
  select(-c('null1', 'null2', 'null3', 'null4', 'null5', 'ID', 'SOAN')) %>%
  filter(grepl("Bristol|Cornwall", County, ignore.case = TRUE)) %>%
  filter(!is.na(Price))
  #filter(!is.na(Locality))
  filter(!is.na("Street"))

# Specify the path where the cleaned data should be saved
output_path <- "/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv"

# Write the cleaned data to a CSV file
write_csv(final_data, output_path)
