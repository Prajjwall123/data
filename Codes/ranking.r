library(tidyverse)

broadband <- read_csv("/Users/acer/Desktop/assignment/Obtain/broadbandspeed/201805_fixed_pc_performance_r03.csv")
housing <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
schooling <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")
crime <- read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")

filtered_broadband <- broadband %>%
  select(postcode_space, 
         `Average upload speed (Mbit/s)`, 
         `Average download speed (Mbit/s)`)

combined_data <- housing %>%
  inner_join(filtered_broadband, by = c("Postcode" = "postcode_space")) %>%
  select(`Town/City`, `Average download speed (Mbit/s)`, `Average upload speed (Mbit/s)`)

broadband_final <- combined_data %>%
  filter(
    !is.na(`Town/City`) &
      !is.na(`Average download speed (Mbit/s)`) &
      !is.na(`Average upload speed (Mbit/s)`)) %>%
  mutate(`Town/City` = str_trim(toupper(`Town/City`))) %>%
  group_by(`Town/City`) %>%
  summarise(
    avg_download_speed = mean(`Average download speed (Mbit/s)`),
    avg_upload_speed = mean(`Average upload speed (Mbit/s)`)
  )

average_att8_by_town <- schooling %>%
  group_by(TOWN) %>%
  summarise(average_att8_score = mean(ATT8SCR)) %>%
  rename("Town/City" = TOWN) %>%
  mutate(`Town/City` = str_trim(toupper(`Town/City`)))

filtered_housing <- housing %>%
  filter(Year == 2022) %>%
  group_by(`Town/City`) %>%
  summarise(average_price = mean(Price)) %>%
  filter(!is.na(`Town/City`)) %>%
  mutate(`Town/City` = str_trim(toupper(`Town/City`)))

town_data <- housing %>%
  mutate(postcode = str_trim(substring(Postcode, 1, 6))) %>%
  mutate(TOWN = str_trim(toupper(`Town/City`))) %>%
  select(postcode, TOWN) %>%
  distinct()

filtered_crime <- crime %>%
  filter(Year == 2023) %>%
  group_by(postcode) %>%
  summarise(crimeno = n()) %>%
  arrange(desc(crimeno))

final_crime <- filtered_crime %>%
  left_join(town_data, by = "postcode") %>%
  na.omit() %>%
  distinct() %>%
  group_by(TOWN) %>%
  summarise(crimerate = sum(crimeno)) %>%
  rename("Town/City" = TOWN) %>%
  mutate(`Town/City` = str_trim(toupper(`Town/City`)))

broadband_schooling <- broadband_final %>%
  inner_join(average_att8_by_town, by = "Town/City")

final_combined_data <- broadband_schooling %>%
  inner_join(filtered_housing, by = "Town/City") %>%
  inner_join(final_crime, by = "Town/City")

View(final_combined_data)