library(ggplot2)
library(dplyr)
library(fmsb)
library(tidyr)

# Schooling
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")
#View(schooling_data)

#Average Attainment 8 score in the year 2021-2022 academic year for Bristol and Cornwall
schooling_data = schooling_data %>%
  filter(YEAR == 2022)

ggplot(schooling_data, aes(x = COUNTY, y = ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Scores for 2021-2022 Bristol and Cornwall",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

#Bristol average attainment 8 score in academic year 2021-2022
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")

schooling_data_bristol = schooling_data %>%
  filter(YEAR == 2022) %>%
  filter(COUNTY == "Bristol")

average_attainment_by_address = schooling_data_bristol %>%
  group_by(ADDRESS2) %>%
  summarise(Average_ATT8SCR = mean(as.numeric(ATT8SCR))) %>%
  arrange(desc(Average_ATT8SCR))

ggplot(average_attainment_by_address, aes(x = factor(ADDRESS2, levels = ADDRESS2), y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Average Attainment 8 Score by district in Bristol (2021-2022)",
       x = "District",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Cornwall average attainment 8 score in academic year 2021-2022
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")

schooling_data_cornwall = schooling_data %>%
  filter(YEAR == 2022) %>%
  filter(COUNTY == "Cornwall")

average_attainment_by_address = schooling_data_bristol %>%
  group_by(ADDRESS2) %>%
  summarise(Average_ATT8SCR = mean(as.numeric(ATT8SCR))) %>%
  arrange(desc(Average_ATT8SCR))

ggplot(average_attainment_by_address, aes(x = factor(ADDRESS2, levels = ADDRESS2), y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Average Attainment 8 Score by district in Cornwall (2021-2022)",
       x = "District",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# House Prices
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
#View(housing_data)

housing_2023 <- housing_data %>% 
  filter(Year == 2023)

# Boxplot of Average House Price in 2022 for both counties
ggplot(data = housing_2023, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot for	Average House Price in Year 2023", x = "County", y = "Price") +
  theme_minimal()

# Bar Chart of Average House Price in 2022 for both counties
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
#View(housing_data)

housing_2023 <- housing_data %>% 
  filter(Year == 2023)

average_price_2023 <- housing_2023 %>% 
  group_by(County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_2023, aes(x = County, y = Average_Price)) +
  geom_bar(stat = "identity",fill = c("red", "blue")) +
  ggtitle("Bar Chart of Average House Price in Year 2023") +
  xlab("County") +
  ylab("Average Price")

# Line Chart of Average House Price from 2020 to 2023 for both counties
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
#View(housing_data)

housing_2020_2023 <- housing_data %>% 
  filter(Year >= 2020 & Year <= 2023)
average_price_per_year <- housing_2020_2023 %>% 
  group_by(Year, County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_per_year, aes(x = Year, y = Average_Price, color = County)) +
  geom_line() +
  geom_point() +
  ggtitle("Line Chart of Average House Price from 2020 to 2023") +
  xlab("Year") +
  ylab("Average Price")


#Broadband 
broadband_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv")
#View(broadband_data)

# Boxplot of Average Download Speeds in Both Counties
ggplot(broadband_data, aes(x = County, y = `Average download speed (Mbit/s)`,fill=County)) +
  geom_boxplot() +
  ggtitle("Boxplot of Average Download Speeds") +
  xlab("County") +
  ylab("Average Download Speed (Mbps)")

bristol_data <- broadband_data %>% 
  filter(County == "CITY OF BRISTOL")


bristol_filtered <- bristol_data %>% 
  summarise(Average_Speed = mean(`Average download speed (Mbit/s)`),
            Maximum_Speed = max(`Maximum download speed (Mbit/s)`))

bristol_df <- data.frame(
  Type = c("Average Download Speed", "Maximum Download Speed"),
  Speed = c(bristol_filtered$Average_Speed, bristol_filtered$Maximum_Speed)
)

# Bar Chart for Bristol
ggplot(bristol_df, aes(x = Type, y = Speed)) +
  geom_bar(stat = "identity", fill = c("blue", "red")) +
  ggtitle("Average and Maximum Download Speeds in Bristol") +
  xlab("") +
  ylab("Speed (Mbps)")


cornwall_filtered <- broadband_data %>% 
  filter(County == "CORNWALL") %>%
  summarise(Average_Speed = mean(`Average download speed (Mbit/s)`),
            Maximum_Speed = max(`Maximum download speed (Mbit/s)`))

cornwall_df <- data.frame(
  Type = c("Average Download Speed", "Maximum Download Speed"),
  Speed = c(cornwall_filtered$Average_Speed, cornwall_filtered$Maximum_Speed)
)

# Bar Chart for Cornwall
ggplot(cornwall_df, aes(x = Type, y = Speed)) +
  geom_bar(stat = "identity", fill = c("blue", "red")) +
  ggtitle("Average and Maximum Download Speeds in Cornwall") +
  xlab("") +
  ylab("Speed (Mbps)")





#Crime
crime=read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")


drug_offences = crime %>%
  filter(`Crime type` == "Drugs")%>%
  filter(`Year` == "2023")

bristol_drug_offences <- drug_offences %>%
  filter(counties == "Bristol, City of") %>%
  summarise(total_population = sum(count),
            total_drug_offences = n())%>%
  mutate(offence_rate = (total_drug_offences / total_population) * 10000)
#View(bristol_drug_offences)

cornwall_drug_offences <- drug_offences %>%
  filter(counties == "Cornwall") %>%
  summarise(total_population = sum(count),
            total_drug_offences = n())%>%
  mutate(offence_rate = (total_drug_offences / total_population) * 10000)
#View(cornwall_drug_offences)

combined_data <- bind_rows(
  cornwall_drug_offences %>% 
    mutate(county = "Cornwall"),
  bristol_drug_offences %>% 
    mutate(county = "Bristol, City of"))
#View(combined_data)

#Draw the graph
ggplot(combined_data, aes(x = county, y = offence_rate, fill = county)) +
  geom_boxplot() +
  labs(title = "Distribution of Drug Offence Rates (2023)",
       x = "Location",
       y = "Offence Rate (per 10,000)") +
  theme_minimal()


#Radar chart of Vehicle Crime Rate From 2021 to 2024
vehicle_crime = crime %>%
  filter(`Crime type` == "Vehicle crime")%>%
  group_by(Year) %>%
  summarise(total_crime = sum(count, na.rm = TRUE))

vehicle_crime_df <- as.data.frame(t(vehicle_crime$total_crime))
colnames(vehicle_crime_df) <- vehicle_crime$Year

vehicle_crime_df <- rbind(rep(max(vehicle_crime$total_crime), length(years)), 
                   rep(0, length(years)), 
                   vehicle_crime_df)

par(mar = c(2, 2, 2, 2))

radarchart(vehicle_crime_df,
           pcol = "red",
           pfcol = "pink",
           cglcol = "grey",
           axislabcol = "darkgrey",
           caxislabels = seq(0, max(vehicle_crime$total_crime)),
           title = "Vehicle Crime Rate from 2021 to 2024")

# robbery  pie chart
crime_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")

rober_november <- crime_data %>%
  filter(`Crime type` == "Robbery" & Year == "2023" & Month == "10") %>%
  group_by(counties) %>%
  summarise(total_robberies = n()) %>%
  mutate(percentage = total_robberies / sum(total_robberies) * 100)

ggplot(rober_november, aes(x = "", y = percentage, fill = counties)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Robberies by County (October 2023)", fill = "County") +
  theme_minimal()


#drug offense rate line chart
crime_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")

county_population <- crime_data %>%
  select(postcode, count, counties, Year) %>%
  distinct() %>%
  group_by(counties, Year) %>%
  summarize(total_population = sum(count))

drug_offenses <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(counties, Year) %>%
  summarize(total_drug_offenses = n()) %>%
  na.omit()

drug_rate_per_10k <- left_join(drug_offenses, county_population, by = c("counties", "Year")) %>%
  mutate(rate_per_10k = (total_drug_offenses / total_population) * 10000)

ggplot(drug_rate_per_10k, aes(x = Year, y = rate_per_10k, color = counties, group = counties)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Drug Offense Rate per 10,000 People",
       x = "Year",
       y = "Drug Offense Rate per 10,000 People",
       color = "Counties") +
  theme_minimal()