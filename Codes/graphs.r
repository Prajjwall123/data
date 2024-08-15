library(ggplot2)
library(dplyr)

# Schooling
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")
#View(schooling_data)

#Average Attainment 8 score in the year 2021-2022 academic year for Bristol and Cornwall
schooling_data = schooling_data %>%
  filter(YEAR == 2021)

ggplot(schooling_data, aes(x = COUNTY, y = ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Scores for 2021-2022 Bristol and Cornwall",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

#Bristol average attainment 8 score in academic year 2021-2022
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")
#View(schooling_data)

schooling_data_bristol = schooling_data %>%
  filter(YEAR == 2021)%>%
  filter(COUNTY=="Bristol")

ggplot(schooling_data_bristol, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Bristol Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#theme_minimal()

#Cornwall average attainment 8 score in academic year 2021-2022
schooling_data = read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")
#View(schooling_data)

schooling_data_cornwall = schooling_data %>%
  filter(YEAR == 2021)%>%
  filter(COUNTY=="Cornwall")

ggplot(schooling_data_cornwall, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Cornwall Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# House Prices
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
#View(housing_data)

housing_2022 <- housing_data %>% 
  filter(Year == 2022)

# Boxplot of Average House Price in 2022 for both counties
ggplot(data = housing_2022, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot for	Average House Price in Year 2022", x = "County", y = "Price") +
  theme_minimal()

# Bar Chart of Average House Price in 2022 for both counties
housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")
#View(housing_data)

housing_2022 <- housing_data %>% 
  filter(Year == 2022)

average_price_2022 <- housing_2022 %>% 
  group_by(County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_2022, aes(x = County, y = Average_Price)) +
  geom_bar(stat = "identity",fill = c("red", "blue")) +
  ggtitle("Bar Chart of Average House Price in Year 2022") +
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


bristol_summary <- bristol_data %>% 
  summarise(Average_Speed = mean(`Average download speed (Mbit/s)`),
            Maximum_Speed = max(`Maximum download speed (Mbit/s)`))

bristol_df <- data.frame(
  Type = c("Average Download Speed", "Maximum Download Speed"),
  Speed = c(bristol_summary$Average_Speed, bristol_summary$Maximum_Speed)
)

# Bar Chart for Bristol
ggplot(bristol_df, aes(x = Type, y = Speed)) +
  geom_bar(stat = "identity", fill = c("blue", "red")) +
  ggtitle("Average and Maximum Download Speeds in Bristol") +
  xlab("") +
  ylab("Speed (Mbps)")


cornwall_summary <- broadband_data %>% 
  filter(County == "CORNWALL") %>%
  summarise(Average_Speed = mean(`Average download speed (Mbit/s)`),
            Maximum_Speed = max(`Maximum download speed (Mbit/s)`))

cornwall_df <- data.frame(
  Type = c("Average Download Speed", "Maximum Download Speed"),
  Speed = c(cornwall_summary$Average_Speed, cornwall_summary$Maximum_Speed)
)

# Bar Chart for Cornwall
ggplot(cornwall_df, aes(x = Type, y = Speed)) +
  geom_bar(stat = "identity", fill = c("blue", "red")) +
  ggtitle("Average and Maximum Download Speeds in Cornwall") +
  xlab("") +
  ylab("Speed (Mbps)")


