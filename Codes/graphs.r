library(ggplot2)
library(dplyr)

# House Prices

housing_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")

housing_2022 <- housing_data %>% 
  filter(Year == 2022)

# Boxplot of Average House Price in 2022 for both counties
ggplot(data = housing_2022, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot for	Average House Price in Year 2022", x = "County", y = "Price") +
  theme_minimal()

# Bar Chart of Average House Price in 2022 for both counties
average_price_2022 <- housing_2022 %>% 
  group_by(County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_2022, aes(x = County, y = Average_Price)) +
  geom_bar(stat = "identity",fill = c("red", "blue")) +
  ggtitle("Bar Chart of Average House Price in Year 2022") +
  xlab("County") +
  ylab("Average Price")

# Line Chart of Average House Price from 2020 to 2023 for both counties
housing_2020_2023 <- housing_data %>% 
  filter(Year >= 2020 & Year <= 2023)
average_price_per_year <- housing_2020_2023 %>% 
  group_by(Year, County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_per_year, aes(x = Year, y = Average_Price, color = County)) +
  geom_line(size = 1) +
  geom_point() +
  ggtitle("Line Chart of Average House Price from 2020 to 2023") +
  xlab("Year") +
  ylab("Average Price")


#Broadband 

broadband_data <- read_csv("/Users/acer/Desktop/assignment/Cleaned/merged_broadband_housing_data.csv")

View(broadband_data)

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