library(tidyverse)
library(dplyr)

#housing cleaning

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


total_housing_data <- bind_rows(housing20, housing21, housing22, housing23)


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
    rename(Year = `Date of Transfer`)%>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL'))%>%
  distinct()


write_csv(cleaned_data, "/Users/acer/Desktop/assignment/Cleaned/cleaned_housing_data.csv")

#Broadband cleaning

broadband <- read_csv("/Users/acer/Desktop/assignment/Obtain/broadbandspeed/201805_fixed_pc_performance_r03.csv") %>%
  select(`postcode_space`, 
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
           !is.na(`Maximum download speed (Mbit/s)`)) %>%
  left_join(cleaned_data %>%
              select(Postcode, `Town/City`, County),
            by = c("postcode_space" = "Postcode")) %>%
  filter(!is.na(`Town/City`) & !is.na(County)) %>%
  distinct()

write_csv(broadband, "/Users/acer/Desktop/assignment/Cleaned/cleaned_broadband_data.csv")


#Crime cleaning

#2021
cornwall1=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-07/2021-07-devon-and-cornwall-street.csv")
cornwall2=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-08/2021-08-devon-and-cornwall-street.csv")
cornwall3=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-09/2021-09-devon-and-cornwall-street.csv")
cornwall4=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-10/2021-10-devon-and-cornwall-street.csv")
cornwall5=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-11/2021-11-devon-and-cornwall-street.csv")
cornwall6=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-12/2021-12-devon-and-cornwall-street.csv")
bristol1=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-07/2021-07-avon-and-somerset-street.csv")
bristol2=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-08/2021-08-avon-and-somerset-street.csv")
bristol3=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-09/2021-09-avon-and-somerset-street.csv")
bristol4=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-10/2021-10-avon-and-somerset-street.csv")
bristol5=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-11/2021-11-avon-and-somerset-street.csv")
bristol6=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2021-12/2021-12-avon-and-somerset-street.csv")

#2022
cornwall7=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-01/2022-01-devon-and-cornwall-street.csv")
cornwall8=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-02/2022-02-devon-and-cornwall-street.csv")
cornwall9=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-03/2022-03-devon-and-cornwall-street.csv")
cornwall10=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-04/2022-04-devon-and-cornwall-street.csv")
cornwall11=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-05/2022-05-devon-and-cornwall-street.csv")
cornwall12=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-06/2022-06-devon-and-cornwall-street.csv")
cornwall13=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-07/2022-07-devon-and-cornwall-street.csv")
cornwall14=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-08/2022-08-devon-and-cornwall-street.csv")
cornwall15=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-09/2022-09-devon-and-cornwall-street.csv")
cornwall16=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-10/2022-10-devon-and-cornwall-street.csv")
cornwall17=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-11/2022-11-devon-and-cornwall-street.csv")
cornwall18=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-12/2022-12-devon-and-cornwall-street.csv")
bristol7=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-01/2022-01-avon-and-somerset-street.csv")
bristol8=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-02/2022-02-avon-and-somerset-street.csv")
bristol9=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-03/2022-03-avon-and-somerset-street.csv")
bristol10=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-04/2022-04-avon-and-somerset-street.csv")
bristol11=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-05/2022-05-avon-and-somerset-street.csv")
bristol12=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-06/2022-06-avon-and-somerset-street.csv")
bristol13=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-07/2022-07-avon-and-somerset-street.csv")
bristol14=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-08/2022-08-avon-and-somerset-street.csv")
bristol15=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-09/2022-09-avon-and-somerset-street.csv")
bristol16=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-10/2022-10-avon-and-somerset-street.csv")
bristol17=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-11/2022-11-avon-and-somerset-street.csv")
bristol18=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2022-12/2022-12-avon-and-somerset-street.csv")

#2023
cornwall19=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-01/2023-01-devon-and-cornwall-street.csv")
cornwall20=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-02/2023-02-devon-and-cornwall-street.csv")
cornwall21=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-03/2023-03-devon-and-cornwall-street.csv")
cornwall22=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-04/2023-04-devon-and-cornwall-street.csv")
cornwall23=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-05/2023-05-devon-and-cornwall-street.csv")
cornwall24=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-06/2023-06-devon-and-cornwall-street.csv")
cornwall25=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-07/2023-07-devon-and-cornwall-street.csv")
cornwall26=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-08/2023-08-devon-and-cornwall-street.csv")
cornwall27=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-09/2023-09-devon-and-cornwall-street.csv")
cornwall28=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-10/2023-10-devon-and-cornwall-street.csv")
cornwall29=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-11/2023-11-devon-and-cornwall-street.csv")
cornwall30=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-12/2023-12-devon-and-cornwall-street.csv")
bristol19=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-01/2023-01-avon-and-somerset-street.csv")
bristol20=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-02/2023-02-avon-and-somerset-street.csv")
bristol21=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-03/2023-03-avon-and-somerset-street.csv")
bristol22=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-04/2023-04-avon-and-somerset-street.csv")
bristol23=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-05/2023-05-avon-and-somerset-street.csv")
bristol24=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-06/2023-06-avon-and-somerset-street.csv")
bristol25=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-07/2023-07-avon-and-somerset-street.csv")
bristol26=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-08/2023-08-avon-and-somerset-street.csv")
bristol27=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-09/2023-09-avon-and-somerset-street.csv")
bristol28=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-10/2023-10-avon-and-somerset-street.csv")
bristol29=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-11/2023-11-avon-and-somerset-street.csv")
bristol30=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2023-12/2023-12-avon-and-somerset-street.csv")

#2024
cornwall31=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-01/2024-01-devon-and-cornwall-street.csv")
cornwall32=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-02/2024-02-devon-and-cornwall-street.csv")
cornwall33=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-03/2024-03-devon-and-cornwall-street.csv")
cornwall34=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-04/2024-04-devon-and-cornwall-street.csv")
cornwall35=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-05/2024-05-devon-and-cornwall-street.csv")
cornwall36=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-06/2024-06-devon-and-cornwall-street.csv")
bristol31=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-01/2024-01-avon-and-somerset-street.csv")
bristol32=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-02/2024-02-avon-and-somerset-street.csv")
bristol33=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-03/2024-03-avon-and-somerset-street.csv")
bristol34=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-04/2024-04-avon-and-somerset-street.csv")
bristol35=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-05/2024-05-avon-and-somerset-street.csv")
bristol36=read_csv("/Users/acer/Desktop/assignment/Obtain/Crime/2024-06/2024-06-avon-and-somerset-street.csv")

#Combining the Datasets
crime_combined=rbind(
  bristol1,
  bristol2,
  bristol3,
  bristol4,
  bristol5,
  bristol6,
  bristol7,
  bristol8,
  bristol9,
  bristol10,
  bristol11,
  bristol12,
  bristol13,
  bristol14,
  bristol15,
  bristol16,
  bristol17,
  bristol18,
  bristol19,
  bristol20,
  bristol21,
  bristol22,
  bristol23,
  bristol24,
  bristol25,
  bristol26,
  bristol27,
  bristol28,
  bristol29,
  bristol30,
  bristol31,
  bristol32,
  bristol33,
  bristol34,
  bristol35,
  bristol36,
  
  cornwall1,
  cornwall2,
  cornwall3,
  cornwall4,
  cornwall5,
  cornwall6,
  cornwall7,
  cornwall8,
  cornwall9,
  cornwall10,
  cornwall11,
  cornwall12,
  cornwall13,
  cornwall14,
  cornwall15,
  cornwall16,
  cornwall17,
  cornwall18,
  cornwall19,
  cornwall20,
  cornwall21,
  cornwall22,
  cornwall23,
  cornwall24,
  cornwall25,
  cornwall26,
  cornwall27,
  cornwall28,
  cornwall29,
  cornwall30,
  cornwall31,
  cornwall32,
  cornwall33,
  cornwall34,
  cornwall35,
  cornwall36
)



postcode_to_lsoa = read_csv("/Users/acer/Desktop/assignment/Obtain/Postcode to LSOA.csv")
colnames(cleanlsoa) = c('LSOA code', 'street', 'counties', "postcode")
population = read_csv("/Users/acer/Desktop/assignment/Obtain/Population.csv")
colnames(population) = c("postcode", "count")


cleancrime = crime_combined %>%
  select(Month, `LSOA code`, `Crime type`, `Falls within`)
cleanlsoa = postcode_to_lsoa %>%
  select(`lsoa11cd`, `lsoa11nm`, `ladnm`, `pcds`)

#dim(cleanlsoa)
#dim(cleancrime)

county_lsoa = cleanlsoa %>%
  filter(counties %in% c("Bristol, City of", "Cornwall")) %>%
  mutate(postcode = str_trim((substring(postcode, 1, 6))))

final_cleaned_crime = cleancrime %>%
  left_join(county_lsoa, by = "LSOA code", relationship = "many-to-many") %>%
  mutate(Year  = str_trim(substring(Month,  1, 4))) %>%
  mutate(Month  = str_trim(substring(Month,  6, 7))) %>%
  left_join(population, by = "postcode") %>%
  distinct()%>%
  na.omit()

#dim(final_cleaned_crime)
#view(final_cleaned_crime)


write.csv(final_cleaned_crime, "/Users/acer/Desktop/assignment/Cleaned/crime_cleaned.csv")

#School Cleaning

bristol_school21 = read_csv("/Users/acer/Desktop/assignment/Obtain/school info/Bristol/2021-2022/801_ks4final.csv")
bristol_school22 = read_csv("/Users/acer/Desktop/assignment/Obtain/school info/Bristol/2022-2023/801_ks4final.csv")
cornwall_school21 = read_csv("/Users/acer/Desktop/assignment/Obtain/school info/Cornwall/2021-2022/908_ks4final.csv")
cornwall_school22 = read_csv("/Users/acer/Desktop/assignment/Obtain/school info/Cornwall/2022-2023/908_ks4final.csv")

# Process Bristol schools
bristol_school21 = bristol_school21 %>%
  select(SCHNAME, PCODE, ATT8SCR, TOWN, ADDRESS2) %>%
  mutate(YEAR = 2021, COUNTY = "Bristol")

bristol_school22 = bristol_school22 %>%
  select(SCHNAME, PCODE, ATT8SCR, TOWN, ADDRESS2) %>%
  mutate(YEAR = 2022, COUNTY = "Bristol")

combined_bristol_school = rbind(bristol_school21, bristol_school22)
cleaned_bristol_school = combined_bristol_school %>%
  filter(!is.na(SCHNAME) & !is.na(PCODE) & !is.na(ATT8SCR) & !is.na(TOWN) & !is.na(ADDRESS2)) %>%
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP")

# Process Cornwall schools
cornwall_school21 = cornwall_school21 %>%
  select(SCHNAME, PCODE, ATT8SCR, TOWN, ADDRESS2) %>%
  mutate(YEAR = 2021, COUNTY = "Cornwall")

cornwall_school22 = cornwall_school22 %>%
  select(SCHNAME, PCODE, ATT8SCR, TOWN, ADDRESS2) %>%
  mutate(YEAR = 2022, COUNTY = "Cornwall")

combined_cornwall_school = rbind(cornwall_school21, cornwall_school22)
cleaned_cornwall_school = combined_cornwall_school %>%
  filter(!is.na(SCHNAME) & !is.na(PCODE) & !is.na(ATT8SCR) & !is.na(TOWN) & !is.na(ADDRESS2)) %>%
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP") %>%
  distinct()


#view(cleaned_bristol_school)
#view(cleaned_cornwall_school)

cleaned_bristol_cornwall = rbind(cleaned_bristol_school, combined_cornwall_school)%>%
  filter(!is.na(SCHNAME) & !is.na(PCODE) & !is.na(ATT8SCR) & !is.na(TOWN)) %>%
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP") %>%
  distinct()
View(cleaned_bristol_cornwall)
#dim(cleaned_bristol_cornwall)
write_csv(cleaned_bristol_cornwall, "/Users/acer/Desktop/assignment/Cleaned/cleaned_school.csv")