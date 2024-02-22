library(readr)
library(dplyr)
Census <- read_csv("D:/文件下载/CaliforniaCensusDataByZip.csv")
Survey <- read_csv("D:/文件下载/VaccinationSurveyData.csv")
ZipCode <- read_csv("D:/文件下载/CaliforniaZipCodeCounties.csv")

target_region <- c("Glenn","Butte","Colusa","Yolo","Solano","Sacramento",
"El Dorado","Sutter","Yuba","Placer","Nevada","Sierra")

Zipcodes <- ZipCode %>%
  filter(county %in% target_region) %>%
  select(zip) %>%
  pull() 

Zipcodes <- unique(Zipcodes)


Census_select <- Census %>% filter(`Geographic Area Name` %in% Zipcodes)


Age_20_24 <- Survey %>% filter(Age >= 20 & Age <= 24)
Age_25_34 <- Survey %>% filter(Age >= 25 & Age <= 34)
Age_35_44 <- Survey %>% filter(Age >= 35 & Age <= 44)
Age_45_54 <- Survey %>% filter(Age >= 45 & Age <= 54)
Age_55_59 <- Survey %>% filter(Age >= 55 & Age <= 59)
Age_60_64 <- Survey %>% filter(Age >= 60 & Age <= 64)
Age_65_74 <- Survey %>% filter(Age >= 65 & Age <= 74)
Age_over75 <- Survey %>% filter(Age >= 75)


pct_20_24 <- nrow(Age_20_24 %>% filter(.[[13]] >= 4))/nrow(Age_20_24)
pct_25_34 <- nrow(Age_25_34 %>% filter(.[[13]] >= 4))/nrow(Age_25_34)
pct_35_44 <- nrow(Age_35_44 %>% filter(.[[13]] >= 4))/nrow(Age_35_44)
pct_45_54 <- nrow(Age_45_54 %>% filter(.[[13]] >= 4))/nrow(Age_45_54)
pct_55_59 <- nrow(Age_55_59 %>% filter(.[[13]] >= 4))/nrow(Age_55_59)
pct_60_64 <- nrow(Age_60_64 %>% filter(.[[13]] >= 4))/nrow(Age_60_64)
pct_65_74 <- nrow(Age_65_74 %>% filter(.[[13]] >= 4))/nrow(Age_65_74)
pct_over75 <- nrow(Age_over75 %>% filter(.[[13]] >= 4))/nrow(Age_over75)

area95811 <- Census %>% filter(`Geographic Area Name` == 95811)

totalvacc <- function(Survey,df){
  Age_20_24 <- Survey %>% filter(Age >= 20 & Age <= 24)
  Age_25_34 <- Survey %>% filter(Age >= 25 & Age <= 34)
  Age_35_44 <- Survey %>% filter(Age >= 35 & Age <= 44)
  Age_45_54 <- Survey %>% filter(Age >= 45 & Age <= 54)
  Age_55_59 <- Survey %>% filter(Age >= 55 & Age <= 59)
  Age_60_64 <- Survey %>% filter(Age >= 60 & Age <= 64)
  Age_65_74 <- Survey %>% filter(Age >= 65 & Age <= 74)
  Age_over75 <- Survey %>% filter(Age >= 75)
  
  pct_20_24 <- nrow(Age_20_24 %>% filter(.[[13]] >= 4))/nrow(Age_20_24)
  pct_25_34 <- nrow(Age_25_34 %>% filter(.[[13]] >= 4))/nrow(Age_25_34)
  pct_35_44 <- nrow(Age_35_44 %>% filter(.[[13]] >= 4))/nrow(Age_35_44)
  pct_45_54 <- nrow(Age_45_54 %>% filter(.[[13]] >= 4))/nrow(Age_45_54)
  pct_55_59 <- nrow(Age_55_59 %>% filter(.[[13]] >= 4))/nrow(Age_55_59)
  pct_60_64 <- nrow(Age_60_64 %>% filter(.[[13]] >= 4))/nrow(Age_60_64)
  pct_65_74 <- nrow(Age_65_74 %>% filter(.[[13]] >= 4))/nrow(Age_65_74)
  pct_over75 <- nrow(Age_over75 %>% filter(.[[13]] >= 4))/nrow(Age_over75)
  
  df$vacc_sum <-  df$`SEX AND AGE!!Total population!!20 to 24 years` * pct_20_24 
  + df$`SEX AND AGE!!Total population!!25 to 34 years` * pct_25_34 
  + df$`SEX AND AGE!!Total population!!35 to 44 years` * pct_35_44
  + df$`SEX AND AGE!!Total population!!45 to 54 years` * pct_45_54
  + df$`SEX AND AGE!!Total population!!55 to 59 years` * pct_55_59
  + df$`SEX AND AGE!!Total population!!60 to 64 years` * pct_60_64
  + df$`SEX AND AGE!!Total population!!65 to 74 years` * pct_65_74
  + (df$`SEX AND AGE!!Total population!!75 to 84 years`+ df$`SEX AND AGE!!Total population!!85 years and over`) * pct_over75
  
  return(sum(df$vacc_sum))
  
}

# total vacc for all region(with 20% buffer)
num <- totalvacc(Survey,Census_select)
num_withbuffer <- num*1.2
round(num_withbuffer)

# total vacc for area95811(with 20% buffer)
num_95811 <- totalvacc(Survey,area95811)
num_95811_withbuffer <- num_95811*1.2
round(num_95811_withbuffer)

codes <- ZipCode %>%
  filter(city == "Sacramento") %>%
  select(zip) %>%
  pull() 

codes <- unique(codes)

Census_Sacramento <- Census %>% filter(`Geographic Area Name` %in% codes)
num <- totalvacc(Survey,Census_Sacramento)
num_withbuffer <- num*1.2
round(num_withbuffer)

