#Load libraries
library(readr)
library(dplyr)
library(rpivotTable)
library(tidyr)

#Read in raw data
data <- read_csv('./raw_data.csv')

#Take a look at the data via rpivotTablev visualization (optional)
rpivotTable(data)

#Compute route frequencies
route_frequency <- data %>% group_by(route_taken,user_type) %>% summarize(length(route_taken)) %>% 
  `colnames<-`(c('Route','User Type','Freq')) %>% 
  arrange(Route,`User Type`,desc(Freq)) %>%
  spread(`User Type`,`Freq`) 

#Replace any missing entries with 0 for frequency
route_frequency[is.na(route_frequency)] <- 0

#We compute the ratio of casual/member
#Any routes with NO casual/member will be excluded 
#We filter down only for the routes that are between 90%-100% ratio 
#We sort descending
most_common_1_to_1 <- route_frequency %>% mutate(ratio_casual_to_member = round(casual/member,digits=2)) %>% 
  filter(casual != 0 ) %>%
  filter(member != 0) %>%
  filter(ratio_casual_to_member != 0) %>%
  filter(abs(ratio_casual_to_member) > .90) %>% 
  filter(abs(ratio_casual_to_member) < 1.10) %>%
  select(-ratio_casual_to_member) %>%
  arrange(desc(casual),desc(member))

#You can visualize this in rpivotTable(optional)
rpivotTable(route_frequency)

