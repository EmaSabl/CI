library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
## Load CI datastore


all <- read_csv("https://public-care-inspectorate-bucket.s3.eu-north-1.amazonaws.com/CI-latest/MDSF_latest.csv", locale = locale(encoding = "Windows-1252"))
all$ServiceName <- gsub('<92>',  '', all$ServiceName) 

## clear not needed columns
columns_keep <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                  "Service_town", "Service_Postcode", "ServiceProvider", 
                  "Provided_by_Local_Authority", "Date_Reg", "CareHome_Main_Area_of_Care", 
                  "Council_Area_Name", "NumberStaff", "Registered_Places", "Client_group",
                  "GradeSpread", "MinGrade", "MaxGrade", "MinGrade_change", "Publication_of_Latest_Grading",
                  "KQ_Support_Wellbeing", "KQ_Care_and_Support_Planning", "KQ_Setting",
                  "KQ_Staff_Team", "KQ_Leadership", "KQ_Care_Play_and_Learning",
                  "Complaints_upheld_2122", "Complaints_upheld_2223", "Complaints_upheld_2324",
                  "Enforcements_issued_2122", "Enforcements_issued_2223", "Enforcements_issued_2324", 
                  "any_requirements_2122", "any_requirements_2223", "any_requirements_2324",
                  "Last_inspection_Date")

all <- all %>% 
  select(all_of(columns_keep)) 

#Fix the formatting - sentence case for towns and remove fully empty entries
all$Service_town <- str_to_title(all$Service_town)

all <- all %>% 
  filter(!is.na(CareService) & !is.na(ServiceName) & !is.na(Date_Reg))

#check changes

x <- read.csv("https://github.com/EmaSabl/CI/blob/e119c6da7a0e9a04cec79ca1f7b370c27bc2d92d/data/CIfull.csv") #previous data
y <- all # new data

columns_equal <- setequal(names(x), names(y))

columns_added <- setdiff(names(y), names(x))
columns_dropped <- setdiff(names(x), names(y))

added_empty <- identical(columns_added, character(0))
dropped_empty <- identical(columns_dropped, character(0))


if (dropped_empty == TRUE & added_empty == FALSE) {
  message= paste("Column(s) added: ", list(columns_added))
} else if (dropped_empty == FALSE & added_empty == TRUE) {
  message= paste("Column(s) removed: ", list(columns_dropped))
} else if (dropped_empty == FALSE & added_empty == FALSE) {
  message= paste("Column(s) removed: ", list(columns_dropped),
                 "Column(s) added: ", list(columns_added))
} else {message <- NULL}

column_compare <-
  if(columns_equal == FALSE) {
    message(paste("Warning: Column names changed, care inspectorate data affected.", message))
  }else if(columns_equal == TRUE) {
    print('Care inspectorate column names match')
  }

all_prev <- x
## Cancelled and new services ####

## Each month publishes files that relate to all registered care services currently operating in Scotland 
## Services cancelled before date on file will be removed and new ones added 

# Find the dropped services (cancelled services)
cancelled_services <- anti_join(all_prev, all, by = "ServiceName") %>% 
  filter(ServiceName != "")

cancelled_LA <- cancelled_services %>% 
  group_by(CareService, Council_Area_Name) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  pivot_wider(names_from = CareService, 
              values_from = n, 
              values_fill = 0) %>%
  mutate('Month total' = rowSums(select(., -(Council_Area_Name))))

## Scotland cancelled services by type of service provided
cancelled_scot <- cancelled_services %>%
  group_by(CareService) %>% 
  summarise(n = n())
  
service_options <- tibble(CareService = unique(all$CareService))

cancelled_scot <- full_join(cancelled_scot, 
          service_options, 
          by = ) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

## save it to a column named based on the preceding month

current_date <- Sys.Date()
month_update <- month(current_date) - 1
year_update <- year(current_date)

if (month_update == 0) {
  month_update <- 12
  year_update <- year_update - 1
}

cancelled_scot <- cancelled_scot %>% 
  rename(
    !!paste(month.abb[month_update], year_update) := n
  )
# Add to spreadsheet stored in github
cancelled_scot_month <- read_csv("https://github.com/EmaSabl/CI/blob/31f636f7a0d97e072a15782fa6850097890385f5/data/cancelled_scot.csv") ## previous data

cancelled_scot <- right_join (cancelled_scot_month,
            cancelled_scot)
# export cancelled data sets
write.csv(cancelled_scot, "data/cancelled_scot.csv", row.names = FALSE)
write.csv(cancelled_LA, "data/cancelled_LA.csv", row.names = FALSE)
