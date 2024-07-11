library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate) 
## clear not needed columns

# Add link directly from CI datastore
update <- read_csv("https://www.careinspectorate.com/images/documents/7669/MDSF_data_30%20June%202024.csv", locale = locale(encoding = "Windows-1252"))
update$ServiceName <- gsub('[^\x20-\x7E]',  '', update$ServiceName) 
columns_keep <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                  "Service_town", "Service_Postcode", "ServiceProvider", 
                  "Provided_by_Local_Authority", "Date_Reg", "CareHome_Main_Area_of_Care", 
                  "Council_Area_Name", "NumberStaff", "Registered_Places", "Client_group",
                  "GradeSpread", "MinGrade", "MaxGrade", "MinGrade_change", "Publication_of_Latest_Grading",
                  "KQ_Support_Wellbeing", "KQ_Care_and_Support_Planning", "KQ_Setting",
                  "KQ_Staff_Team", "KQ_Leadership", "KQ_Care_Play_and_Learning",
                  "Complaints_upheld_2223", "Complaints_upheld_2324", "Complaints_upheld_2425",
                  "Enforcements_issued_2223", "Enforcements_issued_2324", "Enforcements_issued_2425",
                  "any_requirements_2223", "any_requirements_2324", "any_requirements_2425",
                  "Last_inspection_Date", "ServiceStatus")

update <- update %>% 
  select(all_of(columns_keep)) 

#Fix the formatting - sentence case for towns and remove fully empty entries
update$Service_town <- str_to_title(update$Service_town)

update <- update %>% 
  filter(!is.na(CareService) & !is.na(ServiceName) & !is.na(Date_Reg))
write.csv(update, "data/MDSF_latest.csv", row.names = FALSE)
