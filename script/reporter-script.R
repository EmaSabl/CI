library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate) 

## clear not needed columns

# Add link directly from CI datastore
update <- read_csv("https://www.careinspectorate.com/images/documents/7648/MDSF_data_31%20May%202024.csv", locale = locale(encoding = "Windows-1252"))
update$ServiceName <- gsub('[^\x20-\x7E]',  '', update$ServiceName) 
columns_keep <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                  "Service_town", "Service_Postcode", "ServiceProvider", 
                "Date_Reg", "Council_Area_Name","GradeSpread", "MinGrade_change", "Publication_of_Latest_Grading",
                  "KQ_Support_Wellbeing", "KQ_Care_and_Support_Planning", "KQ_Setting",
                  "KQ_Staff_Team", "KQ_Leadership", "KQ_Care_Play_and_Learning",
                  "Complaints_upheld_2223", "Complaints_upheld_2324", "Complaints_upheld_2425",
                  "Enforcements_issued_2223", "Enforcements_issued_2324", "Enforcements_issued_2425",
                  "any_requirements_2223", "any_requirements_2324", "any_requirements_2425",
                  "Last_inspection_Date", "ServiceStatus", "first_reqs_2425", "second_reqs_2425")

update <- update %>% 
  select(all_of(columns_keep)) 

#Fix the formatting - sentence case for towns and remove fully empty entries
update$Service_town <- str_to_title(update$Service_town)

update <- update %>% 
  filter(!is.na(CareService) & !is.na(ServiceName) & !is.na(Date_Reg))


##Set the time 
current_date <- Sys.Date()

month_update <- month(current_date) - 1
year_update <- year(current_date)

if (month_update == 0) {
  month_update <- 12
  year_update <- year_update - 1
}

## details


df <- update %>% 
  filter("ServiceStatus" != "Inactive") 

##care homes

care_homes <- df %>%
  filter(CareService == 'Care Home Service') %>% 
  filter(Subtype == 'Older People') %>% 
  filter(!is.na(Publication_of_Latest_Grading)) %>% 
  select(-c(CareService, Subtype,GradeSpread, ServiceStatus, KQ_Care_Play_and_Learning)) %>% 
  rowwise() %>%
  mutate(`Average` = mean(c_across(starts_with("KQ")), na.rm = TRUE)) %>% 
  mutate(`Complaints_upheld_since_22_23` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate(`Enforcements_upheld_since_22_23` = sum(c_across(starts_with("Enforcements_issued_")), na.rm = TRUE))
  


dundee_care_homes <- care_homes %>% 
  filter(Service_town == "Dundee") %>% 
  filter(Council_Area_Name == "Dundee City") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(dundee_care_homes, "report/dundee_care_homes.csv")

aberdeen_care_homes <- care_homes %>% 
  filter(Service_town == "Aberdeen") %>% 
  filter(Council_Area_Name == "Aberdeen City") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(aberdeen_care_homes, "report/Aberdeen_care_homes.csv")

perth_care_homes <- care_homes %>% 
  filter(Service_town == "Perth") %>% 
  filter(Council_Area_Name == "Perth & Kinross") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(perth_care_homes, "report/perth_care_homes.csv")

dunfermline_care_homes <- care_homes %>% 
  filter(Service_town == "Dunfermline") %>% 
  filter(Council_Area_Name == "Fife") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(dunfermline_care_homes, "report/dunfermline_care_homes.csv")

stirling_care_homes <- care_homes %>% 
  filter(Service_town == "Stirling") %>% 
  filter(Council_Area_Name == "Stirling") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(stirling_care_homes, "report/stirling_care_homes.csv")



inverness_care_homes <- care_homes %>% 
  filter(Service_town == "Inverness") %>% 
  filter(Council_Area_Name == "Highland") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(inverness_care_homes, "report/inverness_care_homes.csv")

elgin_care_homes <- care_homes %>% 
  filter(Service_town == "Elgin") %>% 
  filter(Council_Area_Name == "Moray") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(elgin_care_homes, "report/elgin_care_homes.csv")


##nurseries
nursery <- df %>%
  filter(CareService == 'Day Care of Children') #%>% 
  filter(Subtype == 'Day Care of Children (under 3s)') %>% 
  filter(!is.na(Publication_of_Latest_Grading)) %>% 
  select(-c(CareService, Subtype,GradeSpread, ServiceStatus)) %>% 
  rowwise() %>%
  mutate(`Average` = mean(c_across(starts_with("KQ")), na.rm = TRUE)) %>% 
  mutate(`Complaints_upheld_since_22_23` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate(`Enforcements_upheld_since_22_23` = sum(c_across(starts_with("Enforcements_issued_")), na.rm = TRUE))



dundee_nursery <- nursery %>% 
  filter(Service_town == "Dundee") %>% 
  filter(Council_Area_Name == "Dundee City") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(dundee_nursery, "report/dundee_nursery.csv")

aberdeen_nursery <- nursery %>% 
  filter(Service_town == "Aberdeen") %>% 
  filter(Council_Area_Name == "Aberdeen City") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(aberdeen_nursery, "report/Aberdeen_nursery.csv")

perth_nursery <- nursery %>% 
  filter(Service_town == "Perth") %>% 
  filter(Council_Area_Name == "Perth & Kinross") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(perth_nursery, "report/perth_nursery.csv")

dunfermline_nursery <- nursery %>% 
  filter(Service_town == "Dunfermline") %>% 
  filter(Council_Area_Name == "Fife") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(dunfermline_nursery, "report/dunfermline_nursery.csv")

stirling_nursery <- nursery %>% 
  filter(Service_town == "Stirling") %>% 
  filter(Council_Area_Name == "Stirling") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(stirling_nursery, "report/stirling_nursery.csv")



inverness_nursery <- nursery %>% 
  filter(Service_town == "Inverness") %>% 
  filter(Council_Area_Name == "Highland") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(inverness_nursery, "report/inverness_nursery.csv")

elgin_nursery <- nursery %>% 
  filter(Service_town == "Elgin") %>% 
  filter(Council_Area_Name == "Moray") %>% 
  select(-c(Complaints_upheld_2223, Complaints_upheld_2324, Enforcements_issued_2223, Enforcements_issued_2324, second_reqs_2425)) %>% 
  arrange(Complaints_upheld_since_22_23) %>% 
  arrange(desc(Average)) 

write.csv(elgin_nursery, "report/elgin_nursery.csv")

### Local authority averages

averages <- df %>%
  filter(CareService == 'Care Home Service' | CareService == 'Day Care of Children') %>% 
  filter(Subtype =='Older People' | Subtype == 'Day Care of Children (under 3s)') %>% 
  filter(!is.na(Publication_of_Latest_Grading)) %>% 
  select(-c(Subtype, GradeSpread, ServiceStatus)) %>% 
  rowwise() %>%
  mutate(`Average` = mean(c_across(starts_with("KQ")), na.rm = TRUE)) %>% 
  mutate(`Complaints_upheld_since_22_23` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate(`Enforcements_upheld_since_22_23` = sum(c_across(starts_with("Enforcements_issued_")), na.rm = TRUE))

averages <- averages %>% 
  group_by(Council_Area_Name, CareService) %>% 
  summarise(
    'Wellbeing support' = mean(KQ_Support_Wellbeing, na.rm = TRUE),
    'Care and support' = mean(KQ_Care_and_Support_Planning, na.rm = TRUE),
    'Setting' = mean(KQ_Setting, na.rm = TRUE),
    'Staffing' = mean(KQ_Staff_Team, na.rm = TRUE),
    'Leadership' = mean(KQ_Leadership, na.rm = TRUE),
    'Play and learning' = mean(KQ_Care_Play_and_Learning, na.rm = TRUE),
    'Average grade' = mean(Average, na.rm = TRUE)
  ) 


averages_care_homes <- averages %>% 
  filter(CareService == 'Care Home Service') %>% 
  select(-c(CareService, 'Play and learning'))

averages_nursery <- averages %>% 
  filter(CareService == 'Day Care of Children') %>% 
  select(-c(CareService, `Wellbeing support`, `Care and support`))

write.csv(averages_care_homes, "report/care_home_averages.csv", row.names = FALSE)
write.csv(averages_nursery, "report/nursery_averages.csv", row.names = FALSE)
         
