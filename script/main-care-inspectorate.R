##Libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
## Load CI datastore and clear not needed columns


all <- read_csv("https://public-care-inspectorate-bucket.s3.eu-north-1.amazonaws.com/CI-latest/MDSF_latest.csv")

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

## Binding to last months data ####
## Cancelled and new services ####

all_prev <- read.csv("MDSF_data_31 August 2023.csv")

all_prev <- all_prev %>%  
  select(all_of(columns_keep))

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
## Add to spreadsheet stored in github
## LINK FROM GITHUB ####
#  x<- read.csv("cancelled_scot.csv") ## previous data 
# 
# right_join (x, 
#             cancelled_scot)
# 
# write.csv(cancelled_scot, "", row.names = FALSE)


# Cancelled services by ownership type across Scotland 

cancelled_type <- cancelled_services %>%
  group_by(ServiceType) %>% 
  summarise(n = n())

owner_options <- tibble(ServiceType = unique(all$ServiceType))

cancelled_type <- full_join(cancelled_type, 
                            owner_options,
                            by = ) %>% 
  mutate(n = ifelse(is.na(n), 0, n))
# Save to column with name on preceding month (using functions above)

cancelled_type <- cancelled_type %>% 
  rename(
    !!paste(month.abb[month_update], year_update) := n
  )

## Add to spreadsheet stored in github
## LINK FROM GITHUB ####
#  x_type <- read.csv("cancelled_type.csv") ## previous data 
# right_join (x_type, 
#             cancelled_type)
# 
# write.csv(cancelled_type, "", row.names = FALSE)


## New services (entirely new services) ####
# After joining the two together, filter for services registered last month
update_date <- paste(year_update, month_update, sep = "_")

new_services <- anti_join(all, all_prev, by = "ServiceName")  %>% 
  filter(ServiceName != "") %>% 
  filter(Date_Reg == update_date)

new_LA <- new_services %>% 
  group_by(CareService, Council_Area_Name) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  pivot_wider(names_from = CareService, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate('Month total' = rowSums(select(., -(Council_Area_Name))))

## new across Scotland - by ownership type
new_scot <- new_services %>% 
  group_by(CareService) %>% 
  summarise(n = n())

new_scot <- full_join(new_scot, 
                      service_options) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

## add date

new_scot <- new_scot %>% 
  rename(
    !!paste(month.abb[month_update], year_update) := n
  )
## Add to spreadsheet stored in github
## LINK FROM GITHUB ####
#  x<- read.csv("cancelled_scot.csv") ## previous data 
# 
# right_join (x, 
#             cancelled_scot)
# 
# write.csv(cancelled_scot, "", row.names = FALSE)

# new services by ownership type 
new_type <- new_services %>%
  group_by(ServiceType) %>% 
  summarise(n = n())

new_type <- full_join(new_type, 
                            owner_options,
                            by = ) %>% 
  mutate(n = ifelse(is.na(n), 0, n))
# Save to column with name on preceding month (using functions above)

new_type <- new_type %>% 
  rename(
    !!paste(month.abb[month_update], year_update) := n
  )

## Add to spreadsheet stored in github
## LINK FROM GITHUB ####
#  x_type <- read.csv("cancelled_type.csv") ## previous data 
# right_join (x_type, 
#             cancelled_type)
# 
# write.csv(cancelled_type, "", row.names = FALSE)


## Separate into child and adult services 

adultservs <- all %>%  
  filter(Client_group == "Adults")

childservs <- all %>% 
  filter(Client_group == "Children")

## ADULT SERVICES DATA #######################

## Average and count of each ranking for LAs ######
## Note there is no care and play for adults


AdultLA_avg <- adultservs %>% 
  group_by(Council_Area_Name) %>% 
  summarise(
    Avg_supportwellbeing = mean(KQ_Support_Wellbeing, na.rm = TRUE),
    Avg_caresupport = mean(KQ_Care_and_Support_Planning, na.rm = TRUE),
    Avg_setting = mean(KQ_Setting, na.rm = TRUE),
    Avg_staff = mean(KQ_Staff_Team, na.rm = TRUE),
    Avg_leadership = mean(KQ_Leadership, na.rm = TRUE)
  )


## Pivot longer based on the graded columns (KQ)

adultservs_long <- adultservs %>%
  pivot_longer(
    cols = starts_with("KQ_"),
    names_to = "question",
    values_to = "grade"
  )
##Count the numbers of each grade for each local authority

adultLAgrades <- adultservs_long %>%
  group_by(Council_Area_Name, question, grade) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  complete(Council_Area_Name, question, grade, fill = list(Count = 0))

adultLAgrades <- adultLAgrades %>%
  mutate(grade_means = case_when(grade == 1 ~ "Unsatisfactory",
                                 grade == 2 ~ "Weak",
                                 grade == 3 ~ "Adequate",
                                 grade == 4 ~ "Good",
                                 grade == 5 ~ "Very good",
                                 grade == 6 ~ "Excellent")) %>%
  mutate(grade_means = if_else(is.na(grade_means), "No grade", grade_means))
## Remove play category from adult data
adultLAgrades <- adultLAgrades %>% 
  filter(!(question == "KQ_Care_Play_and_Learning"))

## Each grade in its own column 

adultLAgrades <- select(adultLAgrades, -grade)
adultLAgradesspread <- spread(adultLAgrades, key = grade_means, value = Count)

# Sum for each LA and convert to percentages

adultLAgrades_percent <- adultLAgradesspread %>% 
  mutate(Total = select(., `Adequate`, `Excellent`, `Good`, 
                      `Unsatisfactory`, `Very good`, `Weak`)  %>% rowSums(na.rm = TRUE)) %>% 
  mutate(`Unsatisfactory` = Unsatisfactory/Total, 
         `Weak` = Weak/Total, 
         `Adequate`= Adequate/Total,
         `Good` = Good/Total, 
         `Very good` = `Very good`/Total,
         `Excellent` = Excellent/Total
         ) %>% 
  select(-c(Total, `No grade`))


write.csv(adultLAgrades_percent, "adultgradesavg.csv", row.names = FALSE)
write.csv(AdultLA_avg, "adultsexampleforradial.csv", row.names = FALSE)

## CHILD SERVICES DATA #######################

## Average and count of each ranking for LAs ######
## Note there is no care and play for adults

childLA_avg <- childservs %>% 
  group_by(Council_Area_Name) %>%  
  summarise(
    Avg_supportwellbeing = mean(KQ_Support_Wellbeing, na.rm = TRUE),
    Avg_caresupport = mean(KQ_Care_and_Support_Planning, na.rm = TRUE),
    Avg_setting = mean(KQ_Setting, na.rm = TRUE),
    Avg_staff = mean(KQ_Staff_Team, na.rm = TRUE),
    Avg_leadership = mean(KQ_Leadership, na.rm = TRUE),
    Avg_play = mean(KQ_Care_Play_and_Learning, na.rm = TRUE)
     )
## Add Scotland total - note you have not done a long pivot and 
## Scotland total for the adult services

childavg_scot <- childLA_avg %>%
  adorn_totals(name= "Scotland") %>%
  mutate(across(where(is.numeric),
                ~ replace(., n(), .[n()]/(n()-1)))) %>%
  as_tibble

childavg_long <- childavg_scot %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "question",
    values_to = "avg"
  )

  
## Pivot longer based on the graded columns (KQ)

childservs_long <- childservs %>%
  pivot_longer(
    cols = starts_with("KQ_"),
    names_to = "question",
    values_to = "grade"
  )
##Count the numbers of each grade for each local authority

childLAgrades <- childservs_long %>%
  group_by(Council_Area_Name, question, grade) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  complete(Council_Area_Name, question, grade, fill = list(Count = 0))

childLAgrades <- childLAgrades %>%
  mutate(grade_means = case_when(grade == 1 ~ "Unsatisfactory",
                                 grade == 2 ~ "Weak",
                                 grade == 3 ~ "Adequate",
                                 grade == 4 ~ "Good",
                                 grade == 5 ~ "Very good",
                                 grade == 6 ~ "Excellent")) %>%
  mutate(grade_means = if_else(is.na(grade_means), "No grade", grade_means))


## Each grade in its own column 

childLAgrades = select(childLAgrades, -grade)
childLAgradesspread <- spread(childLAgrades, key = grade_means, value = Count) 


## COMPLAINTS upheld####
## For complaints no entry is equal to 0
comps <- all %>%
  mutate_at(vars("Complaints_upheld_2122", "Complaints_upheld_2223", "Complaints_upheld_2324"),
            list(~ ifelse(is.na(.), 0, .)))

#Sum complaints for each row, and then group into LA and client group

compsLA <- comps %>%
  rowwise() %>%
  mutate(Complaints_since_2122 = sum(c_across(starts_with("Complaints_upheld_")))) %>%
  ungroup() %>%
  group_by(Council_Area_Name, Client_group) %>%
  summarise("All since 2021/22" = sum(Complaints_since_2122),
            "2023/24" = sum(Complaints_upheld_2324),
            "2022/23" = sum(Complaints_upheld_2223),
            "2021/22" = sum(Complaints_upheld_2122))  %>% 
  pivot_wider(id_cols = Council_Area_Name, 
                        names_from = Client_group, 
                        values_from = c("All since 2021/22", "2023/24", "2022/23", "2021/22"))

# write.csv(compsLA, "complaints.csv", row.names = FALSE)

## TABLES ####

# Select only needed columns and rename them for ease of use
columns_keep_table <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                  "Service_town", "Service_Postcode",  "Client_group","MinGrade_change", 
                  "Publication_of_Latest_Grading","KQ_Support_Wellbeing", 
                  "KQ_Care_and_Support_Planning", "KQ_Setting", "KQ_Staff_Team",
                  "KQ_Leadership", "KQ_Care_Play_and_Learning", "Last_inspection_Date", "Date_Reg", 
                  "Complaints upheld since 21/22", "Enforcements since 21/22")



care_tables <- all %>% 
  rowwise() %>%
  mutate(`Complaints upheld since 21/22` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate('Enforcements since 21/22' = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  select(all_of(columns_keep_table)) %>%
  rename(Type = CareService,
         Provider = ServiceType,
         Name = ServiceName,
         Town = Service_town,
         Postcode = Service_Postcode,
         Clients = Client_group,
         'Wellbeing support' = KQ_Support_Wellbeing,
        'Care and support' = KQ_Care_and_Support_Planning,
         'Environment' = KQ_Setting,
        Staffing = KQ_Staff_Team,
        Leadership = KQ_Leadership,
        'Care and play' = KQ_Care_Play_and_Learning,
        'Grades published' = Publication_of_Latest_Grading,
        Registered = Date_Reg
         )


## Create 9 separate tables

day_care_of_children <- care_tables %>% 
  filter(Type == "Day Care of Children" ) %>% 
  mutate(Subtype = ifelse(Subtype == "Day Care of Children (under 3s)",
                          "Under 3s", "Over 3s")) %>% 
  subset(select = -c(`Wellbeing support`,`Care and support`, Type))

write.csv(day_care_of_children, "daycareofchildrentable.csv", row.names = FALSE)
support_service <- care_tables %>% 
  filter(Type == "Support Service") %>% 
  subset(select = -c(`Care and play`, Type))

#housing has a single grade under environment
housing_support <- care_tables %>% 
  filter(Type == "Housing Support Service") %>% 
  subset(select = -c(`Care and play`, Type))

#no postcodes are included for child minding 
child_minding <- care_tables %>% 
  filter(Type == "Child Minding") %>% 
  subset(select = -c(`Postcode`, `Wellbeing support`, `Care and support`, Type))

care_homes <- care_tables %>% 
  filter(Type == "Care Home Service") %>% 
  subset(select = -c(`Care and play`, Type))
  

accom <- care_tables %>% 
  filter(Type == "School Care Accommodation Service" |
           Type == "Offender Accommodation Service" |
           Type == "Secure Accommodation Service") %>% 
  subset(select = -c(`Care and play`, Type))

# How interested are we in nursing agencies? There are 127 entries in the August data
# nurse_agency <- care_tables %>% 
#   filter(Type == "Nurse Agency")

child_care_agency <- care_tables %>% 
  filter(Type == "Child Care Agency")%>% 
  subset(select = -c(Subtype, `Environment`, `Care and play`, Type))


adoption_fostering <- care_tables %>% 
  filter(Type == "Adoption Service" |
           Type == "Fostering Service") %>% 
  subset(select = -c(Subtype, `Environment`, `Care and play`, Type))


adult_placement <- care_tables %>% 
  filter(Type == "Adult Placement Service")%>% 
  subset(select = -c(`Environment`, `Care and play`, Type))

## ENFORCEMENTS ####
enforcements <- all %>% 
  filter(!is.na(Enforcements_issued_2122) |
           !is.na(Enforcements_issued_2223) |
           !is.na(Enforcements_issued_2324))

enforcementsLA <- enforcements %>%
        group_by(Council_Area_Name) %>%
        summarise(`Enforcements in 21/22` = sum(Enforcements_issued_2122,na.rm = TRUE),
           `Enforcements in 22/23` = sum(Enforcements_issued_2223, na.rm = TRUE),
           `Enforcements in 23/24` = sum(Enforcements_issued_2324,na.rm = TRUE))

enforcementscare <- enforcements %>% 
  group_by(CareService) %>%
  summarise(`Enforcements in 21/22` = sum(Enforcements_issued_2122, na.rm = TRUE),
            `Enforcements in 22/23` = sum(Enforcements_issued_2223, na.rm = TRUE),
            `Enforcements in 23/24` = sum(Enforcements_issued_2324, na.rm = TRUE))

## Export csvs

write.csv(all, "data/CI_full.csv", row.names = FALSE)



