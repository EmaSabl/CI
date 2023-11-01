library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
## Load CI datastore


all <- read_csv("data/MDSF_latest.csv")

#check changes

x <- read_csv("data/CIfull.csv") #previous data
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
## CANCELLED SERVICES ####

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
cancelled_scot_month <- read_csv("data/cancelled_scot.csv") ## previous data

cancelled_scot <- right_join (cancelled_scot_month,
            cancelled_scot, by='CareService')

# Cancelled services by ownership type across Scotland 

cancelled_type <- cancelled_services %>%
  group_by(ServiceType) %>% 
  summarise(n = n())

owner_options <- tibble(ServiceType = unique(all$ServiceType))

cancelled_type <- full_join(cancelled_type, 
                            owner_options,
                            by = ) %>% 
  mutate(n = ifelse(is.na(n), 0, n))
# Save to column with name on preceding month (using date functions above)

cancelled_type <- cancelled_type %>% 
  rename(
    !!paste(month.abb[month_update], year_update) := n
  )
## Add to spreadsheet stored in github

cancelled_type_month <- read_csv("data/cancelled_type.csv") ## previous data
cancelled_type_final <- right_join (cancelled_type_month,
             cancelled_type, by='ServiceType')

# export cancelled data sets

## NOTE TO SELF REMEMBER TO UNCOMMENT THE CANCELLED SCOT####

#write.csv(cancelled_scot, "data/cancelled_scot.csv", row.names = FALSE)
write.csv(cancelled_LA, "data/cancelled_LA.csv", row.names = FALSE)
#write.csv(cancelled_type_final, "data/cancelled_type.csv, row.names = FALSE)

## NEW SERVICES ####
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

new_services_scot <- read_csv("data/new_services_scot.csv") ## previous data  
new_services_scot_final <- right_join (new_services_scot, 
             new_scot)
 


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

new_type_month <- read_csv("data/new_services_type.csv") ## previous data 
new_type_final <- right_join (new_type_month, 
             new_type)

#export new services information 
## NOTE TO SELF REMEMBER TO UNCOMMENT ####
# write.csv(new_services_scot_final, "data/new_services_scot.csv", row.names = FALSE)
write.csv(new_type_final, "data/new_services_type.csv", row.names = FALSE)
write.csv(new_LA, "data/new_services_LA.csv", row.names = FALSE)

## TABLES ####

# Select only needed columns and rename them for ease of use
columns_keep_table <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                  "Service_town",  "Client_group", 
                  "Publication_of_Latest_Grading","KQ_Support_Wellbeing", 
                  "KQ_Care_and_Support_Planning", "KQ_Setting", "KQ_Staff_Team",
                  "KQ_Leadership", "KQ_Care_Play_and_Learning", "Last_inspection_Date", "Date_Reg", 
                  "Complaints upheld since 21/22", "Enforcements since 21/22")

#These can be added if useful "MinGrade_change","Service_Postcode"

care_tables <- all %>% 
  rowwise() %>%
  mutate(`Complaints upheld since 21/22` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate('Enforcements since 21/22' = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  select(all_of(columns_keep_table)) %>%
  rename(Type = CareService,
         Provider = ServiceType,
         Name = ServiceName,
         Town = Service_town,
         Clients = Client_group,
         'Wellbeing support' = KQ_Support_Wellbeing,
        'Care and support' = KQ_Care_and_Support_Planning,
         'Setting' = KQ_Setting,
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
  subset(select = -c(`Wellbeing support`,`Care and support`, `Type`))


support_service <- care_tables %>% 
  filter(Type == "Support Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

#housing has a single grade under environment
housing_support <- care_tables %>% 
  filter(Type == "Housing Support Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

#no postcodes are included for child minding 
child_minding <- care_tables %>% 
  filter(Type == "Child Minding") %>% 
  subset(select = -c(`Wellbeing support`, `Care and support`, `Type`))

care_homes <- care_tables %>% 
  filter(Type == "Care Home Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

accom <- care_tables %>% 
  filter(Type == "School Care Accommodation Service" |
           Type == "Offender Accommodation Service" |
           Type == "Secure Accommodation Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

# How interested are we in nursing agencies? There are 127 entries in the August data
# nurse_agency <- care_tables %>% 
#   filter(Type == "Nurse Agency")

child_care_agency <- care_tables %>% 
  filter(Type == "Child Care Agency")%>% 
  subset(select = -c(Subtype, `Setting`, `Care and play`, `Type`))


adoption_fostering <- care_tables %>% 
  filter(Type == "Adoption Service" |
           Type == "Fostering Service") %>% 
  subset(select = -c(Subtype, `Setting`, `Care and play`, `Type`))


adult_placement <- care_tables %>% 
  filter(Type == "Adult Placement Service")%>% 
  subset(select = -c(`Setting`, `Care and play`, `Type`))

#export tables 
write.csv(day_care_of_children, "data/table/day_care_of_children.csv", row.names = FALSE)
write.csv(support_service, "data/table/support_service.csv", row.names = FALSE)
write.csv(housing_support, "data/table/housing_support.csv", row.names = FALSE)
write.csv(child_minding, "data/table/child_minding.csv", row.names = FALSE)
write.csv(care_homes, "data/table/care_homes.csv", row.names = FALSE)
write.csv(accom, "data/table/accom.csv", row.names = FALSE)
write.csv(child_care_agency, "data/table/child_care_agency.csv", row.names = FALSE)
write.csv(adoption_fostering, "data/table/adoption_fostering.csv", row.names = FALSE)
write.csv(adult_placement, "data/table/adult_placement.csv", row.names = FALSE)

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
#export complaints tables
write.csv(compsLA, "data/complaints_LA.csv", row.names = FALSE)

## ENFORCEMENTS ####
enforcements <- all %>% 
  filter(!is.na(Enforcements_issued_2122) |
           !is.na(Enforcements_issued_2223) |
           !is.na(Enforcements_issued_2324))

# LA breakdown
enforcementsLA <- enforcements %>%
        group_by(Council_Area_Name) %>%
        summarise(`Enforcements in 21/22` = sum(Enforcements_issued_2122,na.rm = TRUE),
           `Enforcements in 22/23` = sum(Enforcements_issued_2223, na.rm = TRUE),
           `Enforcements in 23/24` = sum(Enforcements_issued_2324,na.rm = TRUE))

# Care service type breakdown
enforcementscare <- enforcements %>% 
  group_by(CareService) %>%
  summarise(`Enforcements in 21/22` = sum(Enforcements_issued_2122, na.rm = TRUE),
            `Enforcements in 22/23` = sum(Enforcements_issued_2223, na.rm = TRUE),
            `Enforcements in 23/24` = sum(Enforcements_issued_2324, na.rm = TRUE))

write.csv(enforcementsLA, "data/enforcements_LA.csv", row.names = FALSE)
write.csv(enforcementscare, "data/enforcements_service.csv", row.names = FALSE)

## Splitting up adult and child services 

adultservs <- all %>%  
  filter(Client_group == "Adults")

childservs <- all %>% 
  filter(Client_group == "Children")

## ADULT SERVICES DATA #######################

## Average and count of each ranking for LAs ######
## Note there is no care and play for adults

#make sure charts indicate that grades are averages
AdultLA_avg <- adultservs %>% 
  group_by(Council_Area_Name) %>% 
  summarise(
    'Wellbeing support' = mean(KQ_Support_Wellbeing, na.rm = TRUE),
    'Care and support' = mean(KQ_Care_and_Support_Planning, na.rm = TRUE),
    'Setting' = mean(KQ_Setting, na.rm = TRUE),
    'Staffing' = mean(KQ_Staff_Team, na.rm = TRUE),
    'Leadership' = mean(KQ_Leadership, na.rm = TRUE)
  ) %>% 
  filter(Council_Area_Name != 'outside Scotland')  %>% 
  rename (Council = Council_Area_Name)


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
  mutate(grade_means = if_else(is.na(grade_means), "No grade", grade_means)) %>% 
  filter(question != "KQ_Care_Play_and_Learning") %>% 
  filter(Council_Area_Name != "outside Scotland") %>% 
  select(-grade) %>% 
  mutate(question = case_when(question == "KQ_Support_Wellbeing" ~ "Wellbeing support",
                              question == "KQ_Care_and_Support_Planning" ~ "Care and support",
                              question == "KQ_Setting" ~ "Setting",
                              question == "KQ_Staff_Team" ~ "Staffing",
                              question == "KQ_Leadership" ~ "Leadership"))

## Each grade in its own column 

adultLAgrades_spread <- spread(adultLAgrades, key = grade_means, value = Count)

# Sum for each LA and convert to percentages

# adultLAgrades_percent <- adultLAgrades_spread %>% 
#  mutate(Total = select(., `Adequate`, `Excellent`, `Good`, 
#                      `Unsatisfactory`, `Very good`, `Weak`)  %>% rowSums(na.rm = TRUE)) %>% 
#  mutate(`Unsatisfactory` = Unsatisfactory/Total, 
#         `Weak` = Weak/Total, 
#         `Adequate`= Adequate/Total,
#         `Good` = Good/Total, 
#         `Very good` = `Very good`/Total,
#         `Excellent` = Excellent/Total
#         ) %>% 
#  select(-c(Total, `No grade`))


## CHILD SERVICES DATA #######################

## Average and count of each ranking for LAs ######
## Note there is no care and play for adults

childLA_avg <- childservs %>% 
  group_by(Council_Area_Name) %>%  
  summarise(
    'Wellbeing support' = mean(KQ_Support_Wellbeing, na.rm = TRUE),
    'Care and support' = mean(KQ_Care_and_Support_Planning, na.rm = TRUE),
    'Setting' = mean(KQ_Setting, na.rm = TRUE),
    'Staffing' = mean(KQ_Staff_Team, na.rm = TRUE),
    'Leadership' = mean(KQ_Leadership, na.rm = TRUE),
    'Play and learning' = mean(KQ_Care_Play_and_Learning, na.rm = TRUE)
     ) %>% 
  filter(Council_Area_Name != 'outside Scotland')  %>% 
  rename (Council = Council_Area_Name) 

  
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
  mutate(grade_means = if_else(is.na(grade_means), "No grade", grade_means))  %>% 
  filter(Council_Area_Name != "outside Scotland") %>% 
  select(-grade) %>% 
  mutate(question = case_when(question == "KQ_Support_Wellbeing" ~ "Wellbeing support",
                              question == "KQ_Care_and_Support_Planning" ~ "Care and support",
                              question == "KQ_Setting" ~ "Setting",
                              question == "KQ_Staff_Team" ~ "Staffing",
                              question == "KQ_Leadership" ~ "Leadership",
                              question == "KQ_Care_Play_and_Learning" ~ "Play and learning" ))
                            

## Each grade in its own column 

childLAgrades_spread <- spread(childLAgrades, key = grade_means, value = Count) 

# exports for child and adult services
write.csv(AdultLA_avg, "data/adult_services_avg_LA.csv", row.names = FALSE)
write.csv(adultLAgrades_spread, "data/adult_grades_counts_LA.csv", row.names = FALSE)
write.csv(childLA_avg, "data/child_services_avg_LA.csv", row.names = FALSE)
write.csv(childLAgrades_spread, "data/child_grades_counts_LA.csv", row.names = FALSE)

#once fully automated - uncomment this
# write.csv(all, "data/CIfull.csv", row.names = FALSE) 
