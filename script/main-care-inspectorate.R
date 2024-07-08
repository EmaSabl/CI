library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringdist)
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
##Fix variations in LA data 
correct_names <- c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll & Bute", "City of Edinburgh",
                "Clackmannanshire", "Dumfries & Galloway", "Dundee City", "East Ayrshire", "East Dunbartonshire",
                "East Lothian", "East Renfrewshire", "Falkirk", "Fife", "Glasgow City", "Highland", "Inverclyde",
                "Midlothian", "Moray", "Na h-Eileanan Siar", "North Ayrshire", "North Lanarkshire", "Orkney Islands",
                "Perth & Kinross", "Renfrewshire", "Scottish Borders", "Shetland Islands", "South Ayrshire",
                "South Lanarkshire", "Stirling", "West Dunbartonshire", "West Lothian", "outside Scotland")

correct_spelling <- function(name, correct_names, threshold = 0.30) {
  if (is.na(name)) {
    return(name)
  }
  
  # Specific condition to handle the given code
  if (name == "SP2021000143") {
    return("Scottish Borders")
  }
  if (name == "Edinburgh, City of") {
    return("City of Edinburgh")
  }
  if (name == "Edinburgh") {
    return("City of Edinburgh")
  }
  
  if (name %in% correct_names) {
    return(name)
  } else {
    distances <- stringdist(name, correct_names, method = "jw") # Jaro-Winkler distance
    min_distance <- min(distances)
    closest <- correct_names[which.min(distances)]
    
    # Set a threshold to avoid incorrect corrections
    if (min_distance < threshold) {
      message(sprintf("Correcting misspelling: '%s' to '%s'", name, closest))
      return(closest)
    } else {
      message(sprintf("No suitable correction found for '%s'", name))
      return(name)
    }
  }
}
all <- all %>%
  mutate(Council_Area_Name = sapply(Council_Area_Name, correct_spelling, correct_names = correct_names))

all_prev <- all_prev %>%
  mutate(Council_Area_Name = sapply(Council_Area_Name, correct_spelling, correct_names = correct_names))


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
              values_fill = 0)


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

cancelled_scot <- left_join (cancelled_scot_month,
            cancelled_scot, by='CareService')

## Monthly breakdown for LA cancelled services

cancel_month <- cancelled_services %>%
  group_by(Council_Area_Name) %>% 
  summarise(n = n()) %>% 
  rename(!!paste(month.abb[month_update], year_update) := n,
         'Council' = Council_Area_Name) %>% 
  mutate(Council = as.character(Council))

cancelled_LA_month <- read_csv("data/cancelled_LA_month.csv")

cancelled_LA_month <- full_join(cancelled_LA_month,
                                cancel_month,
                                by = 'Council')
cancelled_LA_month[is.na(cancelled_LA_month)] <- 0

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
cancelled_type_final <- left_join (cancelled_type_month,
             cancelled_type, by='ServiceType')

# export cancelled data sets


#write.csv(cancelled_scot, "data/cancelled_scot.csv", row.names = FALSE)
#write.csv(cancelled_LA, "data/cancelled_LA.csv", row.names = FALSE)
#write.csv(cancelled_type_final, "data/cancelled_type.csv", row.names = FALSE)
#write.csv(cancelled_LA_month, "data/cancelled_LA_month.csv", row.names = FALSE)

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
new_services_scot_final <- left_join (new_services_scot, 
             new_scot)
 
#LA monthly update 
new_month <- new_services %>%
  group_by(Council_Area_Name) %>% 
  summarise(n = n()) %>% 
  rename(!!paste(month.abb[month_update], year_update) := n,
         'Council' = Council_Area_Name)

new_LA_month <- read_csv("data/new_LA_month.csv")

new_LA_month <- full_join(new_LA_month,
                                new_month,
                                by = 'Council')
new_LA_month[is.na(new_LA_month)] <- 0

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
new_type_final <- left_join (new_type_month, 
             new_type)

#export new services information 

#write.csv(new_services_scot_final, "data/new_services_scot.csv", row.names = FALSE)
#write.csv(new_type_final, "data/new_services_type.csv", row.names = FALSE)
#write.csv(new_LA, "data/new_services_LA.csv", row.names = FALSE)
#write.csv(new_LA_month, "data/new_LA_month.csv", row.names = FALSE)

## TABLES ####

# Select only needed columns and rename them for ease of use
columns_keep_table <- c("CareService", "Subtype", "ServiceType", "ServiceName",
                        "Service_town", "Council_Area_Name",  "Client_group", 
                        "Publication_of_Latest_Grading","KQ_Support_Wellbeing", 
                        "KQ_Care_and_Support_Planning", "KQ_Setting", "KQ_Staff_Team",
                        "KQ_Leadership", "KQ_Care_Play_and_Learning", "Last_inspection_Date", "Date_Reg", 
                        "Complaints upheld since 22/23", "Enforcements since 22/23")

#These can be added if useful "MinGrade_change","Service_Postcode"

care_tables <- all %>% 
  rowwise() %>%
  mutate(`Complaints upheld since 22/23` = sum(c_across(starts_with("Complaints_upheld_")), na.rm = TRUE)) %>% 
  mutate('Enforcements since 22/23' = sum(c_across(starts_with("Enforcements_issued_")), na.rm = TRUE)) %>% 
  filter(ServiceStatus == "Active") %>% 
  select(all_of(columns_keep_table)) %>%
  rename(Type = CareService,
         Provider = ServiceType,
         Name = ServiceName,
         Town = Service_town,
         Council = Council_Area_Name,
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

care_homes <- care_tables %>% 
  filter(Clients == "Adults") %>% 
  filter(Type == "Care Home Service") %>% 
  subset(select = -c(`Care and play`, `Type`, `Clients`))

all_childcare <- care_tables %>% 
  filter(Type == "Day Care of Children" | Type == "Child Minding" | Type == "Child Care Agency") %>% 
  subset(select = -c(`Wellbeing support`, `Care and support`, `Subtype`, `Clients`))

day_care_of_children <- care_tables %>% 
  filter(Type == "Day Care of Children" ) %>% 
  mutate(Subtype = ifelse(Subtype == "Day Care of Children (under 3s)",
                          "Under 3s", "Over 3s")) %>% 
  subset(select = -c(`Wellbeing support`,`Care and support`, `Type`))

#no postcodes are included for child minding 
child_minding <- care_tables %>% 
  filter(Type == "Child Minding") %>% 
  subset(select = -c(`Wellbeing support`, `Care and support`, `Type`))

child_care_agency <- care_tables %>% 
  filter(Type == "Child Care Agency")%>% 
  subset(select = -c(Subtype, `Setting`, `Care and play`, `Type`))

### The below tables are not being used###
support_service <- care_tables %>% 
  filter(Type == "Support Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

#housing has a single grade under environment
housing_support <- care_tables %>% 
  filter(Type == "Housing Support Service") %>% 
  subset(select = -c(`Care and play`, `Type`))

accom <- care_tables %>% 
  filter(Type == "School Care Accommodation Service" |
           Type == "Offender Accommodation Service" |
           Type == "Secure Accommodation Service") %>% 
  subset(select = -c(`Care and play`, `Type`))


adoption_fostering <- care_tables %>% 
  filter(Type == "Adoption Service" |
           Type == "Fostering Service") %>% 
  subset(select = -c(Subtype, `Setting`, `Care and play`, `Type`))


adult_placement <- care_tables %>% 
  filter(Type == "Adult Placement Service")%>% 
  subset(select = -c(`Setting`, `Care and play`, `Type`))

## histogram sheets
care_home_hist <- care_homes %>% 
  select(-c(Town, Registered, Last_inspection_Date, 'Enforcements since 22/23')) %>% 
  mutate('Total' = sum(c_across('Wellbeing support':'Leadership' ), na.rm = TRUE))

day_care_hist <- day_care_of_children %>% 
  select(-c(Town, Registered, Last_inspection_Date, 'Enforcements since 22/23', Clients)) %>% 
  mutate('Total' = sum(c_across('Setting':'Care and play' ), na.rm = TRUE))

childmind_hist <- child_minding %>% 
  select(-c(Subtype, Town, Registered, Last_inspection_Date, 'Enforcements since 22/23', Clients)) %>% 
  mutate('Total' = sum(c_across('Setting':'Care and play' ), na.rm = TRUE))

#export tables 
write.csv(care_home_hist, "data/table/carehome_histo.csv", row.names = TRUE)
write.csv(day_care_hist, "data/table/daycare_histo.csv", row.names = TRUE)
write.csv(childmind_hist, "data/table/childmind_histo.csv", row.names = TRUE)


#write.csv(all_childcare, "data/table/all_childcare.csv", row.names = FALSE)
#write.csv(day_care_of_children, "data/table/day_care_of_children.csv", row.names = FALSE)
#write.csv(child_minding, "data/table/child_minding.csv", row.names = FALSE)
#write.csv(care_homes, "data/table/care_homes.csv", row.names = FALSE)
#write.csv(child_care_agency, "data/table/child_care_agency.csv", row.names = FALSE)



#########################################
#Total services
all_combinations <- expand(all, Council_Area_Name, CareService)
latest_date <-paste(month.abb[month_update], year_update, sep = " ")

total_services_LA <- all %>%
  group_by(Council_Area_Name, CareService) %>%
  summarise(n = n())  %>%
  full_join(all_combinations, by = c("Council_Area_Name", "CareService")) %>%
  rename(!!paste(latest_date) := n,
         'Council' = Council_Area_Name) %>% 
  mutate(Council = as.character(Council))

total_services_LA[is.na(total_services_LA)] <- 0

total_services_month <- read_csv("data/total_type_change_LA.csv")

total_services_month <- total_services_month %>%
  left_join(total_services_LA, by = c("Council", "CareService"))

#write.csv(total_services_month, "data/total_type_change_LA.csv", row.names = FALSE)



total_services_month_long <- total_services_month %>%
pivot_longer(cols = matches("^[A-Za-z]{3} \\d{4}$"),  # Regex to match 'MMM YYYY' pattern
                names_to = "Date",
                values_to = "Value")
 
 total_services_linegraph <- total_services_month_long %>%
   pivot_wider(names_from = Council,
               values_from = Value)

## just care home data 
careHomeTime <- total_services_linegraph %>% 
  filter(CareService == "Care Home Service") %>% 
  select(-CareService)
## just the kid stuff 
childTime <- total_services_linegraph %>% 
  filter(CareService == "Day Care of Children" | 
           CareService == "Child Minding" | 
           CareService == "Child Care Agency")

child_time_scotland <- childTime %>% 
  rowwise() %>% 
  select(-'outside Scotland') %>% 
  mutate('Scotland' = sum(c_across('Aberdeen City':'West Lothian'), na.rm = TRUE)) %>% 
  select(c(CareService, Date, Scotland))

care_time_scotland <- careHomeTime %>% 
  rowwise() %>% 
  select(-'outside Scotland') %>% 
  mutate('Scotland' = sum(c_across('Aberdeen City':'West Lothian'), na.rm = TRUE)) %>%  
  select(c(Date, Scotland))



#write.csv(total_services_linegraph, "data/LAservicebytime.csv", row.names = FALSE)
#write.csv(careHomeTime, "data/care_homes_totals.csv", row.names = FALSE)
#write.csv(childTime, "data/child_services_totals.csv", row.names = FALSE)
write.csv(child_time_scotland, "data/child_services_scotland.csv", row.names = FALSE)
write.csv(care_time_scotland, "data/care_services_scotland.csv", row.names = FALSE)

##SPLIT ADULT AND CHILD
## Adult services include just care homes
## Child services include just day care of children, child minding, child care agency

adultservs <- all %>%  
  filter(Client_group == "Adults") %>%
  filter(CareService == "Care Home Service") 


childservs <- all %>% 
  filter(Client_group == "Children") %>% 
  filter(CareService == "Day Care of Children" | 
          CareService == "Child Minding" | 
          CareService == "Child Care Agency")

## COMPLAINTS upheld####

##new code to replace the old
adult_totals <- adultservs %>% 
  group_by(Council_Area_Name) %>%
  summarise(n = n())  %>% 
  rename("Services" = n,
         'Council' = Council_Area_Name) %>% 
  mutate(Council = as.character(Council))

child_totals <- childservs %>%  
  group_by(Council_Area_Name) %>%
  summarise(n = n())  %>% 
  rename("Services" = n,
         'Council' = Council_Area_Name) %>% 
  mutate(Council = as.character(Council))


comps_adult_past <- read_csv("data/adult_complaints.csv")
comps_adult <- adultservs %>% 
  mutate_at(vars("Complaints_upheld_2425"),
            list(~ ifelse(is.na(.), 0, .))) %>% 
  group_by(Council_Area_Name) %>% 
  summarise("2024/25" = sum(Complaints_upheld_2425)) %>%
  rename('Council'="Council_Area_Name")

comps_adult_past <- comps_adult_past%>% 
  select(-c("2024/25", "Services"))

complaints_adult <- left_join(comps_adult_past, comps_adult, by = 'Council') 

complaints_adult <- left_join(complaints_adult, adult_totals, by='Council')


##child complaints
comps_child_past <- read_csv("data/child_complaints.csv")
comps_child <- childservs %>% 
  mutate_at(vars("Complaints_upheld_2425"),
            list(~ ifelse(is.na(.), 0, .))) %>% 
  group_by(Council_Area_Name) %>% 
  summarise("2024/25" = sum(Complaints_upheld_2425)) %>%
  rename('Council'="Council_Area_Name")

comps_child_past <- comps_child_past%>% 
  select(-c("2024/25", "Services"))

complaints_child <- left_join(comps_child_past, comps_child, by = 'Council') 

complaints_child <- left_join(complaints_child, child_totals, by = 'Council')

#export complaints tables
#write.csv(complaints_adult, "data/adult_complaints.csv", row.names = FALSE)
#write.csv(complaints_child, "data/child_complaints.csv", row.names = FALSE)

## ENFORCEMENTS ####
##################################


enforce_adult_past <- read_csv("data/adult_enforcements.csv")
enforce_adult <- adultservs %>% 
  mutate_at(vars("Enforcements_issued_2425"),
            list(~ ifelse(is.na(.), 0, .))) %>% 
  group_by(Council_Area_Name) %>% 
  summarise("2024/25" = sum(Enforcements_issued_2425)) %>%
  rename('Council'="Council_Area_Name")

enforce_adult_past <- enforce_adult_past%>% 
  select(-c("2024/25", "Services"))

enforcements_adult <- enforce_adult_past %>% 
  left_join(enforce_adult, by = "Council") 

enforcements_adult <- left_join(enforcements_adult, adult_totals, by = 'Council')


##child enforcements
enforce_child_past <- read_csv("data/child_enforcements.csv")
enforce_child <- childservs %>% 
  mutate_at(vars("Enforcements_issued_2425"),
            list(~ ifelse(is.na(.), 0, .))) %>% 
  group_by(Council_Area_Name) %>% 
  summarise("2024/25" = sum(Enforcements_issued_2425)) %>%
  rename('Council'="Council_Area_Name")

enforce_child_past <- enforce_child_past%>% 
  select(-c("2024/25", "Services"))

enforcements_child <- enforce_child_past %>% 
  left_join(enforce_child, by = "Council") 

enforcements_child <- left_join(enforcements_child, child_totals, by = 'Council')


#write.csv(enforcements_adult, "data/adult_enforcements.csv", row.names = FALSE)
#write.csv(enforcements_child, "data/child_enforcements.csv", row.names = FALSE)

# Care service type breakdown
#enforcementscare <- enforcements %>% 
#  group_by(CareService) %>%
#  summarise(`Enforcements in 22/23` = sum(Enforcements_issued_2223, na.rm = TRUE),
#            `Enforcements in 23/24` = sum(Enforcements_issued_2324, na.rm = TRUE),
#            `Enforcements in 24/25` = sum(Enforcements_issued_2425,na.rm = TRUE))
#
#write.csv(enforcementscare, "data/enforcements_service.csv", row.names = FALSE)


## GRADES ######################################################
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
## Add filter to PJ and Courier 

PJ_AdultLA_avg <- AdultLA_avg %>% 
  filter(Council == "Aberdeen City" | Council == "Aberdeenshire" | Council == "Highland" | 
         Council == "Na h-Eileanan Siar" | Council == "Orkney Islands" |  Council == "Shetland Islands" | Council == "Moray")


Cour_AdultLA_avg <- AdultLA_avg %>% 
  filter(Council ==  "Angus" | Council == "Dundee City" | Council == "Fife" |
           Council == "Perth & Kinross" | Council == "Stirling")

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
    'Setting' = mean(KQ_Setting, na.rm = TRUE),
    'Staffing' = mean(KQ_Staff_Team, na.rm = TRUE),
    'Leadership' = mean(KQ_Leadership, na.rm = TRUE),
    'Play and learning' = mean(KQ_Care_Play_and_Learning, na.rm = TRUE)
     ) %>% 
  filter(Council_Area_Name != 'outside Scotland')  %>% 
  rename (Council = Council_Area_Name) 

## Add filter to PJ and Courier 

PJ_ChildLA_avg <- childLA_avg %>% 
  filter(Council == "Aberdeen City" | Council == "Aberdeenshire" | Council == "Highland" | 
           Council == "Na h-Eileanan Siar" | Council == "Orkney Islands" |  Council == "Shetland Islands" | Council == "Moray")


Cour_ChildLA_avg <- childLA_avg %>% 
  filter(Council ==  "Angus" | Council == "Dundee City" | Council == "Fife" |
           Council == "Perth & Kinross" | Council == "Stirling")


  
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
###### Scatter plots grades

adult_LA_avg_scatter <- AdultLA_avg %>% 
  pivot_longer(
    cols = c('Wellbeing support','Care and support', 'Setting', 'Staffing', 'Leadership'),
    names_to = "Grade", 
    values_to = "Average"
  )

adult_LA_avg_scatter <- adult_LA_avg_scatter %>% 
  mutate(colour_filter = Council)

adult_scatter_PJ <- adult_LA_avg_scatter %>% 
  mutate(colour_filter = ifelse(colour_filter %in% c("Aberdeen City", "Aberdeenshire", 
                                                     "Highland", "Na h-Eileanan Siar", "Orkney Islands", 
                                                     "Shetland Islands", "Moray"), colour_filter, "Other"))
adult_scatter_C <- adult_LA_avg_scatter %>% 
  mutate(colour_filter = ifelse(colour_filter %in% c("Dundee City", "Angus", 
                                                     "Fife", "Perth & Kinross", "Stirling"), colour_filter, "Other"))

child_LA_avg_scatter <- childLA_avg %>% 
  pivot_longer(
    cols = c('Play and learning', 'Setting', 'Staffing', 'Leadership'),
    names_to = "Grade", 
    values_to = "Average"
  ) 

child_LA_avg_scatter<-child_LA_avg_scatter %>% 
  mutate(colour_filter = Council)

child_scatter_PJ <- child_LA_avg_scatter %>% 
  mutate(colour_filter = ifelse(colour_filter %in% c("Aberdeen City", "Aberdeenshire", 
                                                     "Highland", "Na h-Eileanan Siar", "Orkney Islands", 
                                                     "Shetland Islands", "Moray"), colour_filter, "Other"))

child_scatter_C <- child_LA_avg_scatter %>% 
  mutate(colour_filter = ifelse(colour_filter %in% c("Dundee City", "Angus", 
                                                     "Fife", "Perth & Kinross", "Stirling"), colour_filter, "Other"))

###### Grades over time 

Adult_grades_time <- AdultLA_avg %>% 
  mutate(Date = !!paste(month.abb[month_update], year_update), .before = Council)


Adult_grades_historic <- read_csv("data/timeseriesAdultGrades.csv")

Adult_grades_timeseries <- rbind(Adult_grades_historic, Adult_grades_time)
Adult_grades_timeseries <- apply(Adult_grades_timeseries,2,as.character)


##children
child_grades_time <- childLA_avg %>%
  mutate(Date = !!paste(month.abb[month_update], year_update), .before = Council)

child_grades_historic <- read_csv("data/timeseriesChildGrades.csv")

child_grades_timeseries <- rbind(child_grades_historic, child_grades_time)
child_grades_timeseries <- apply(child_grades_timeseries,2,as.character)


#write.csv(Adult_grades_timeseries, "data/timeseriesAdultGrades.csv", row.names = FALSE)
#write.csv(child_grades_timeseries, "data/timeseriesChildGrades.csv", row.names = FALSE)



# exports for child and adult services
#write.csv(adult_scatter_PJ, "data/PJ_adult_grades_scatter.csv", row.names = FALSE)
#write.csv(adult_scatter_C, "data/C_adult_grades_scatter.csv", row.names = FALSE)
#write.csv(child_scatter_PJ, "data/PJ_child_grades_scatter.csv", row.names = FALSE)
#write.csv(child_scatter_C, "data/C_child_grades_scatter.csv", row.names = FALSE)


#write.csv(AdultLA_avg, "data/adult_services_avg_LA.csv", row.names = FALSE)
#write.csv(adultLAgrades_spread, "data/adult_grades_counts_LA.csv", row.names = FALSE)
#write.csv(childLA_avg, "data/child_services_avg_LA.csv", row.names = FALSE)
#write.csv(childLAgrades_spread, "data/child_grades_counts_LA.csv", row.names = FALSE)
#write.csv(PJ_AdultLA_avg, "data/PJ_radial_adult_grades.csv", row.names = FALSE)
#write.csv(Cour_AdultLA_avg, "data/C_radial_adult_grades.csv", row.names = FALSE)
#write.csv(PJ_ChildLA_avg, "data/PJ_radial_child_grade.csv", row.names = FALSE)
#write.csv(Cour_ChildLA_avg, "data/C_radial_child_grade.csv", row.names = FALSE)

#Finally update the csv for the previous month
#write.csv(all, "data/CIfull.csv", row.names = FALSE) 
