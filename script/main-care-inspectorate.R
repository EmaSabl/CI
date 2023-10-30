library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
## Load CI datastore


all <- read_csv("data/MDSF_latest.csv")

#check changes

x <- read.csv("data/CIfull.csv") #previous data
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
cancelled_scot_month <- read_csv("data/cancelled_scot.csv") ## previous data

cancelled_scot <- right_join (cancelled_scot_month,
            cancelled_scot, by='CareService')
# export cancelled data sets
write.csv(cancelled_scot, "data/cancelled_scot.csv", row.names = FALSE)
write.csv(cancelled_LA, "data/cancelled_LA.csv", row.names = FALSE)
