##Parameters set for memory for RJDBC--------------------------------------------
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx81920m"))

##R packages----------------------------------------------------------------------
library(rlang)
library(dplyr)
library(tidyverse)
library(tibble)
library(janitor)
library(knitr)
library(grDevices)
# SK Note: if add_header_above is erroring out due to unused arguments, 
# you probably have an older version of kableExtra and need to update
library(kableExtra)
library(htmltools)
library(data.table)
library(reshape2)
library(xml2)
library(lubridate)
conflict_prefer("year", "lubridate")


# The sourced scripts in this script are only for testing purposes during development. Comment out when moving to production
## directory where user-defined functions (for kable table and others) and the formatted SQL tables are
#dir<-"~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/EmergencyEvents/2019_nCoV/T2D2/Metrics/E-mail Scripts/"
##Load user-defined functions------------------------------------------------------------------------------
#source(paste0(dir,"functions.R"))
##Retrieve Data from SQL Server & priority zips file--------------------------------------------------------
#source(paste0(dir, "load_data.R"))

#In results table, create a cases_contact variable that defines the case/contact logic-------------------

cases_contacts <- result %>%
  mutate(
    case_contact = 
      case_when(
        # Case logic, recordtype is "Case" and salesforce_contact_id is not missing 
        #MP (12/28/20): switch to using saleforce_contact_id for cases due to growing number of cases with missing maven_event_id
        recordtype == "Case" & salesforce_contact_id!="" & !is.na(salesforce_contact_id) ~ "Case",
        # Contact logic, recordtype is "Exposed" and salesforce_contact_id is not missing 
        recordtype == "Exposed" & salesforce_contact_id!="" & !is.na(salesforce_contact_id) ~ 
          "Contact")) %>% 
  # Keep only non-missing case/contacts per logic above
  filter(!is.na(case_contact)) 


# Load disposition definitions --------------------------------------------
source(paste0("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/",
              "EmergencyEvents/2019_nCoV/T2D2/Analyses and Reports/__Reports/",
              "Daily email report/production/disposition_definitions.R")) 


# Create base table with flags for report metrics -------------------------

base <- interaction %>%
  inner_join(cases_contacts, by = c("result__c"="salesforce_result_id"), keep = TRUE) %>%
  
  #assign time frames
  mutate(new = ifelse(createddate.y == Sys.Date()-1, 1, 0),
         in_window = ifelse(monitoring_status__c != "Monitoring Stopped" &
                              monitoring_end_date__c >= Sys.Date()-1, 1, 0)) %>%
  
  #differentiate confirmed cases, probable cases, symptomatic contacts, asymptomatic contacts
  mutate(confirmed_case = ifelse(case_contact == "Case", 1, 0),
         symptomatic_case = ifelse(case_contact == "Case" & anysymptoms == 1, 1, 0),
         probable_case = ifelse(case_contact == "Contact" &
                                  !salesforce_id %in% (cases_contacts %>% 
                                                         filter(case_contact == "Case") %>%
                                                         pull(salesforce_contact_id)) &
                                    cste_probable_case == 1, 1, 0),
         # symptomatic_contact = ifelse(case_contact == "Contact" & ever_symptomatic == 1, 1, 0),
         asymptomatic_contact = ifelse(case_contact == "Contact" & 
                                         presumed_positive__c == "false", 1, 0),
         presumed_positive = ifelse(case_contact == "Contact" & 
                                      presumed_positive__c == "true", 1, 0)) %>%
  
  #flags for intake metrics
  mutate(outside_nyc = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                                disposition__c == "Out of jurisdiction", 1, 0),
         in_nyc = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                           disposition__c != "Out of jurisdiction", 1, 0),
         congregate = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                               disposition__c == "Referred to NYC Health Department Congregate Settings Team" &
                               currently_live_in_a_congregate_setting == "true", 1, 0),
         pot_accurate_num = ifelse(!disposition__c %in% c("No Phone Number", 
                                                          "Incorrect or Inactive Number"), 1, 0),
         reached_initial = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                                    disposition__c %in% reached, 1, 0),
         attempted_24 = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                              disposition__c %in% attempted &
                                difftime(first_attempt_date_time,
                                         created_date_time__c.y, units = "hours") <= 24, 1, 0),
         complete_intake = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                                    disposition__c %in% completed, 1, 0),
         interview_24hr = ifelse(complete_intake == 1 & 
                                   difftime(first_completed_date_time, created_date_time__c.y, units = "hours") <= 24 &
                                   interaction_date_time__c >= created_date_time__c.y & 
                                   year(interaction_date_time__c) >= 2020, 1, 0),
         in_progress = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                                disposition__c == "In Progress", 1, 0),
         not_in_progress = ifelse(complete_intake != 1 & in_progress != 1, 1, 0),
         no_number = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                              disposition__c == "No Phone Number", 1, 0),
         incorrect_number = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                              disposition__c == "Incorrect or Inactive Number", 1, 0),
         other_not_started = ifelse(recordtypeid %in% c("Case Investigation", "Contact Intake") &
                                      not_in_progress == 1 & no_number == 0 & 
                                      incorrect_number == 0, 1, 0),
         no_close_contacts = ifelse(total_exposed_contacts__c == 0, 1, 0),
         close_contacts_1_9 = ifelse(total_exposed_contacts__c >=1 &
                                       total_exposed_contacts__c <= 9, 1, 0),
         close_contacts_10plus = ifelse(total_exposed_contacts__c >= 10, 1, 0)
  ) %>%
  
  #flags for monitoring metrics
  mutate(completed_monitoring = ifelse(recordtypeid %in% c("Case Phone Monitoring",
                                                           "Contact Phone Monitoring",
                                                           "Case SMS Monitoring",
                                                           "Contact SMS Monitoring") &
                                         disposition__c %in% c("Completed",
                                                               "Call Completed"), 1, 0)
         ) %>%
  # flags for location-based contacts
  mutate(location_bulk = ifelse(location_event__c != "" & 
                                  exposure_type__c == "Location Exposure", 1, 0),
         facility_named = ifelse(location_event__c != "" & 
                                   exposure_type__c == "Case Exposure", 1, 0)
  ) %>%
  
  # Deduplication based on disposition
  # and filter out contacts related to school closures
  filter(disposition__c != "Duplicate / Already Completed"
         ## MP (2/10/21): school contacts don't seem to be treated differently
         ## in how/when they are investigated
         # , school_contact != 1
         )
  

# Calculate metrics for Overall cases and contacts ------------------------
overall <- tibble(metrics = NA, number = NA, percent = NA, Domain = NA, time_frame = "Overall") %>%
  
  #Overall case metrics ####
  ## calculate total cases
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(presumed_positive, confirmed_case),
                        percent = round(sum(presumed_positive, confirmed_case) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases", Domain = "Case", time_frame = "Overall")) %>%
  
  ## calculate Referred from DOHMH
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(confirmed_case),
                        percent = round(sum(confirmed_case) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Referred from DOHMH", Domain = "Case", time_frame = "Overall")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Cases residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing outside of NYC", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Cases residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing in NYC", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Cases residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Cases residing in congregate settings", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases with potentially accurate phone numbers", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Unreached after investigation
  bind_rows(base %>% filter(recordtypeid %in% c("Case Investigation",
                                                "Case Phone Monitoring",
                                                "Case SMS Monitoring")) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>%
              group_by(salesforce_contact_id) %>%
              mutate(unreached_after_inv = ifelse(sum(complete_intake) > 0 &
                                                    sum(completed_monitoring) == 0, 1, 0)) %>%
              ungroup() %>%
              distinct(salesforce_contact_id, .keep_all = T) %>%
              summarize(number = sum(unreached_after_inv),
                        percent = round(sum(unreached_after_inv) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Unreached after investigation", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Did not start interview (doesn't make sense to do anymore after take no phone number/incorrect number out of the denominator)
  # bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
  #             summarize(number = sum(not_in_progress),
  #                       percent = round(sum(not_in_progress) /
  #                                         sum(incomplete_intake) * 100, 1)) %>%
  #             mutate(metrics = "Did not start interview", Domain = "Case", time_frame = "Overall")) %>%
  
  ## Contact elicitation
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1) %>%
              summarize(number = mean(total_exposed_contacts__c)) %>%
              mutate(metrics = "Mean # contacts elicited", Domain = "Case", time_frame = "Overall")) %>%
  
  # bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1) %>%
  #             summarize(number = median(total_exposed_contacts__c)) %>%
  #             mutate(metrics = "Median # contacts elicited", Domain = "Case", time_frame = "Overall")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1) %>%
              summarize(number = sum(no_close_contacts),
                        percent = round(sum(no_close_contacts) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "No close contacts", Domain = "Case", time_frame = "Overall")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1) %>%
              summarize(number = sum(close_contacts_1_9),
                        percent = round(sum(close_contacts_1_9) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "1-9 close contacts", Domain = "Case", time_frame = "Overall")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1) %>%
              summarize(number = sum(close_contacts_10plus),
                        percent = round(sum(close_contacts_10plus) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "10+ close contacts", Domain = "Case", time_frame = "Overall")) %>%
  
  #Overall contact metrics ####
  ## calculate total contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
            summarize(number = sum(presumed_positive, asymptomatic_contact),
                      percent = round(sum(presumed_positive, asymptomatic_contact) /
                                        sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
            mutate(metrics = "Contacts", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## calculate asymptomatic contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(asymptomatic_contact),
                        percent = round(sum(asymptomatic_contact) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Asymptomatic contact", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Location-based (bulk?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(location_bulk),
                        percent = round(sum(location_bulk) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Bulk upload", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Location-based (named?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(facility_named),
                        percent = round(sum(facility_named) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Named contact", Domain = "Contact", time_frame = "Overall")) %>%
  
    ## Contacts residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing outside of NYC", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Contacts residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in NYC", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Contacts residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in congregate settings", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts with potentially accurate phone numbers", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Unreached after investigation
  bind_rows(base %>% filter(recordtypeid %in% c("Contact Intake",
                                                "Contact Phone Monitoring",
                                                "Contact SMS Monitoring")) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>%
              filter(presumed_positive == 0) %>%
              group_by(salesforce_contact_id) %>%
              mutate(unreached_after_inv = ifelse(sum(complete_intake) > 0 &
                                                    sum(completed_monitoring) == 0, 1, 0)) %>%
              ungroup() %>%
              distinct(salesforce_contact_id, .keep_all = T) %>%
              summarize(number = sum(unreached_after_inv),
                        percent = round(sum(unreached_after_inv) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Unreached after investigation", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Contact", time_frame = "Overall")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Contact Intake") %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Contact", time_frame = "Overall")) %>%
  
  filter(!is.na(metrics), number != 0)
  

# Calculate metrics for Cases and Contacts Within Monitoring Window -------

window <- tibble(metrics = NA, number = NA, percent = NA, Domain = NA, time_frame = "In Monitoring Window") %>%
  
  #In Monitoring Window case metrics ####
  ## calculate total cases
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
            summarize(number = sum(presumed_positive, confirmed_case),
                      percent = round(sum(presumed_positive, confirmed_case) /
                                        sum(presumed_positive, confirmed_case) * 100, 1)) %>%
            mutate(metrics = "Cases", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## calculate Referred from DOHMH
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(confirmed_case),
                        percent = round(sum(confirmed_case) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Referred from DOHMH", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Cases residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing outside of NYC", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Cases residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing in NYC", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Cases residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Cases residing in congregate settings", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases with potentially accurate phone numbers", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Unreached after investigation
  bind_rows(base %>% filter(recordtypeid %in% c("Case Investigation",
                                                "Case Phone Monitoring",
                                                "Case SMS Monitoring"),
                            in_window == 1) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>%
              group_by(salesforce_contact_id) %>%
              mutate(unreached_after_inv = ifelse(sum(complete_intake) > 0 &
                                                    sum(completed_monitoring) == 0, 1, 0)) %>%
              ungroup() %>%
              distinct(salesforce_contact_id, .keep_all = T) %>%
              summarize(number = sum(unreached_after_inv),
                        percent = round(sum(unreached_after_inv) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Unreached after investigation", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Completed daily monitoring
  bind_rows(base %>% filter(recordtypeid %in% c("Case Investigation",
                                                "Case Phone Monitoring", 
                                                "Case SMS Monitoring"), in_window == 1) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>% 
              filter(interaction_date == Sys.Date()-1) %>%
              summarize(number = sum(completed_monitoring),
                        percent = round(sum(completed_monitoring) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Completed daily monitoring", Domain = "Case", time_frame = "In Monitoring Window")) %>%  
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", in_window == 1) %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  ## Contact elicitation
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, in_window == 1) %>%
              summarize(number = mean(total_exposed_contacts__c)) %>%
              mutate(metrics = "Mean # contacts elicited", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  # bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, in_window == 1) %>%
  #             summarize(number = median(total_exposed_contacts__c)) %>%
  #             mutate(metrics = "Median # contacts elicited", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, in_window == 1) %>%
              summarize(number = sum(no_close_contacts),
                        percent = round(sum(no_close_contacts) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "No close contacts", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, in_window == 1) %>%
              summarize(number = sum(close_contacts_1_9),
                        percent = round(sum(close_contacts_1_9) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "1-9 close contacts", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, in_window == 1) %>%
              summarize(number = sum(close_contacts_10plus),
                        percent = round(sum(close_contacts_10plus) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "10+ close contacts", Domain = "Case", time_frame = "In Monitoring Window")) %>%
  
  #In Monitoring Window contact metrics ####
  ## calculate total contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
            summarize(number = sum(presumed_positive, asymptomatic_contact),
                      percent = round(sum(presumed_positive, asymptomatic_contact) /
                                        sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
            mutate(metrics = "Contacts", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## calculate asymptomatic contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(asymptomatic_contact),
                        percent = round(sum(asymptomatic_contact) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Asymptomatic contact", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Location-based (bulk?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(location_bulk),
                        percent = round(sum(location_bulk) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Bulk upload", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Location-based (named?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(facility_named),
                        percent = round(sum(facility_named) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Named contact", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Contacts residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing outside of NYC", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Contacts residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in NYC", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Contacts residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in congregate settings", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts with potentially accurate phone numbers", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Unreached after investigation
  bind_rows(base %>% filter(recordtypeid %in% c("Contact Intake",
                                                "Contact Phone Monitoring",
                                                "Contact SMS Monitoring"),
                            in_window == 1) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>%
              filter(presumed_positive == 0) %>%
              group_by(salesforce_contact_id) %>%
              mutate(unreached_after_inv = ifelse(sum(complete_intake) > 0 &
                                                    sum(completed_monitoring) == 0, 1, 0)) %>%
              ungroup() %>%
              distinct(salesforce_contact_id, .keep_all = T) %>%
              summarize(number = sum(unreached_after_inv),
                        percent = round(sum(unreached_after_inv) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Unreached after investigation", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Completed daily monitoring
  bind_rows(base %>% filter(recordtypeid %in% c("Contact Intake",
                                                "Contact Phone Monitoring", 
                                                "Contact SMS Monitoring"), in_window == 1) %>%
              add_tally(wt = complete_intake, name = "n_complete_intakes") %>% 
              filter(interaction_date == Sys.Date()-1) %>%
              summarize(number = sum(completed_monitoring),
                        percent = round(sum(completed_monitoring) /
                                          mean(n_complete_intakes) * 100, 1)) %>%
              mutate(metrics = "Completed daily monitoring", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", in_window == 1) %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Contact", time_frame = "In Monitoring Window")) %>%
  
  filter(!is.na(metrics), number != 0)



# Calculate metrics for New Cases and Contacts --------------------------------------------
new <- tibble(metrics = NA, number = NA, percent = NA, Domain = NA, time_frame = "New") %>%
  
  #New case metrics ####
  ## calculate total cases
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
            summarize(number = sum(presumed_positive, confirmed_case),
                      percent = round(sum(presumed_positive, confirmed_case) /
                                        sum(presumed_positive, confirmed_case) * 100, 1)) %>%
            mutate(metrics = "Cases", Domain = "Case", time_frame = "New")) %>%
  
  ## calculate Referred from DOHMH
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(confirmed_case),
                        percent = round(sum(confirmed_case) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Referred from DOHMH", Domain = "Case", time_frame = "New")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Case", time_frame = "New")) %>%
  
  ## Cases residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing outside of NYC", Domain = "Case", time_frame = "New")) %>%
  
  ## Cases residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases residing in NYC", Domain = "Case", time_frame = "New")) %>%
  
  ## Cases residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Cases residing in congregate settings", Domain = "Case", time_frame = "New")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Case", time_frame = "New")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, confirmed_case) * 100, 1)) %>%
              mutate(metrics = "Cases with potentially accurate phone numbers", Domain = "Case", time_frame = "New")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Case", time_frame = "New")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Case", time_frame = "New")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Case", time_frame = "New")) %>%
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Case", time_frame = "New")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", new == 1) %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Case", time_frame = "New")) %>%
  
  ## Did not start interview (doesn't make sense to do anymore after take no phone number/incorrect number out of the denominator)
  # bind_rows(base %>% filter(recordtypeid == "Case Investigation") %>%
  #             summarize(number = sum(not_in_progress),
  #                       percent = round(sum(not_in_progress) /
  #                                         sum(incomplete_intake) * 100, 1)) %>%
  #             mutate(metrics = "Did not start interview", Domain = "Case", time_frame = "New")) %>%
  
  ## Contact elicitation
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, new == 1) %>%
              summarize(number = mean(total_exposed_contacts__c)) %>%
              mutate(metrics = "Mean # contacts elicited", Domain = "Case", time_frame = "New")) %>%
  
  # bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, new == 1) %>%
  #             summarize(number = median(total_exposed_contacts__c)) %>%
  #             mutate(metrics = "Median # contacts elicited", Domain = "Case", time_frame = "New")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, new == 1) %>%
              summarize(number = sum(no_close_contacts),
                        percent = round(sum(no_close_contacts) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "No close contacts", Domain = "Case", time_frame = "New")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, new == 1) %>%
              summarize(number = sum(close_contacts_1_9),
                        percent = round(sum(close_contacts_1_9) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "1-9 close contacts", Domain = "Case", time_frame = "New")) %>%
  
  bind_rows(base %>% filter(recordtypeid == "Case Investigation", complete_intake == 1, new == 1) %>%
              summarize(number = sum(close_contacts_10plus),
                        percent = round(sum(close_contacts_10plus) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "10+ close contacts", Domain = "Case", time_frame = "New")) %>%
  
  #New contact metrics ####
  ## calculate total contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
            summarize(number = sum(presumed_positive, asymptomatic_contact),
                      percent = round(sum(presumed_positive, asymptomatic_contact) /
                                        sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
            mutate(metrics = "Contacts", Domain = "Contact", time_frame = "New")) %>%
  
  ## calculate asymptomatic contacts
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(asymptomatic_contact),
                        percent = round(sum(asymptomatic_contact) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Asymptomatic contact", Domain = "Contact", time_frame = "New")) %>%
  
  ## calculate Symptomatic contacts (now based on presumed positive as it seems this is the criteria for getting a case investigation)
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(presumed_positive),
                        percent = round(sum(presumed_positive) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Symptomatic contact", Domain = "Contact", time_frame = "New")) %>%
  
  ## Location-based (bulk?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(location_bulk),
                        percent = round(sum(location_bulk) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Bulk upload", Domain = "Contact", time_frame = "New")) %>%
  
  ## Location-based (named?) uploads
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(facility_named),
                        percent = round(sum(facility_named) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Named contact", Domain = "Contact", time_frame = "New")) %>%
  
  ## Contacts residing outside NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(outside_nyc),
                        percent = round(sum(outside_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing outside of NYC", Domain = "Contact", time_frame = "New")) %>%
  
  ## Contacts residing in NYC
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(in_nyc),
                        percent = round(sum(in_nyc) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in NYC", Domain = "Contact", time_frame = "New")) %>%
  
  ## Contacts residing in congregate settings
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(congregate),
                        percent = round(sum(congregate) / sum(in_nyc) * 100, 1)) %>%
              mutate(metrics = "Contacts residing in congregate settings", Domain = "Contact", time_frame = "New")) %>%
  
  ## Attempted within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(attempted_24, na.rm=T),
                        percent = round(sum(attempted_24, na.rm=T) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Attempted to notify within 24 hr", Domain = "Contact", time_frame = "New")) %>%
  
  ## Cases with potentially accurate phone numbers
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(pot_accurate_num),
                        percent = round(sum(pot_accurate_num) /
                                          sum(presumed_positive, asymptomatic_contact) * 100, 1)) %>%
              mutate(metrics = "Contacts with potentially accurate phone numbers", Domain = "Contact", time_frame = "New")) %>%
  
  ## Reached
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(reached_initial),
                        percent = round(sum(reached_initial) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Reached for initial investigation", Domain = "Contact", time_frame = "New")) %>%
  
  ## Completed intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(complete_intake),
                        percent = round(sum(complete_intake) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Completed intake", Domain = "Contact", time_frame = "New")) %>%
  
  ## Interviewed within 24hr
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(interview_24hr, na.rm=T),
                        percent = round(sum(interview_24hr, na.rm=T) /
                                          sum(complete_intake) * 100, 1)) %>%
              mutate(metrics = "Interviewed within 24 hr", Domain = "Contact", time_frame = "New")) %>%
  
  ## Did not complete intake
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(pot_accurate_num) - sum(complete_intake),
                        percent = round((sum(pot_accurate_num) - sum(complete_intake)) /
                                          sum(pot_accurate_num) * 100, 1)) %>%
              mutate(metrics = "Did not complete intake", Domain = "Contact", time_frame = "New")) %>%
  
  ## Intake in progress
  bind_rows(base %>% filter(recordtypeid == "Contact Intake", new == 1) %>%
              summarize(number = sum(in_progress),
                        percent = round(sum(in_progress) /
                                          (sum(pot_accurate_num) - sum(complete_intake)) * 100, 1)) %>%
              mutate(metrics = "Interview in progress", Domain = "Contact", time_frame = "New")) %>%
  
  filter(!is.na(metrics), number != 0)

# Table 1 and 2 combined metrics------------------------------------------------------------------------------------------------

## Long format of all metrics 
combined_all_cases_contacts_long <- bind_rows(
  
  overall, window, new

) 

## Order of metrics for wide table 
metrics_order<-c(paste0(unique(cases_contacts$case_contact),"s<sup>1</sup>"),
                 "Asymptomatic contact", "Referred from DOHMH", "Symptomatic contact",
                 "Bulk upload", "Named contact",
                 paste0(unique(cases_contacts$case_contact),"s residing outside of NYC"),
                 paste0(unique(cases_contacts$case_contact),"s residing in NYC"), 
                 paste0(unique(cases_contacts$case_contact),"s residing in congregate settings"), 
                 "Attempted to notify within 24 hr<sup>4</sup>",
                 paste0(unique(cases_contacts$case_contact),"s with potentially accurate phone numbers<sup>5</sup>"), 
                 "Reached for initial investigation<sup>6</sup>",
                 "Completed intake<sup>7</sup>", "Interviewed within 24 hr<sup>8</sup>", 
                 "Unreached after investigation<sup>9</sup>", "Completed daily monitoring<sup>10</sup>",
                 "Did not complete intake", "Interview in progress<sup>11</sup>", 
                 "No close contacts", "1-9 close contacts", "10+ close contacts", 
                 "Mean # contacts elicited")


## Wide combined table 
combined_all_cases_contacts_wide<-combined_all_cases_contacts_long %>%
  # select(-Topic) %>% 
  
  ## Change numbers that are 0 to NA 
  mutate(across(c("number"),~na_if(.x,0))) %>% 
  
  ## Round percentages to 1 decimal place
  mutate(percent = ifelse(is.na(percent),NA, sprintf("%.1f",round(percent)))) %>%
  
  ## Add `%` sign to percent 
  mutate(percent = ifelse(is.na(percent), NA, paste0(percent,"%"))) %>% 
  
  ## Round and comma separate numbers, but retain 2 decimal places for mean and median elicited contacts
  mutate(number = round(number, 2),
         number = ifelse(is.na(number), NA, format(number, big.mark=",", scientific=FALSE)),
         number = str_remove(number, "\\.00")
         ) %>% 
  
  ## Add superscripts to metrics for footnotes (use <sup></sup> html tag with number in between)
  mutate(metrics = case_when(
    metrics %in% c("Cases","Contacts") ~ paste0(metrics,"<sup>1</sup>"),
    metrics == "Attempted to notify within 24 hr" ~ paste0(metrics, "<sup>4</sup>"),
    str_detect(metrics, "potentially") ~ paste0(metrics, "<sup>5</sup>"),
    metrics == "Reached for initial investigation" ~ paste0(metrics, "<sup>6</sup>"),
    metrics == "Completed intake" ~ paste0(metrics, "<sup>7</sup>"),
    metrics == "Interviewed within 24 hr" ~ paste0(metrics, "<sup>8</sup>"),
    metrics == "Unreached after investigation" ~ paste0(metrics, "<sup>9</sup>"),
    metrics == "Completed daily monitoring" ~ paste0(metrics,"<sup>10</sup>"),
    metrics == "Interview in progress" ~ paste0(metrics,"<sup>11</sup>"),
    TRUE ~ metrics)) %>% 
  
  ## Split here by Cases/Contacts 
  group_by(Domain) %>% 
  
  ## Spread the time_frame (New, Currently Monitoring, Cumulative) by number and percent 
  group_map( ~.x %>% 
               multi_spread(time_frame,c("number","percent")) %>% 
               
               ## Rearrange order of columns 
               select(metrics,starts_with("New"),starts_with("In"),starts_with("Overall")) %>%
               
               ## Replace all NAs with "" (blank)
               mutate(across(where(is.character),~replace_na(.x,"")) %>%
                        
                        ## Arrange metric order by metric order vector above 
                        arrange(match(metrics, metrics_order)))) %>%
  
  ## Set names to list elements 
  setNames(c("Case","Contact"))      


## Create kable table ---------------------------------------------------------------------------------------------------------
## This function applies for both case/contact tables 
cases_contact_kable_fn<-function(tables){
  tables<-tables
  table_name_quo<-enquo(tables)
  
  ## Call in the specified list data frame from the wide table 
  combined_all_cases_contacts_wide[[tables]] %>% 
    kable (
      
      ## Column names to appear on table 
      col.names=c("", "n","%","n","%","n","%"),
      format="html",
      
      ## Escape = F will allow HTML format of metric superscripts to be read in as HTML tags 
      escape=F,
      
      ## Padding 
      pad = 7,
      
      ## Align first column left, the number and % columns right 
      align = c("l",rep("r",6)),
      
      ## By default, table should have black borders 
      table.attr = "style=\"color:black;\""
    ) %>%
    kable_styling("bordered",font_size=15, position="center") %>%
    
    ## First column with metric names specification
    #column_spec(1, include_thead=T, 
                #border_right="1px solid black", 
                #border_left="1px solid black", 
    #            width="15em") %>% 
    
    ## Specification of remaining columns 
    #column_spec(c(2:ncol(combined_all_cases_contacts_wide[[tables]])),
    #            include_thead=T , 
    #            width="3em", 
                #border_right= "1px solid black", 
                #border_left= "1px solid black"
    #            ) %>% 
    
    ## First row: Cases or Contacts total 
    row_spec(which(combined_all_cases_contacts_wide[[tables]]$metrics==paste0(tables,"s<sup>1</sup>")), bold=T) %>%
    
    ## Only for contacts: the location-based contacts section
    { if (tables=="Contact") pack_rows(., "Location-based Contacts<sup>12</sup>", which(combined_all_cases_contacts_wide[[tables]]$metrics=="Bulk upload"), 
                                       which(combined_all_cases_contacts_wide[[tables]]$metrics=="Named contact"), bold=T,indent=F,
                                       label_row_css="border:1px solid black", escape=F) 
      else .} %>%
    
    ## Pack residing in/outside of residency into row group 
    pack_rows("Residency Status", which(combined_all_cases_contacts_wide[[tables]]$metrics==paste0(tables,"s residing outside of NYC")), 
              which(combined_all_cases_contacts_wide[[tables]]$metrics==paste0(tables,"s residing in NYC")), bold=T,indent=F,
              label_row_css="border:1px solid black") %>%
    
    pack_rows("Monitoring Status<sup>3</sup>", 
              which(combined_all_cases_contacts_wide[[tables]]$metrics=="Attempted to notify within 24 hr<sup>4</sup>"),
              str_which(combined_all_cases_contacts_wide[[tables]]$metrics,"Interview in progress<sup>11</sup>"),
              bold=T, indent=F, 
              label_row_css="border: 1px solid black", escape=F) %>%
    
    ## Only for cases: the contact elicitation section
              { if (tables=="Case") pack_rows(., "Contact Elicitation<sup>12</sup>", which(combined_all_cases_contacts_wide[[tables]]$metrics=="No close contacts"), 
                                              which(combined_all_cases_contacts_wide[[tables]]$metrics=="Mean # contacts elicited"), bold=T,indent=F,
                                              label_row_css="border:1px solid black", escape=F) 
                else .} %>%
    
    ## Add 1 indent to completed daily monitoring, started, did not start 
    add_indent(c(str_which(combined_all_cases_contacts_wide[[tables]]$metrics,"congregate settings"),
                 which(combined_all_cases_contacts_wide[[tables]]$metrics=="Reached for initial investigation<sup>6</sup>"),
                 which(combined_all_cases_contacts_wide[[tables]]$metrics=="Completed intake<sup>7</sup>"),
                 str_which(combined_all_cases_contacts_wide[[tables]]$metrics,"Did not complete intake")
    )) %>% 
    
    ## Add 2 indents to did not start intake metrics (each indent moves metric name by 2em)
    add_indent(c(which(combined_all_cases_contacts_wide[[tables]]$metrics=="Interviewed within 24 hr<sup>8</sup>"),
                 which(combined_all_cases_contacts_wide[[tables]]$metrics=="Unreached after investigation<sup>9</sup>"),
                 which(combined_all_cases_contacts_wide[[tables]]$metrics=="Completed daily monitoring<sup>10</sup>"),
                 str_which(combined_all_cases_contacts_wide[[tables]]$metrics,"in progress")
    ), level_of_indent=2) %>% 
    
    ## Specify header names above the columns 
    add_header_above(c("Metrics"=1,
                       "New"= 2,
                       "In Monitoring Window<sup>2</sup>"= 2,
                       "Overall" = 2),
                     line=F,
                     color="#000000",
                     
                     ## Header color 
                     background="#D9E2F3",
                     bold=T,
                     extra_css="border-left: 1px solid black;
                     border-right: 1px solid black;
                     border-top: 1px solid black;
                     border-bottom: 1px solid black;
                     height:50px",
                     escape=F
    ) %>%
    
    ## For the row with column names, add borders and background color 
    row_spec(0, extra_css=     "border-left: 1px solid black;
             border-right: 1px solid black;
             border-top: 1px solid black;
             border-bottom: 1px solid black;
             ",
             background="#D9E2F3",
             color="#000000") %>%
    
    ## For remainder rows (1 to the rest of each table), add borders 
    row_spec(1:nrow(combined_all_cases_contacts_wide[[tables]]), extra_css="border-left: 1px solid black;
                              border-right: 1px solid black;
                              border-top: 1px solid black;
                              border-bottom: 1px solid black",
             color="#000000") 
}


## Map kable function for case/contacts defined in the combined wide table. 
## Objects will output in global environment 
c("Case","Contact") %>% 
  map(cases_contact_kable_fn) %>%
  setNames(c("cases_table", "contacts_table")) %>%
  list2env(c("cases_table","contacts_table"), envir=.GlobalEnv)
















