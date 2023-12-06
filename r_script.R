### R version 4.3.1
### Rong Huang
### Dec 5, 2023
#---------------------
library(tidyverse)
library(boot)
### load data tables
setwd("/Users/rong/Desktop/Career/Graticule")
# specify column classes
enc_classes = c("character", "Date", "logical", "logical")
patient_classes = c("character","character", "Date", "Date")
# read-in
enc_tbl = read.csv("encounter_table.csv", header = TRUE, colClasses = enc_classes) 
patient_tbl = read.csv("patient_table.csv", header = TRUE, colClasses = patient_classes)
# label factor vars
enc = enc_tbl
patient = 
  patient_tbl %>%
  mutate(gender = factor(gender, levels = c("M","F"), labels = c("Male","Female")))

### Compute median age at index date for the exposed cohort
merged_data = merge(patient, enc, by = "patientid") 
exposed = 
  merged_data %>%
  filter(exposure == TRUE) 
exposed$age_at_encounter =
  as.numeric(difftime(exposed$encounterdate, exposed$birthdate, units = "days"))/365.25
median_age = median(exposed$age_at_encounter, na.rm = TRUE)

# use bootstrap to find CI
set.seed(1230)
boot_res = boot(exposed$age_at_encounter, function(x,i) median(x[i]), R = 10000)
ci = boot.ci(boot_res, type = "bca")

## median: 54.82
## 95% ci: [49.10, 60.52]


### Compute aggregate time at risk for the exposed cohort, in person-days
study_start = as.Date("2022-07-01")
study_end = as.Date("2022-12-31")
merged_data_sliced = subset(merged_data, encounterdate >= study_start & encounterdate <= study_end)

# find next encounter date and days in between
data = merged_data_sliced %>%
  arrange(patientid, encounterdate) %>%
  group_by(patientid) %>%
  mutate(next_encounterdate = lead(encounterdate, default = study_end)) %>%
  mutate(days_in_between = as.numeric(difftime(next_encounterdate, encounterdate, units = "days")))

# find aggregate time at risk
tot_time_at_risk = sum(data[data$exposure == TRUE,]$days_in_between)
tot_time_at_risk  
## 942 person-days


### provide pseudo-code to select a 1:1 unexposed cohort
# iterate
for each individual in exposed:
# filter
potential_matches = data%>% 
  filter(gender == exposed_gender) %>% 
  filter(abs(dob - exposed_dob) <= max_age_diff) %>%
  filter(format(encounter_date, "%V") == format(index_date, "%V"))

# select match
if (nrow(potential_matches) > 0):
  matched_unexposed = potential_matches[1, ]

matched_pairs = rbind(matched_pairs, cbind(exposed_individual, matched_unexposed))

# remove the matched individual from the unexposed cohort
unexposed_cohort = unexposed_cohort[unexposed_cohort$Patient_ID != matched_unexposed$Patient_ID, ]
