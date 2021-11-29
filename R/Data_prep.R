library(tidyverse)

# Load data
df1 <- read.csv("Data/pv_61_20n_data.csv", sep = ";")

# rename variables
df1 <- df1 %>% rename("pb2020" = "ph_blei",
               "pb1988" = "bl_blei",
               "sex" = "pcsex",
               "age_31DEC2018" = "ptalter",
               "age_at_survey" = "ptalteru",
               "yrs_training" = "ptedyrs",     
               "highest_qual" = "ptberufb",  
               "school_qual" = "ptschul",
               "educ_level" = "ptbild",
               "prof_status" = "ptbersoz",  
               "house_income_euro" = "pthhnet",
               "income_helmert" = "ptgeld",
               "soc_class_helmert" = "ptses",
               "soc_class_helmert_cat" = "ptses5",
               "weight_kg" = "ptgewi",
               "height_cm" = "ptgroe",
               "bmi" = "ptbmi",
               "bmi_whocat" = "ptbmiwho",
               "physact_cat" = "ptphact",
               "smok_cat3" = "ptcigsmk",      
               "smok_cat4" = "ptcigreg",      
               "smok_cat5" = "ptsmkreg",      
               "etoh_g_day" = "ptalkkon",
               "etoh_cat" = "ptalkcat",
               "spirits_g_day" = "ptspirit",
               "wine_g_day" = "ptwein",
               "beer_g_day" = "ptbier",
               "health_ins" ="pk01",
               "pit020g" = "pit020g",     
               "level_care" = "pit021g",
               "monthly_net_income" = "pa1_3",   
               "currentwork_status" = "pbr01",
               "prof_cat" = "pc022ax",        
               "prof_skill" = "pc022bx",      
               "employee_type" = "pc022cx",
               "official_level" = "pc022dx",
               "self_employ" = "pc022ex",
               "currphysical_cond" = "pc033",
               "rate_health" = "pc033a",
               "consume_fish" = "po3f1",
               "fish_last6months" = "po3f1a",
               "fishoil" = "po3f2",
               "fishoil_last6months" = "po3f2a",
               "fishoil_duration" = "po3f2b",
               "exam_date" = "pcuntdat",
               "zz_nr" = "zz_nr"   
               )

# Format variables
df1$sex <- as.factor( ifelse(df1$sex ==1, "Male", "Female") )

df1$age_cat <- factor(case_when(df1$age_at_survey >=50 & df1$age_at_survey < 55 ~ "50 to 54yrs",
                                df1$age_at_survey >=55 & df1$age_at_survey < 60 ~ "55 to 59yrs",
                                df1$age_at_survey >=60 & df1$age_at_survey < 65 ~ "60 to 64yrs",
                                df1$age_at_survey >=65 & df1$age_at_survey < 70 ~ "65 to 69yrs",
                                df1$age_at_survey >=70 & df1$age_at_survey < 75 ~ "70 to 74yrs"),
                      levels = c("50 to 54yrs", "55 to 59yrs", "60 to 64yrs",
                                 "65 to 69yrs", "70 to 74yrs"))

df1$highest_qual <- factor( case_when(df1$highest_qual == 1 ~ "no degree",
                              df1$highest_qual == 2 ~ "vocational school / apprenticeship",
                              df1$highest_qual == 3 ~ "technical school / technician / master school",
                              df1$highest_qual == 4 ~ "engineering school / polytechnic",
                              df1$highest_qual == 5 ~ "university of applied sciences / university"),
                            levels = c("no degree", "vocational school / apprenticeship",
                                       "technical school / technician / master school",
                                       "engineering school / polytechnic",
                                       "university of applied sciences / university"))

df1$school_qual <- factor( case_when(df1$school_qual == 1 ~ "secondary school / elementary school",
                             df1$school_qual == 2 ~ "middle maturity / junior high school",
                             df1$school_qual == 3 ~ "Abitur / Fachabitur / Fachhochschulreife"),
                           levels = c("secondary school / elementary school", 
                                      "middle school / junior high school",
                                      "Abitur / Fachabitur / Fachhochschulreife"))
# school level is max os school level education
# educ level is max of school + college degree
df1$educ_level <- factor(case_when(df1$educ_level == 0 ~ "Did not finish",
                                   df1$educ_level == 2 ~ "Secondary school",
                                   df1$educ_level == 3 ~ "Vocational school / apprenticeship",
                                   df1$educ_level == 4 ~ "Secondary school / Middle school",
                                   df1$educ_level == 5 ~ "Technical college / technician / master",
                                   df1$educ_level == 6 ~ "Abitur",
                                   df1$educ_level == 9 ~ "University"),
                         levels = c("Did not finish", "Secondary school",
                                    "Vocational school / apprenticeship",
                                    "Secondary school / Middle school",
                                    "Technical college / technician / master",
                                    "Abitur", "University"))

df1$prof_status <- factor( case_when(df1$prof_status == 1 ~ "unskilled worker",
                                     df1$prof_status == 2 ~ "civil servants / semi-skilled workers",
                                     df1$prof_status == 3 ~ "started simple job, skilled worker",
                                     df1$prof_status == 4 ~ "middle-class civil servants / skilled workers or foremen",
                                     df1$prof_status == 5 ~ "master / foreman",
                                     df1$prof_status == 6 ~ "Senior civil servants / employees qualified activity",
                                     df1$prof_status == 7 ~ "self-employed, max. 1 employee",
                                     df1$prof_status == 8 ~ "senior civil servants / employees high quality Activity / self. Academics / self-employed with max. 9 employees",
                                     df1$prof_status == 9 ~ "started extensive management tasks / self-employed with at least 10 employees" ),
                                   levels = c("unskilled worker",
                                              "civil servants / semi-skilled workers",
                                              "started simple job, skilled worker",
                                              "middle-class civil servants / skilled workers or foremen",
                                              "master / foreman",
                                              "Senior civil servants / employees qualified activity",
                                              "self-employed, max. 1 employee",
                                              "senior civil servants / employees high quality Activity / self. Academics / self-employed with max. 9 employees",
                                              "started extensive management tasks / self-employed with at least 10 employees"))

# df1$house_income_euro - leaving as numeric

df1$income_helmert <- factor( case_when(df1$income_helmert == 1 ~ "up to 875 EURO 50%",
                                     df1$income_helmert == 2 ~ "875 -1207.5 69%",
                                     df1$income_helmert == 3 ~ "1207.5 -1557.5 89%",
                                     df1$income_helmert == 4 ~ "1557.5 -1907.5 109%",
                                     df1$income_helmert == 5 ~ "1907.5-2257.5 129%",
                                     df1$income_helmert == 6 ~ "2257.5 -2607.5 149%",
                                     df1$income_helmert == 7 ~ "2607.5-2957.5 169%",
                                     df1$income_helmert == 8 ~ "2957.5 -3307.5 189%",
                                     df1$income_helmert == 9 ~ "over 3307.5 over 189%"),
                              levels = c("up to 875 EURO 50%", "875 -1207.5 69%",
                                         "1207.5 -1557.5 89%", "1557.5 -1907.5 109%",
                                         "1907.5-2257.5 129%", "2257.5 -2607.5 149%",
                                         "2607.5-2957.5 169%", "2957.5 -3307.5 189%",
                                         "over 3307.5 over 189%"))

# df1$soc_class_helmert - leaving as numeric

df1$soc_class_helmert_cat <- factor( case_when(df1$soc_class_helmert_cat == 1 ~ "1 to 10",
                                               df1$soc_class_helmert_cat == 2 ~ ">10 to 13",
                                               df1$soc_class_helmert_cat == 3 ~ ">13 to 15",
                                               df1$soc_class_helmert_cat == 4 ~ ">15 to 19",
                                               df1$soc_class_helmert_cat == 5 ~ ">19"),
                                     levels = c("1 to 10", ">10 to 13", ">13 to 15", ">15 to 19", ">19"))

#df1$weight_kg - leaving as continuous
#df1$height_cm  - leaving as continuous
#df1$bmi - leaving as continuous

df1$bmi_whocat <- factor( case_when(df1$bmi_whocat == 1 ~ "Underweight (< 18.5 kg/m2)",
                                    df1$bmi_whocat == 2 ~ "Normal weight (≥ 18.5 to <25 kg/m2)",
                                    df1$bmi_whocat == 3 ~ "Pre-obese (≥ 25 to < 30 kg/m2)",
                                    df1$bmi_whocat == 4 ~ "Obese, grade 1 (≥ 30 to <35 kg/m2)",
                                    df1$bmi_whocat == 5 ~ "Obese, grade 2 (≥ 35 to <40 kg/m2)",
                                    df1$bmi_whocat == 6 ~ "Obese, grade 3 (≥ 40 kg/m2)"),
                          levels = c("Underweight (< 18.5 kg/m2)", "Normal weight (≥ 18.5 to <2 5 kg/m2)",
                                     "Pre-obese (≥ 25 to < 30 kg/m2)", "Obese, grade 1 (≥ 30 to <35 kg/m2)",
                                     "Obese, grade 2 (≥ 35 to <40 kg/m2)", "Obese, grade 3 (≥ 40 kg/m2)" ))

df1$physact_cat <- factor(case_when(df1$physact_cat == 1 ~ "Regularly > 2 hrs/wk",
                                    df1$physact_cat == 2 ~ "Regularly ~ 1 hrs/wk",
                                    df1$physact_cat == 3 ~ "Irregularly ~ 2 hrs/wk",
                                    df1$physact_cat == 4 ~ "Almost none or none"),
                          levels = c("Regularly > 2 hrs/wk", "Regularly ~ 1 hrs/wk",
                                     "Irregularly ~ 2 hrs/wk", "Almost none or none"))

df1$smok_cat3 <- factor(case_when(df1$smok_cat3 == 1 ~ "Smoker",
                                  df1$smok_cat3 == 2 ~ "Ex-smoker",
                                  df1$smok_cat3 == 3 ~ "Never smoker"),
                        levels = c("Never smoker", "Ex-smoker", "Smoker"))

df1$smok_cat4 <- factor(case_when(df1$smok_cat4 == 1 ~ "Regular smoker",
                                  df1$smok_cat4 == 2 ~ "Irregular smoker",
                                  df1$smok_cat4 == 3 ~ "Ex-smoker",
                                  df1$smok_cat4 == 4 ~ "Never smoker"),
                        levels = c("Never smoker", "Ex-smoker", "Irregular smoker", "Regular smoker"))

df1$smok_cat5 <- factor(case_when(df1$smok_cat5 == 1 ~ "Regular smoker",
                                  df1$smok_cat5 == 2 ~ "Occasional smoker",
                                  df1$smok_cat5 == 3 ~ "Ex-smoker who smoked regularly",
                                  df1$smok_cat5 == 4 ~ "Ex-smoker who smoked occasionally",
                                  df1$smok_cat5 == 5 ~ "Never smoker"),
                        levels = c("Never smoker", "Ex-smoker who smoked occasionally",
                                   "Ex-smoker who smoked regularly", "Occasional smoker", "Regular smoker"))

#df1$etoh_g_day - leave as continuous

df1$etoh_cat <- factor(case_when(df1$etoh_cat ==1 ~ "No alcohol intake",
                                 df1$etoh_cat ==2 ~ "0 to <20g/day",
                                 df1$etoh_cat ==3 ~ "≥20 to <40g/day",
                                 df1$etoh_cat ==4 ~ "≥40 to <60g/day",
                                 df1$etoh_cat ==5 ~ "≥60 to <80g/day",
                                 df1$etoh_cat ==6 ~ "≥80g/day"),
                       levels = c("No alcohol intake", "0 to <20g/day", "≥20 to <40g/day",
                                  "≥40 to <60g/day", "≥60 to <80g/day", "≥80g/day"))

#df1$spirits_g_day - leave as continuous
#df1$wine_g_day - leave as continuous
#df1$beer_g_day - leave as continuous

df1$health_ins <- factor(case_when(df1$health_ins == 1 ~ "Statutory health insurance",
                                   df1$health_ins == 2 ~ "Private health insurance",
                                   df1$health_ins == 3 ~ "Other health insurance",
                                   df1$health_ins == 4 ~ "No health insurance",
                                   df1$health_ins == 5 ~ "Unknown"),
                         levels = c("Statutory health insurance", "Private health insurance",
                                    "Other health insurance", "No health insurance", "Unknown"))

df1$pit020g <- factor(case_when(df1$pit020g == 1 ~ "Yes",
                                df1$pit020g == 2 ~ "No",
                                df1$pit020g == 3 ~ "Don't know"),
                      levels = c("Yes", "No", "I don't know"))

df1$level_care <- factor(case_when(df1$level_care == 1 ~ "Grade I",
                                   df1$level_care == 2 ~ "Grade II",
                                   df1$level_care == 3 ~ "Grade III",
                                   df1$level_care == 4 ~ "Grade IV",
                                   df1$level_care == 5 ~ "Grade V",
                                   df1$level_care == 6 ~ "Don't know",
                                   df1$level_care == -999 ~ NA_character_),
                         levels = c("Grade I", "Grade II", "Grade III", "Grade IV", "Grade V",
                                    "I don't know"))

df1$monthly_net_income <- factor(case_when(df1$monthly_net_income == 1 ~ "< 500 EUR",
                                           df1$monthly_net_income == 2 ~ "500 to 749 EUR", 
                                           df1$monthly_net_income == 3 ~ "750 to 999 EUR", 
                                           df1$monthly_net_income == 4 ~ "1,000 to 1,499 EUR", 
                                           df1$monthly_net_income == 5 ~ "1,500 to 1,999 EUR", 
                                           df1$monthly_net_income == 6 ~ "2,000 to 2,499 EUR", 
                                           df1$monthly_net_income == 7 ~ "2,500 to 2,999 EUR", 
                                           df1$monthly_net_income == 8 ~ "3,000 to 3,499 EUR", 
                                           df1$monthly_net_income == 9 ~ "3,500 to 3,999 EUR", 
                                           df1$monthly_net_income == 10 ~ "4,000 to 4,499 EUR", 
                                           df1$monthly_net_income == 11 ~ "4,500 to 4,999 EUR",
                                           df1$monthly_net_income == 12 ~ "5,000 to 5,499 EUR",
                                           df1$monthly_net_income == 13 ~ "5,500 to 5,999 EUR",
                                           df1$monthly_net_income == 14 ~ "> 6,000 EUR",
                                           df1$monthly_net_income == 15 ~ "I don't know",
                                           df1$monthly_net_income == 16 ~ "Not specified"),
                                 levels = c("< 500 EUR", "500 to 749 EUR", "750 to 999 EUR",
                                            "1,000 to 1,499 EUR", "1,500 to 1,999 EUR",
                                            "2,000 to 2,499 EUR", "2,500 to 2,999 EUR",
                                            "3,000 to 3,499 EUR", "3,500 to 3,999 EUR",
                                            "4,000 to 4,499 EUR", "4,500 to 4,999 EUR",
                                            "5,000 to 5,499 EUR", "5,500 to 5,999 EUR",
                                            "> 6,000 EUR", "I don't know", "Not specified")
                                 )


df1$currentwork_status <- factor(case_when(df1$currentwork_status == 1 ~ "Pensioner",
                                           df1$currentwork_status == 2 ~ "Partial retirement (passive)",
                                           df1$currentwork_status == 3 ~ "Partial retirement (active)",
                                           df1$currentwork_status == 4 ~ "Employed",
                                           df1$currentwork_status == 5 ~ "Registered unemployed",
                                           df1$currentwork_status == 6 ~ "Other"),
                                 levels = c("Pensioner", "Partial retirement (passive)",
                                            "Partial retirement (active)", "Employed",
                                            "Registered unemployed", "Other"))

df1$prof_cat <- factor(case_when(df1$prof_cat == 1 ~ "Worker",
                                 df1$prof_cat == 2 ~ "Employee",
                                 df1$prof_cat == 3 ~ "Civil servant",
                                 df1$prof_cat == 4 ~ "Self-employed farmer",
                                 df1$prof_cat == 5 ~ "Other self-employed",
                                 df1$prof_cat == 6 ~ "Family member helping out",
                                 df1$prof_cat == 7 ~ "Something else",
                                 df1$prof_cat == -999 ~ NA_character_),
                       levels = c("Worker", "Employee", "Civil servant", "Self-employed farmer",
                                  "Other self-employed", "Family member helping out", "Something else"))


df1$prof_skill <- factor(case_when(df1$prof_skill == 1 ~ "Unskilled worker",
                                   df1$prof_skill == 2 ~ "Semi-skilled worker",
                                   df1$prof_skill == 3 ~ "Trained and Skilled worker",
                                   df1$prof_skill == 4 ~ "Foreman / Column leader",
                                   df1$prof_skill == 5 ~ "Master, foreman",
                                   df1$prof_skill == 9 ~ "Not specified",
                                   df1$prof_skill == -999 ~ NA_character_),
                         levels = c("Unskilled worker", "Semi-skilled worker",
                                    "Trained and Skilled worker", "Foreman / Column leader",
                                    "Master, foreman", "Not specified"))

df1$employee_type <- factor(case_when(df1$employee_type == 1 ~ "Industrial and foreman in employment",
                                      df1$employee_type == 2 ~ "Employee with simple activity",
                                      df1$employee_type == 3 ~ "Employee with qualified job",
                                      df1$employee_type == 4 ~ "Employee with highly qualified activity or management function",
                                      df1$employee_type == 5 ~ "Employee with comprehensive management responsibilities",
                                      df1$employee_type == 9 ~ "Not specified",
                                      df1$employee_type == -999 ~ NA_character_),
                            levels = c("Industrial and foreman in employment",
                                       "Employee with simple activity", "Employee with qualified job",
                                       "Employee with highly qualified activity or management function",
                                       "Employee with comprehensive management responsibilities",
                                       "Not specified"))

df1$official_level <- factor(case_when(df1$official_level == 1 ~ "In simple service",
                                       df1$official_level == 2 ~ "In the middle grade",
                                       df1$official_level == 3 ~ "In the upper grade of the civil service",
                                       df1$official_level == 4 ~ "In the higher service",
                                       df1$official_level == 9 ~ "Not specified",
                                       df1$official_level == 1-999 ~ NA_character_),
                             levels =c( "In simple service", "In the middle grade",
                                        "In the upper grade of the civil service",
                                        "In the higher service", "Not specified"))

df1$self_employ <- factor(case_when(df1$self_employ == 1 ~ "With 1 employee or alone",
                                    df1$self_employ == 2 ~ "With 2 to 9 employees",
                                    df1$self_employ == 3 ~ "With 10+ employees",
                                    df1$self_employ == 9 ~ "Not specified",
                                    df1$self_employ == -999 ~ NA_character_),
                          levels = c("With 1 employe or alone", "With 2 to 9 employees",
                                     "With 10+ employees", "Not specified"))

df1$currphysical_cond <- factor(case_when(df1$currphysical_cond == 1 ~ "Very good",
                                          df1$currphysical_cond == 2 ~ "Good",
                                          df1$currphysical_cond == 3 ~ "Less good",
                                          df1$currphysical_cond == 4 ~ "Bad"),
                                levels =c("Very good", "Good", "Less good", "Bad"))

df1$rate_health <- factor(case_when(df1$rate_health == 1 ~ "Better",
                                    df1$rate_health == 2 ~ "Worse",
                                    df1$rate_health == 3 ~ "The same",
                                    df1$rate_health == 4 ~ "I don't know"),
                          levels = c("Better", "Worse", "The same", "I don't know"))

df1$consume_fish <- factor(case_when(df1$consume_fish == 1 ~ "Yes",
                                     df1$consume_fish == 2 ~ "No"),
                           levels = c("No", "Yes"))

df1$fish_last6months <- factor(case_when(df1$fish_last6months == 1 ~ "Never",
                                         df1$fish_last6months == 2 ~ "≤ 1 serving per month",
                                         df1$fish_last6months == 3 ~ "2-3 servings per month",
                                         df1$fish_last6months == 4 ~ "1-2 servings per week",
                                         df1$fish_last6months == 5 ~ "3+ portions per week",
                                         df1$fish_last6months == -999 ~ NA_character_),
                               levels = c("Never", "≤ 1 serving per month", "2-3 servings per month",
                                          "1-2 servings per week", "3+ portions per week"))

df1$fishoil <- factor(case_when(df1$fishoil == 1 ~ "Yes",
                                df1$fishoil == 2 ~ "No"),
                      levels = c("No", "Yes"))
                                         
df1$fishoil_last6months <- factor(case_when(df1$fishoil_last6months == 1 ~ "Never",
                                            df1$fishoil_last6months == 2 ~ "≤ 1x per month",
                                            df1$fishoil_last6months == 3 ~ "2-3 times per month",
                                            df1$fishoil_last6months == 4 ~ "1-2 times per week",
                                            df1$fishoil_last6months == 5 ~ "3-4 times per week",
                                            df1$fishoil_last6months == 6 ~ "5-6 times per week",
                                            df1$fishoil_last6months == 7 ~ "1x daily",
                                            df1$fishoil_last6months == 8 ~ "≥ 2x daily",
                                            df1$fishoil_last6months == 9 ~ "I don't know",
                                            df1$fishoil_last6months == -999 ~ NA_character_ ),
                                  levels = c("Never", "≤ 1x per month", "2-3 times per month",
                                             "1-2 times per week", "3-4 times per week",
                                             "5-6 times per week", "1x daily", "≥ 2x daily",
                                             "I don't know"))

df1$fishoil_duration <- factor(case_when(df1$fishoil_duration == 1 ~ "< 1 week",
                                         df1$fishoil_duration == 2 ~ "< 1 month",
                                         df1$fishoil_duration == 3 ~ "< 6 months",
                                         df1$fishoil_duration == 4 ~ "< 1 year",
                                         df1$fishoil_duration == 5 ~ "≥ 1 year",
                                         df1$fishoil_duration == 9 ~ "I don't know",
                                         df1$fishoil_duration == -999 ~ NA_character_ ),
                               levels = c("< 1 week", "< 1 month", "< 6 months",
                                          "< 1 year", "≥ 1 year", "I don't know"))

df1$exam_date <- as.Date(df1$exam_date, "%d/%m/%Y")
df1$exam_year <- as.numeric(format( df1$exam_date, "%Y"))
df1$exam_month <- as.numeric(format( df1$exam_date, "%m"))

df1$sampling_period <- factor(case_when(df1$exam_month %in% c(1, 2, 3) &
                                            df1$exam_year == 2018 ~ "January - March, 2018",
                                        df1$exam_month %in% c(1, 2, 3) &
                                            df1$exam_year == 2019 ~ "January - March, 2019",
                                 df1$exam_month %in% c(4, 5, 6) &
                                     df1$exam_year == 2018 ~ "April - June, 2018",
                                 df1$exam_month %in% c(4, 5, 6) &
                                     df1$exam_year == 2019 ~ "April - June, 2019",
                                 df1$exam_month %in% c(7, 8, 9) &
                                     df1$exam_year == 2018 ~ "July - September, 2018",
                                 df1$exam_month %in% c(10, 11, 12) &
                                     df1$exam_year == 2018 ~ "October - December, 2018"),
                              levels = c("January - March, 2018", "April - June, 2018",
                                         "July - September, 2018", "October - December, 2018",
                                         "January - March, 2019", "April - June, 2019"))

# Remove redundant variables
df1$prof_skill <- NULL
df1$prof_status <- NULL   #prof_cat has much the same information but less missings
df1$employee_type <- NULL # 
df1$official_level <- NULL # 
df1$self_employ <- NULL # 

# Save processed data
saveRDS(df1, "Data/KORA_data_formatted.RDS")


