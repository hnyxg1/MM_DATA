library(readxl)
mm_data <- read_excel("/Users/guxujun/Library/CloudStorage/OneDrive-JohnsHopkins/Book1.xlsx")

library(dplyr)
library(haven)
install.packages("labelled")
library(labelled)
colnames(Book1)


mm_data  %>%
  distinct(`Medical Record Number`) %>%
  count()

mm_data  %>%
  group_by("High Risk Cytogenetics") %>% 
  summarize(mean(Sex), mean())

table(mm_data$`High Risk Cytogenetics`, mm_data$`Race 1`)
table(mm_data$`High Risk Cytogenetics`, mm_data$`Marital Status at Diagnosis`)
table(mm_data$`High Risk Cytogenetics`, mm_data$`Primary Payer at Diagnosis`)



library(dplyr)
library(tidyr)
risky_table <- mm_data %>%
  # 1) Pivot the three “group” columns into a single pair of columns:
  pivot_longer(
    cols      = c(`Sex`,
                  `Race 1`,
                  `Marital Status at Diagnosis`,
                  `Primary Payer at Diagnosis`),
    names_to  = "GroupVar",
    values_to = "GroupValue"
  ) %>%
  count(`High Risk Cytogenetics`, GroupVar, GroupValue) %>%
  pivot_wider(
    names_from   = `High Risk Cytogenetics`,
    values_from  = n,
    values_fill  = 0
  )  %>%
  rename (`0 not identified` = `0 High-risk cytogenetics not identified/not present`,
          `1 present` = `1 High-risk cytogenetics present`,
          `7 test ordered but unavailable`=`7 Test ordered, results not in chart`,
          `9 unknown` = `9 Not documented in medical record\r\nHigh Risk Cytogenetics not assessed or unknown if assessed`)


table(mm_data$`High Risk Cytogenetics`)
sum(is.na(mm_data$`High Risk Cytogenetics`))
anyNA(mm_data$`High Risk Cytogenetics`)



LDH_table <- mm_data %>%
  pivot_longer(
    cols      = c(`Sex`,
                  `Race 1`,
                  `Marital Status at Diagnosis`,
                  `Primary Payer at Diagnosis`),
    names_to  = "GroupVar",
    values_to = "GroupValue"
  ) %>%
  count(`LDH (Lactate Dehydrogenase) Pretreatment Level`, GroupVar, GroupValue) %>%
  pivot_wider(
    names_from   = `LDH (Lactate Dehydrogenase) Pretreatment Level`,
    values_from  = n,
    values_fill  = 0
  )

Serum_table <- mm_data %>%
  pivot_longer(
    cols      = c(`Sex`,
                  `Race 1`,
                  `Marital Status at Diagnosis`,
                  `Primary Payer at Diagnosis`),
    names_to  = "GroupVar",
    values_to = "GroupValue"
  ) %>%
  count(`Serum Beta-2 Microglobulin Pretreatment Level`, GroupVar, GroupValue) %>%
  pivot_wider(
    names_from   = `Serum Beta-2 Microglobulin Pretreatment Level`,
    values_from  = n,
    values_fill  = 0
  )%>% 
  mutate( `0_pct`=`0 beta2-microglobulin < 3.5 mg/L`/137,
          `1_pct`=`1 beta2-microglobulin >= 3.5 mg/L < 5.5 mg/L`/61,
          `2_pct`=`2 beta2-microglobulin >= 5.5 mg/L`/70,
          `5_pct`=`5 Schema Discriminator 1: Plasma Cell Myeloma Terminology coded to 1 or 9`/33,
          `7_pct`=`7 Test ordered, results not in chart`/1,
          `9_pct`=`9 Not documented in medical record\r\nSerum Beta-2 Microglobulin Pretreatment Level not assessed or unknown if assessed`/106
          
  )

table(mm_data$`Accession Number`)
mm_data  %>%
  distinct(`Accession Number`) %>%
  count()

table(duplicated(mm_data$`Accession Number`))

write.csv(risky_table, "risky_table.csv", row.names = FALSE)
write.csv(LDH_table, "LDH_table.csv", row.names = FALSE)
write.csv(Serum_table, "Serum_table.csv", row.names = FALSE)

sample <- mm_data %>%
  group_by(`High Risk Cytogenetics`)%>%
  summarize(sex=n(Sex), pct_sex=n(sex)/)

  count(Sex) %>% 
  mutate(percentage = n / sum(n) * 100) 
  
  
#Stratified Dataset
library(gtsummary)
overall_table <- mm_data %>%
  tbl_summary(include = c(`Age at Diagnosis (Years)`,Sex,`Race 1`,`Marital Status at Diagnosis`, 
                          `Primary Payer at Diagnosis`, `High Risk Cytogenetics`,
                          `LDH (Lactate Dehydrogenase) Pretreatment Level`,
                          `Serum Beta-2 Microglobulin Pretreatment Level`),
              statistic=list(
                all_continuous() ~ "{mean} (SD={sd}) ({min}, {max})",
                all_categorical() ~"{n} /{N} ({p}%)"
              ),
              digits = all_categorical() ~ c(2,0),
              missing_text = "(Missing)") %>% 
  bold_labels() %>% 
  modify_header(label ~ "**Variable**") %>% 
  bold_labels() %>% 
  modify_caption("**Table XX. Overall Groups**")

print(overall_table)



cyto_table <- mm_data %>%
  tbl_summary(by = `High Risk Cytogenetics`,
              include = c(`Age at Diagnosis (Years)`, Sex,`Race 1`,`Marital Status at Diagnosis`, 
                          `Primary Payer at Diagnosis`),
              digits = all_categorical() ~ c(0, 2),
              missing_text = "(Missing)",
              statistic=list(
                all_continuous() ~ "{mean} ({sd})"))%>% 
  add_overall %>% 
  modify_header(label ~ "**Variable**") %>% 
  bold_labels() %>% 
  modify_caption("**Table XX. High Risk Cytogenetics**")

  
LDH_level_table <- mm_data %>%
  tbl_summary(by = `LDH (Lactate Dehydrogenase) Pretreatment Level`,
              include = c(`Age at Diagnosis (Years)`, Sex,`Race 1`,`Marital Status at Diagnosis`, 
                          `Primary Payer at Diagnosis`),
              digits = all_categorical() ~ c(0, 2),
              missing_text = "(Missing)",statistic=list(
                all_continuous() ~ "{mean} ({sd})"))%>% 
  add_overall %>% 
  modify_header(label ~ "**Variable**") %>% 
  bold_labels()%>% 
  modify_caption("**Table XX. LDH Pretreatment Level**")


Serum_table <- mm_data %>%
  tbl_summary(by = `Serum Beta-2 Microglobulin Pretreatment Level`,
              include = c(`Age at Diagnosis (Years)`,Sex,`Race 1`,`Marital Status at Diagnosis`, 
                          `Primary Payer at Diagnosis`),
              digits = all_categorical() ~ c(0, 2),
              missing_text = "(Missing)",
              statistic=list(
                all_continuous() ~ "{mean} ({sd})")) %>% 
  add_overall %>% 
  modify_header(label ~ "**Variable**") %>% 
  bold_labels()%>% 
  modify_caption("**Table XX. Serum Beta-2 Microglobulin Pretreatment Level**")

print(cyto_table)
print(LDH_level_table)
print(Serum_table)




# New cleaned dataset
mm_data_cleaned <- mm_data %>% 
  select(-`Race 2`:-`Race 5`)%>%
  mutate(across(contains("Date"), ~ as.Date(as.numeric(.x), origin = "1899-12-30"))) %>%
  mutate(across(
    contains("Date"),
    ~ as.integer(!is.na(.x)),
    .names = "{.col}_nonmissing"
  )) %>% 
  filter(!is.na(`Date of Initial Diagnosis`))%>%   #should we do that in this stage?
  mutate(Sex=recode(Sex, 
                    "01 - Male"=1, 
                    "02 - Female"=2),
         `Race 1`=recode(`Race 1`,
                         "01 - White"=1,
                         "02 - Black or African American" = 2,
                         "04 - Chinese" = 3,
                         "08 - Korean" = 3,
                         "10 - Vietnamese" =3,
                         "15 - Asian Indian, NOS or Pakistani, NOS" =3,
                         "16 -      Asian Indian" =3,
                         "96 - Other Asian, including Asian, NOS and Oriental, NOS" =3,
                         "98 - Some other race" = 4,
                         "99 - Unknown by patient" = NA_real_),
         `Marital Status 1`=recode(`Marital Status at Diagnosis`,
                                   "01 - Single (Never Married)" = 2,
                                   "02 - Married" = 1,
                                   "03 - Separated" = 2,
                                   "04 - Divorced" = 2,
                                   "05 - Widowed" = 2,
                                   "09 - Unknown" = NA_real_),
         `Marital Status 2`=recode(`Marital Status at Diagnosis`,
                                   "01 - Single (Never Married)" = 1,
                                   "02 - Married" = 2,
                                   "03 - Separated" = 3,
                                   "04 - Divorced" = 3,
                                   "05 - Widowed" = 4,
                                   "09 - Unknown" = NA_real_),
         `Insurance Type`=recode(`Primary Payer at Diagnosis`,
                                 "10 - Insurance, NOS" = NA_real_,
                                 "20 - Private Insurance: Managed Care/HMO/PPO" = 1,
                                 "21 - Private Insurance: Fee-for-Service" = 1,
                                 "31 - Medicaid" = 2,
                                 "35 - Medicaid: via Managed Care plan" = 2,
                                 "60 - Medicare w/o supp; Medicare, NOS" = 2,
                                 "61 - Medicare w/ supp, NOS" = 2,
                                 "62 - Medicare: via Managed Care plan" = 2,
                                 "63 - Medicare w/ private supp" = 2,
                                 "64 - Medicare w/ Medicaid eligibility" = 2,
                                 "65 - TRICARE" = 2,
                                 "67 - Veterans Affairs" = 2,
                                 "99 - Unknown" = NA_real_),
         `Primary Payer at Diagnosis`=recode(`Primary Payer at Diagnosis`,
                                             "10 - Insurance, NOS" = NA_real_,
                                             "20 - Private Insurance: Managed Care/HMO/PPO" = 1,
                                             "21 - Private Insurance: Fee-for-Service" = 1,
                                             "31 - Medicaid" = 2,
                                             "35 - Medicaid: via Managed Care plan" = 2,
                                             "60 - Medicare w/o supp; Medicare, NOS" = 3,
                                             "61 - Medicare w/ supp, NOS" = 3,
                                             "62 - Medicare: via Managed Care plan" = 3,
                                             "63 - Medicare w/ private supp" = 3,
                                             "64 - Medicare w/ Medicaid eligibility" = 3,
                                             "65 - TRICARE" = 4,
                                             "67 - Veterans Affairs" = 4,
                                             "99 - Unknown" = NA_real_),
         `Spanish Origin`=recode(`Spanish Origin`,
                                 "00 - Non-Spanish, Non-Hispanic" = 1,
                                 "02 - Puerto Rican" = 2,
                                 "04 - South Or Central American (Except Brazil)"=2,
                                 "05 - Other Specified Spanish Origin" =2,
                                 "06 - Spanish, NOS; Hispanic, NOS; Latino, NOS" = 2,
                                 "09 - Unknown Whether Spanish Or Not"=NA_real_
         )
  ) %>% 
  rename(Transplant="Date of Hematologic Transplant/Endocrine Procedure_nonmissing")


val_labels(mm_data_cleaned$Sex) <- c(Male= 1, Female = 2)
val_labels(mm_data_cleaned$`Race 1`) <- c(White = 1, Black = 2, Asian = 3, Others = 4)
val_labels(mm_data_cleaned$`Marital Status 2`) <- c(Single = 1, Married = 2, `Separated or Divorced`= 3, Widowed = 4)
val_labels(mm_data_cleaned$`Marital Status 1`) <- c(Married = 1, Others = 2)
val_labels(mm_data_cleaned$ `Primary Payer at Diagnosis`) <- c(Private = 1, Medicaid = 2, Medicare = 3, 
                                                               Tricare_VA = 4)
val_labels(mm_data_cleaned$`Insurance Type`) <- c(Private = 1, Public = 2)
val_labels(mm_data_cleaned$`Spanish Origin`) <- c(`Non-Hispanic`=1, Hispanic = 2)
val_labels(mm_data_cleaned$Transplant)<- c(Yes=1, No = 0)




date_check <- mm_data %>%
  mutate(`Date of Initial Diagnosis` = as.Date(as.numeric(`Date of Initial Diagnosis`), origin = "1899-12-30")) %>%
  select(`Enterprise ID`,`Date of Initial Diagnosis`)

print(date_check)

table(mm_data_cleaned$`Summary of Therapy`)


dup <- mm_data_cleaned %>% 
  filter(duplicated('Deidentified ID')|duplicated('Deidentified ID', fromLast=TRUE))

length(unique(mm_data_cleaned$'Deidentified ID'))
