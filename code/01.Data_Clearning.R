### Xujun Gu
### July 2025

# In this file:
# Clean the data and set up cohort


## Setup
library(gtsummary)
library(dplyr)
library(labelled)
library(haven)
library(readxl)

## load functions and data
source(paste0("code/00_project-setup.R"))

mm_data <- read_excel("Xujun_analytical_file.xlsx", 
                                    sheet = "2018-2022 Analytic Myeloma(new)", 
                                    col_types = c("numeric", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "numeric", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "text", "text", "text", 
                                                  "date", "date", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "date", "text", "text", "date", 
                                                  "text", "text", "text", "text", "text", 
                                                  "date", "text", "date", "text", "text", 
                                                  "text", "date", "date", "text", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "date", "date", "text", "text", 
                                                  "text", "numeric", "text", "numeric", 
                                                  "text", "numeric", "text", "numeric", 
                                                  "text", "text", "numeric", "text", 
                                                  "date", "text", "text", "date", "text", 
                                                  "text", "text", "date", "text", "text", 
                                                  "text", "date", "text", "text", "text", 
                                                  "text"))

# Step 1: Creating label and generateing new variable 
mm_data_labeled <- mm_data %>%
  # remove unused race columns
  select(-`Race 2`:-`Race 5`) %>%
  
  # derive clinical status variables used in staging
  mutate(
    Albumin_Status = case_when(
      `Serum Albumin Pretreatment Level` == "1 Serum albumin >=3.5 g/dL" ~ 1,
      `Serum Albumin Pretreatment Level` == "0 Serum albumin <3.5 g/dL"  ~ 0,
      TRUE ~ NA_real_
    ),
    B2M_Status = case_when(
      `Serum Beta-2 Microglobulin Pretreatment Level` ==
        "0 beta2-microglobulin < 3.5 mg/L"                                     ~ 0,
      `Serum Beta-2 Microglobulin Pretreatment Level` ==
        "1 beta2-microglobulin >= 3.5 mg/L < 5.5 mg/L"                         ~ 1,
      `Serum Beta-2 Microglobulin Pretreatment Level` ==
        "2 beta2-microglobulin >= 5.5 mg/L"                                    ~ 2,
      TRUE ~ NA_real_
    ),
    LDH_Status = case_when(
      `LDH (Lactate Dehydrogenase) Pretreatment Level` ==
        "0 Normal LDH level\r\nLow, below normal"                             ~ 0,
      `LDH (Lactate Dehydrogenase) Pretreatment Level` ==
        "1 Above normal LDH level; High"                                      ~ 1,
      TRUE ~ NA_real_
    ),
    High_Risk_Cytogenetics = case_when(
      `High Risk Cytogenetics` ==
        "0 High-risk cytogenetics not identified/not present"                 ~ 0,
      `High Risk Cytogenetics` ==
        "1 High-risk cytogenetics present"                                    ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Revised International Staging System (RISS)
    RISS = case_when(
      # Stage I: Albumin >=3.5, B2M<3.5, LDH normal, no high-risk cytogenetics
      Albumin_Status == 1 & B2M_Status == 0 & LDH_Status == 0 &
        High_Risk_Cytogenetics == 0                                            ~ "I",
      # Stage III: B2M >=5.5 and either high-risk cytogenetics or LDH elevated
      B2M_Status == 2 & (High_Risk_Cytogenetics == 1 | LDH_Status == 1)         ~ "III",
      # Stage II: all remaining complete cases
      !is.na(Albumin_Status) & !is.na(B2M_Status) & !is.na(LDH_Status) &
        !is.na(High_Risk_Cytogenetics)                                          ~ "II",
      TRUE                                                                      ~ NA_character_
    ),
    
    # International Staging System (ISS)
    ISS = case_when(
      # Stage I: B2M<3.5 mg/L and Albumin >=3.5 g/dL
      Albumin_Status == 1 & B2M_Status == 0                                     ~ "I",
      # Stage III: B2M >=5.5 mg/L
      B2M_Status == 2                                                            ~ "III",
      # Stage II: all remaining complete cases
      !is.na(Albumin_Status) & !is.na(B2M_Status)                                ~ "II",
      TRUE                                                                      ~ NA_character_
    )
  ) %>%
  
  # convert Excel date numbers to Date and create nonmissing flags
  # mutate(across(contains("Date"), ~ as.Date(as.numeric(.x), origin = "1899-12-30"))) %>%
  # mutate(across(
  #   .cols = contains("Date", ignore.case = TRUE),  
  #   .fns = ~ as.Date(., format = "%Y-%m-%d")      
  # )) %>% 
  mutate(across(contains("Date"),
                ~ as.integer(!is.na(.x)), .names = "{.col}_nonmissing")) %>%
  
  # rename nonmissing date flags to more concise names
  rename(
    Lymph_Node_Dissected_Flag   = `Date Regional Lymph Node Dissection_nonmissing`,
    Diagnostic_Flag             = `Date of Surgical Diagnostic and Staging Procedure_nonmissing`,
    Surgery1_Flag               = `Date of Surgery...83_nonmissing`,
    Surgery2_Flag               = `Date of Surgery...87_nonmissing`,
    Chemo_Flag                  = `Date of First Chemotherapy (This Course)_nonmissing`,
    Radiation_Flag              = `Date Radiation Started_nonmissing`,
    Hormone_Flag                = `Date of First Hormone Therapy_nonmissing`,
    Immuno_Flag                 = `Date of First Immunotherapy_nonmissing`,
    Other_Treatment_Flag        = `Date Other Treatment Started_nonmissing`,
    Transplant_Flag             = `Date of Hematologic Transplant/Endocrine Procedure_nonmissing`
  ) %>%
  
  # recode demographic and clinical categorical variables
  mutate(
    Sex = recode(Sex,
                 `01 - Male`   = "Male",
                 `02 - Female` = "Female"),
    
    `Race 1` = recode(`Race 1`,
                      `01 - White`                              = "White",
                      `02 - Black or African American`          = "Black",
                      `04 - Chinese`                            = "Asian",
                      `08 - Korean`                             = "Asian",
                      `10 - Vietnamese`                         = "Asian",
                      `15 - Asian Indian, NOS or Pakistani, NOS`= "Asian",
                      `16 -      Asian Indian`                  = "Asian",
                      `96 - Other Asian, including Asian, NOS and Oriental, NOS` = "Asian",
                      `98 - Some other race`                    = "Others",
                      `99 - Unknown by patient`                 = NA_character_),
    
    `Marital Status 1` = recode(`Marital Status at Diagnosis`,
                                `01 - Single (Never Married)` = "Non-Married",
                                `02 - Married`                = "Married",
                                `03 - Separated`              = "Non-Married",
                                `04 - Divorced`               = "Non-Married",
                                `05 - Widowed`                = "Non-Married",
                                `09 - Unknown`                = NA_character_),
    
    `Marital Status 2` = recode(`Marital Status at Diagnosis`,
                                `01 - Single (Never Married)` = "Single",
                                `02 - Married`                = "Married",
                                `03 - Separated`              = "Separated or Divorced",
                                `04 - Divorced`               = "Separated or Divorced",
                                `05 - Widowed`                = "Widowed",
                                `09 - Unknown`                = NA_character_),
    
    `Insurance Type` = recode(`Primary Payer at Diagnosis`,
                              `10 - Insurance, NOS`                  = NA_character_,
                              `20 - Private Insurance: Managed Care/HMO/PPO` = "Private",
                              `21 - Private Insurance: Fee-for-Service`      = "Private",
                              `31 - Medicaid`                               = "Public",
                              `35 - Medicaid: via Managed Care plan`        = "Public",
                              `60 - Medicare w/o supp; Medicare, NOS`       = "Public",
                              `61 - Medicare w/ supp, NOS`                  = "Public",
                              `62 - Medicare: via Managed Care plan`        = "Public",
                              `63 - Medicare w/ private supp`               = "Public",
                              `64 - Medicare w/ Medicaid eligibility`       = "Public",
                              `65 - TRICARE`                                = "Public",
                              `67 - Veterans Affairs`                       = "Public",
                              `99 - Unknown`                                = NA_character_),
    
    `Primary Payer at Diagnosis` = recode(`Primary Payer at Diagnosis`,
                                          `10 - Insurance, NOS`                  = NA_character_,
                                          `20 - Private Insurance: Managed Care/HMO/PPO` = "Private",
                                          `21 - Private Insurance: Fee-for-Service`      = "Private",
                                          `31 - Medicaid`                               = "Medicaid",
                                          `35 - Medicaid: via Managed Care plan`        = "Medicaid",
                                          `60 - Medicare w/o supp; Medicare, NOS`       = "Medicare",
                                          `61 - Medicare w/ supp, NOS`                  = "Medicare",
                                          `62 - Medicare: via Managed Care plan`        = "Medicare",
                                          `63 - Medicare w/ private supp`               = "Medicare",
                                          `64 - Medicare w/ Medicaid eligibility`       = "Medicare",
                                          `65 - TRICARE`                                = "Military",
                                          `67 - Veterans Affairs`                       = "Military",
                                          `99 - Unknown`                                = NA_character_),
    
    `Spanish Origin` = recode(`Spanish Origin`,
                              `00 - Non-Spanish, Non-Hispanic`         = "Non-Hispanic",
                              `02 - Puerto Rican`                       = "Hispanic",
                              `04 - South Or Central American (Except Brazil)` = "Hispanic",
                              `05 - Other Specified Spanish Origin`     = "Hispanic",
                              `06 - Spanish, NOS; Hispanic, NOS; Latino, NOS`  = "Hispanic",
                              `09 - Unknown Whether Spanish Or Not`     = NA_character_),
    
    `High Risk Cytogenetics` = recode(`High Risk Cytogenetics`,
                                      `0 High-risk cytogenetics not identified/not present` = "Not High Risk",
                                      `1 High-risk cytogenetics present`                    = "High Risk",
                                      `5 Schema Discriminator 1: Plasma Cell Myeloma Terminology coded to 1 or 9` = "Schema Discriminator 1",
                                      `7 Test ordered, results not in chart`                = NA_character_,
                                      `9 Not documented in medical record\r\nHigh Risk Cytogenetics not assessed or unknown if assessed` = NA_character_),
    
    `LDH (Lactate Dehydrogenase) Pretreatment Level` = recode(
      `LDH (Lactate Dehydrogenase) Pretreatment Level`,
      `0 Normal LDH level\r\nLow, below normal` = "Normal LDH",
      `1 Above normal LDH level; High` = "High LDH",
      `5 Schema Discriminator 1: Plasma Cell Myeloma Terminology coded to 1 or 9` = "Schema Discriminator 1",
      `7 Test ordered, results not in chart` = NA_character_,
      `9 Not documented in medical record \r\nLDH (Lactate Dehydrogenase) Level not assessed or unknown if assessed` = NA_character_
    ),
    
    `Serum Beta-2 Microglobulin Pretreatment Level` = recode(
      `Serum Beta-2 Microglobulin Pretreatment Level`,
      `0 beta2-microglobulin < 3.5 mg/L`                 = "< 3.5 mg/L",
      `1 beta2-microglobulin >= 3.5 mg/L < 5.5 mg/L`     = ">= 3.5 mg/L < 5.5 mg/L",
      `2 beta2-microglobulin >= 5.5 mg/L`                = ">= 5.5 mg/L",
      `5 Schema Discriminator 1: Plasma Cell Myeloma Terminology coded to 1 or 9` = "Schema Discriminator 1",
      `7 Test ordered, results not in chart`             = NA_character_,
      `9 Not documented in medical record\r\nSerum Beta-2 Microglobulin Pretreatment Level not assessed or unknown if assessed` = NA_character_
    ),
    
    # recode flags from numeric to yes/no strings
    Lymph_Node_Dissected_Flag = recode(as.character(Lymph_Node_Dissected_Flag),
                                       `1` = "Yes", `0` = "No"),
    Diagnostic_Flag          = recode(as.character(Diagnostic_Flag),          `1` = "Yes", `0` = "No"),
    Surgery1_Flag            = recode(as.character(Surgery1_Flag),            `1` = "Yes", `0` = "No"),
    Surgery2_Flag            = recode(as.character(Surgery2_Flag),            `1` = "Yes", `0` = "No"),
    Chemo_Flag               = recode(as.character(Chemo_Flag),               `1` = "Yes", `0` = "No"),
    Radiation_Flag           = recode(as.character(Radiation_Flag),           `1` = "Yes", `0` = "No"),
    Hormone_Flag             = recode(as.character(Hormone_Flag),             `1` = "Yes", `0` = "No"),
    Immuno_Flag              = recode(as.character(Immuno_Flag),              `1` = "Yes", `0` = "No"),
    Other_Treatment_Flag     = recode(as.character(Other_Treatment_Flag),     `1` = "Yes", `0` = "No"),
    Transplant_Flag          = recode(as.character(Transplant_Flag),          `1` = "Yes", `0` = "No")
  ) %>%
  
  # derive composite treatment indicator: yes if any therapy flag is yes
  mutate(
    Treatment = case_when(
      if_any(c(Lymph_Node_Dissected_Flag, Diagnostic_Flag, Surgery1_Flag,
               Surgery2_Flag, Chemo_Flag, Radiation_Flag, Hormone_Flag,
               Immuno_Flag, Other_Treatment_Flag, Transplant_Flag),
             ~ .x == "Yes") ~ "Yes",
      TRUE ~ "No"
    ),
    # age categories based on diagnosis age
    age_cat = case_when(
      `Age at Diagnosis (Years)` < 65 ~ "Age Under 65",
      `Age at Diagnosis (Years)` < 75 ~ "Age 65 to 74",
      `Age at Diagnosis (Years)` >= 75 ~ "Age Over 75",
      TRUE ~ NA_character_
    )
  )





library(tidylog)             # masks dplyr verbs with logging versions


#Step 2: Cohort building
mm_data_cleaned <- mm_data_labeled %>%
  
  # require a diagnosis date
  filter(!is.na(`Date of Initial Diagnosis`)) %>%
  
  # exclude observations where all three of these fields equal the schema discriminator
  filter(
    `High Risk Cytogenetics` != "Schema Discriminator 1" & 
      `LDH (Lactate Dehydrogenase) Pretreatment Level` != "Schema Discriminator 1" & 
      `Serum Beta-2 Microglobulin Pretreatment Level` != "Schema Discriminator 1" 
  ) %>%
  
  # restrict to Black or White race
  filter(`Race 1` %in% c("Black", "White")) %>%
  
  # remove smoldering histology
  filter(!grepl("SMOLDERING", `Histology Description`))%>%
  
  select("Deidentified ID",
         "Postal Code - Current",
         "County - Current",
         Sex,
         "Race 1",
         "Spanish Origin",
         "Vital Status",
         "Underlying Cause of Death",
         "Family Cancer History",
         "Primary Site Description",
         "Histology Description",
         "Age at Diagnosis (Years)",
         "Marital Status at Diagnosis",
         "Primary Payer at Diagnosis",
         "Date of Initial Diagnosis",
         Albumin_Status,
         B2M_Status,
         LDH_Status,
         High_Risk_Cytogenetics,
         RISS,
         ISS,
         Lymph_Node_Dissected_Flag,
         Diagnostic_Flag,
         Surgery1_Flag,
         Surgery2_Flag,
         Chemo_Flag,
         Radiation_Flag,
         Hormone_Flag,
         Immuno_Flag,
         Other_Treatment_Flag,
         Transplant_Flag,
         "Marital Status 1",
         "Marital Status 2",
         "Insurance Type",
         Treatment,
         age_cat,
         "High Risk Cytogenetics",
         "Serum Albumin Pretreatment Level",
         "Serum Beta-2 Microglobulin Pretreatment Level",
         "LDH (Lactate Dehydrogenase) Pretreatment Level",
         "Date Regional Lymph Node Dissection",
         "Date of Surgery...83",
         "Date of Surgery...87",
         "Date of First Chemotherapy (This Course)",
         "Date Radiation Started",
         "Date of First Hormone Therapy",
         "Date of First Immunotherapy",
         "Date Other Treatment Started",
         "Date of Hematologic Transplant/Endocrine Procedure"
  ) 


