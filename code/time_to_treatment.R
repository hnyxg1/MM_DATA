library(dplyr)
library(lubridate)



# Calculate time to treatment for each treatment type
time <- mm_data_cleaned %>%
  mutate(
    # Convert reference date to Date format (adjust format if needed)
    Date_of_Initial_Diagnosis = as.Date(`Date of Initial Diagnosis`, format = "%Y-%m-%d"),
    
    # Convert treatment dates to Date format
    Date_Chemo = as.Date(`Date of First Chemotherapy (This Course)`, format = "%Y-%m-%d"),
    Date_Radiation = as.Date(`Date Radiation Started`, format = "%Y-%m-%d"),
    Date_Hormone = as.Date(`Date of First Hormone Therapy`, format = "%Y-%m-%d"),
    Date_Immuno = as.Date(`Date of First Immunotherapy`, format = "%Y-%m-%d"),
    Date_Transplant = as.Date(`Date of Hematologic Transplant/Endocrine Procedure`, format = "%Y-%m-%d"),
    
    # Calculate time to treatment in days, only if the corresponding flag is 1
    Time_to_Chemo = ifelse(!is.na(Date_Chemo) & !is.na(Date_of_Initial_Diagnosis),
                           as.numeric(Date_Chemo - Date_of_Initial_Diagnosis),
                           NA),
    Time_to_Radiation = ifelse( !is.na(Date_Radiation) & !is.na(Date_of_Initial_Diagnosis),
                               as.numeric(Date_Radiation - Date_of_Initial_Diagnosis),
                               NA),
    Time_to_Hormone = ifelse(!is.na(Date_Hormone) & !is.na(Date_of_Initial_Diagnosis),
                             as.numeric(Date_Hormone - Date_of_Initial_Diagnosis),
                             NA),
    Time_to_Immuno = ifelse(!is.na(Date_Immuno) & !is.na(Date_of_Initial_Diagnosis),
                            as.numeric(Date_Immuno - Date_of_Initial_Diagnosis),
                            NA),
    Time_to_Transplant = ifelse(!is.na(Date_Transplant) & !is.na(Date_of_Initial_Diagnosis),
                                as.numeric(Date_Transplant - Date_of_Initial_Diagnosis),
                                NA)
  ) %>% 
  select(Time_to_Chemo,
         Time_to_Radiation,
         Time_to_Hormone,
         Time_to_Immuno,
         Time_to_Transplant
         )

# View the first few rows of the result
head(mm_data_cleaned)