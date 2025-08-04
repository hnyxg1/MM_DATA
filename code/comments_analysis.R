
library(readxl)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(topicmodels)
library(tidytext)
library(udpipe)
library(spacyr)
library(dplyr)
raw_data <- read_excel("/Users/guxujun/Library/CloudStorage/Dropbox/UMB/Xujun_analytical_file.xlsx")


data <- raw_data %>% 
  select("Deidentified ID", contains("comment"))

# Install required packages if not already installed
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(stringr)) install.packages("stringr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(writexl)) install.packages("writexl")
if (!require(corrplot)) install.packages("corrplot")

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(writexl)
library(corrplot)

# Read the Excel file (replace with actual file path)
data <- raw_data %>% 
  select("Deidentified ID", contains("comment"))

# Initialize new columns for extracted variables
data <- data %>%
  mutate(
    Medications = NA_character_,
    Treatments = NA_character_,
    Disease_Characteristics = NA_character_,
    Age = NA_real_,
    Sex = NA_character_,
    Race_Ethnicity = NA_character_,
    Social_History = NA_character_,
    Clinical_Outcomes = NA_character_,
    Diagnostic_Procedures = NA_character_,
    Healthcare_Interactions = NA_character_,
    Cytogenetic_Risk = NA_character_
  )

# Initialize columns for specific lab tests
lab_tests <- c("Albumin", "Beta-2 Microglobulin", "LDH", "Hemoglobin", "Creatinine", 
               "Kappa Light Chain", "Lambda Light Chain", "M Spike", "IgG", "IgA", "IgM")
for (test in lab_tests) {
  data[[paste0(test, "_Value")]] <- NA_real_
  data[[paste0(test, "_Status")]] <- NA_character_
}

# Define keyword lists
medications_list <- c("Velcade", "Bortezomib", "Revlimid", "Lenalidomide", "Dexamethasone", 
                      "Cytoxan", "Cyclophosphamide", "Melphalan", "Daratumumab", 
                      "Carfilzomib", "Prednisone", "Darzalex")
treatments_list <- c("Chemotherapy", "Radiation", "Stem Cell Transplant", "ASCT", 
                     "Hormone Therapy", "Immunotherapy", "Palliative", "Surgery")
disease_keywords <- c("IgG", "IgA", "Kappa", "Lambda", "R-ISS", "Stage I", "Stage II", 
                      "Stage III", "Standard Risk", "High Risk", "Smoldering Myeloma", 
                      "Plasmacytoma", "Multiple Myeloma")
social_history_keywords <- c("TOB", "ETOH", "FAM HX", "SMOKER", "ALCOHOL")
clinical_outcomes_keywords <- c("Remission", "CR", "VGPR", "Progressive Disease", "Expired", "Deceased")
diagnostic_procedures_keywords <- c("Biopsy", "Bone Marrow BX", "CT", "MRI", "PET", "Skeletal Survey")
healthcare_interactions_keywords <- c("UMMC", "WHC", "Sinai Hospital", "Dr\\.", "Follow-Up", "Consult")
cytogenetic_keywords <- c("t\\(11;14\\)", "t\\(4;14\\)", "del\\(17p\\)", "del\\(13q\\)", 
                          "gain 1q21", "monosomy 13", "IGH", "FGFR3", "CCND1")

# Function to extract keywords
extract_keywords <- function(text, keywords) {
  if (is.na(text)) return(NA)
  found <- keywords[sapply(keywords, function(k) str_detect(text, regex(k, ignore_case = TRUE)))]
  if (length(found) == 0) return(NA)
  paste(unique(found), collapse = ", ")  # Ensure unique keywords
}

# Function to extract lab test values and status
extract_lab_tests <- function(text, data_row, lab_tests) {
  if (is.na(text)) return(data_row)
  for (test in lab_tests) {
    # Match test name followed by a number and optional status
    pattern <- paste0(test, "\\s*[:=]\\s*(\\d+\\.?\\d*)\\s*(g/dL|mg/L|U/L|mg/dL)?\\s*(NORMAL|HIGH|LOW)?")
    matches <- str_match_all(text, regex(pattern, ignore_case = TRUE))[[1]]
    if (nrow(matches) > 0) {
      # Take the first match (most recent or relevant)
      data_row[[paste0(test, "_Value")]] <- as.numeric(matches[1, 2])
      if (ncol(matches) > 3) {
        data_row[[paste0(test, "_Status")]] <- matches[1, 4]
      }
    }
  }
  data_row
}

# Function to extract age
extract_age <- function(text) {
  if (is.na(text)) return(NA)
  match <- str_extract(text, "\\d+\\s*Y\\.?O\\.?")
  if (is.na(match)) return(NA)
  as.numeric(str_extract(match, "\\d+"))
}

# Function to extract sex
extract_sex <- function(text) {
  if (is.na(text)) return(NA)
  if (str_detect(text, regex("MALE", ignore_case = TRUE))) return("Male")
  if (str_detect(text, regex("FEMALE", ignore_case = TRUE))) return("Female")
  NA
}

# Function to extract race/ethnicity
extract_race_ethnicity <- function(text) {
  if (is.na(text)) return(NA)
  race_terms <- c("AA", "African American", "Caucasian", "Non-Hispanic", "Hispanic", "White", "Black")
  found <- race_terms[sapply(race_terms, function(r) str_detect(text, regex(r, ignore_case = TRUE)))]
  if (length(found) == 0) return(NA)
  paste(unique(found), collapse = ", ")
}

# Combine all comment columns
data$Combined_Comments <- apply(data[, c("Comments - Pathology", "Comments - Physical Exam", 
                                         "Comments - Staging", "Comments - X-rays/Scans", 
                                         "Comments - Scopes", "Comments - Lab Tests", 
                                         "Comments - Operations", "Comments - Remarks", 
                                         "Comments - Follow-Up", "General Comments All Follow Ups", 
                                         "Comments - Surgical Treatment", 
                                         "Comments - Chemotherapy Treatment", 
                                         "Comments - Radiation Treatment", 
                                         "Comments - Hormone Treatment", 
                                         "Comments - Immunotherapy Treatment", 
                                         "Comments - Other Treatment")], 1, 
                                function(x) paste(x[!is.na(x)], collapse = "; "))

# Apply extraction functions
for (i in 1:nrow(data)) {
  text <- data$Combined_Comments[i]
  data$Medications[i] <- extract_keywords(text, medications_list)
  data$Treatments[i] <- extract_keywords(text, treatments_list)
  data$Disease_Characteristics[i] <- extract_keywords(text, disease_keywords)
  data$Age[i] <- extract_age(text)
  data$Sex[i] <- extract_sex(text)
  data$Race_Ethnicity[i] <- extract_race_ethnicity(text)
  data$Social_History[i] <- extract_keywords(text, social_history_keywords)
  data$Clinical_Outcomes[i] <- extract_keywords(text, clinical_outcomes_keywords)
  data$Diagnostic_Procedures[i] <- extract_keywords(text, diagnostic_procedures_keywords)
  data$Healthcare_Interactions[i] <- extract_keywords(text, healthcare_interactions_keywords)
  data$Cytogenetic_Risk[i] <- extract_keywords(text, cytogenetic_keywords)
  data[i, ] <- extract_lab_tests(text, data[i, ], lab_tests)
}

# Clean up
data <- data %>% select(-Combined_Comments)

# Flag incomplete records
data$Incomplete_Flag <- apply(data[, 1:17], 1, function(x) sum(is.na(x)) > 10)

# Save comprehensive dataset
write_xlsx(data, "comprehensive_ehr_data.xlsx")

# Detailed Analysis
# 1. Medication Summary
medication_summary <- data %>%
  filter(!is.na(Medications)) %>%
  separate_rows(Medications, sep = ", ") %>%
  count(Medications, name = "Count") %>%
  arrange(desc(Count))

# 2. Treatment Summary
treatment_summary <- data %>%
  filter(!is.na(Treatments)) %>%
  separate_rows(Treatments, sep = ", ") %>%
  count(Treatments, name = "Count") %>%
  arrange(desc(Count))

# 3. Disease Characteristics by Stage
disease_by_stage <- data %>%
  filter(!is.na(Disease_Characteristics)) %>%
  mutate(Stage = str_extract(Disease_Characteristics, "Stage I|Stage II|Stage III")) %>%
  filter(!is.na(Stage)) %>%
  group_by(Stage) %>%
  summarise(Count = n())

# 4. Lab Test Summary
lab_test_summary <- data %>%
  select("Deidentified ID", ends_with("_Value")) %>%
  pivot_longer(cols = ends_with("_Value"), names_to = "Test_Name", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Test_Name = gsub("_Value", "", Test_Name)) %>%
  group_by(Test_Name) %>%
  summarise(
    Mean_Value = mean(Value, na.rm = TRUE),
    Min_Value = min(Value, na.rm = TRUE),
    Max_Value = max(Value, na.rm = TRUE),
    Count = n()
  )

# 5. Demographic Summary
demographic_summary <- data %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Male_Count = sum(Sex == "Male", na.rm = TRUE),
    Female_Count = sum(Sex == "Female", na.rm = TRUE),
    Race_Ethnicity_Summary = paste(unique(na.omit(Race_Ethnicity)), collapse = ", "),
    Incomplete_Records = sum(Incomplete_Flag)
  )

# 6. Clinical Outcomes by Stage
outcome_by_stage <- data %>%
  filter(!is.na(Disease_Characteristics), !is.na(Clinical_Outcomes)) %>%
  mutate(Stage = str_extract(Disease_Characteristics, "Stage I|Stage II|Stage III")) %>%
  filter(!is.na(Stage)) %>%
  separate_rows(Clinical_Outcomes, sep = ", ") %>%
  group_by(Stage, Clinical_Outcomes) %>%
  summarise(Count = n(), .groups = "drop")

# 7. Correlation Analysis (Lab Values)
lab_cor_data <- data %>%
  select(ends_with("_Value")) %>%
  select_if(~sum(!is.na(.)) > 0)  # Remove columns with all NA
cor_matrix <- cor(lab_cor_data, use = "pairwise.complete.obs")

# 8. Subgroup Analysis: Outcomes by Age Group
data <- data %>%
  mutate(Age_Group = case_when(
    Age < 50 ~ "<50",
    Age >= 50 & Age < 65 ~ "50-64",
    Age >= 65 ~ "65+",
    TRUE ~ NA_character_
  ))

outcome_by_age <- data %>%
  filter(!is.na(Clinical_Outcomes), !is.na(Age_Group)) %>%
  separate_rows(Clinical_Outcomes, sep = ", ") %>%
  group_by(Age_Group, Clinical_Outcomes) %>%
  summarise(Count = n(), .groups = "drop")

# Print Summaries
cat("Medication Summary:\n")
print(medication_summary)
cat("\nTreatment Summary:\n")
print(treatment_summary)
cat("\nDisease Characteristics by Stage:\n")
print(disease_by_stage)
cat("\nLab Test Summary:\n")
print(lab_test_summary)
cat("\nDemographic Summary:\n")
print(demographic_summary)
cat("\nClinical Outcomes by Stage:\n")
print(outcome_by_stage)
cat("\nClinical Outcomes by Age Group:\n")
print(outcome_by_age)

# Visualizations
# 1. Medication Frequency
ggplot(medication_summary, aes(x = reorder(Medications, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequency of Medications Used", x = "Medication", y = "Count")
ggsave("medications_plot.png", width = 10, height = 6)

# 2. Treatment Frequency
ggplot(treatment_summary, aes(x = reorder(Treatments, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequency of Treatment Types", x = "Treatment", y = "Count")
ggsave("treatments_plot.png", width = 10, height = 6)

# 3. Stage Distribution
ggplot(disease_by_stage, aes(x = Stage, y = Count, fill = Stage)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Disease Stages", x = "Stage", y = "Count")
ggsave("stage_distribution_plot.png", width = 8, height = 6)

# 4. Lab Test Values (Boxplot)
lab_plot_data <- data %>%
  select("Deidentified ID", ends_with("_Value")) %>%
  pivot_longer(cols = ends_with("_Value"), names_to = "Test_Name", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Test_Name = gsub("_Value", "", Test_Name)) %>%
  filter(Test_Name %in% c("Albumin", "Beta-2 Microglobulin", "LDH"))

ggplot(lab_plot_data, aes(x = Test_Name, y = Value)) +
  geom_boxplot(fill = "coral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Key Lab Test Values", x = "Test", y = "Value")
ggsave("lab_test_values_plot.png", width = 10, height = 6)

# 5. Clinical Outcomes by Stage
ggplot(outcome_by_stage, aes(x = Stage, y = Count, fill = Clinical_Outcomes)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Clinical Outcomes by Disease Stage", x = "Stage", y = "Count", fill = "Outcome")
ggsave("outcome_by_stage_plot.png", width = 10, height = 6)

# 6. Clinical Outcomes by Age Group
ggplot(outcome_by_age, aes(x = Age_Group, y = Count, fill = Clinical_Outcomes)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Clinical Outcomes by Age Group", x = "Age Group", y = "Count", fill = "Outcome")
ggsave("outcome_by_age_plot.png", width = 10, height = 6)

# 7. Correlation Matrix of Lab Values
png("lab_correlation_plot.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()

# Save summaries to Excel
summary_list <- list(
  Medication_Summary = medication_summary,
  Treatment_Summary = treatment_summary,
  Disease_by_Stage = disease_by_stage,
  Lab_Test_Summary = lab_test_summary,
  Demographic_Summary = demographic_summary,
  Outcome_by_Stage = outcome_by_stage,
  Outcome_by_Age = outcome_by_age
)
write_xlsx(summary_list, "ehr_analysis_summaries.xlsx")