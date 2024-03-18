
library(psych)
library(dplyr)
#TEST
setwd("~/GitHub/Prob-and-Stats-for-Biomed/Final Project")

# Import TSV files
clinical <- read.table("Data/clinical.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
exposure <- read.table("Data/exposure.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
mutation <- read.table("Data/frequent-mutations.2024-03-16.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
genes <- read.table("Data/frequently-mutated-genes.2024-03-16.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")

# Convert empty values to NA and remove empty columns
data_frames <- list(clinical = clinical, 
                    exposure = exposure, 
                    family_history = family_history, 
                    follow_up = follow_up, 
                    aliquot = aliquot, 
                    analyte = analyte, 
                    portion = portion, 
                    sample = sample, 
                    pathology_detail = pathology_detail, 
                    slide = slide,
                    mutation = mutation,
                    genes = genes)

replace_dash_with_NA <- function(df) {
  df[df == "'--"] <- NA
  df <- df[, colSums(is.na(df)) < nrow(df)]
  return(df)
} 

modified_data_frames <- lapply(data_frames, replace_dash_with_NA)

list2env(modified_data_frames, envir = .GlobalEnv)

# filter columns in clinical df

clinical_cleaned = subset(clinical, select = c("case_id", 
                                               "ethnicity", 
                                               "gender", 
                                               "race", 
                                               "vital_status", 
                                               "age_at_diagnosis", 
                                               "days_to_death",
                                               "ajcc_pathologic_stage", 
                                               "treatment_or_therapy", 
                                               "treatment_type"))
str(clinical_cleaned)

# Convert age column to int and years
clinical_cleaned$age_at_diagnosis <- as.integer(clinical_cleaned$age_at_diagnosis)
clinical_cleaned$days_to_death <- as.integer(clinical_cleaned$days_to_death)

clinical_cleaned$age_at_diagnosis <- ifelse(clinical_cleaned$age_at_diagnosis > 150,
                                            clinical_cleaned$age_at_diagnosis / 365,
                                            round(clinical_cleaned$age_at_diagnosis, digits = 2))

# Calculate age at death
clinical_cleaned <- clinical_cleaned %>% mutate(age_at_death = age_at_diagnosis + days_to_death/365)
clinical_cleaned <- clinical_cleaned %>% relocate(age_at_death, .after = age_at_diagnosis)

# Remove rows that don't have vital_status
clinical_cleaned <- subset(clinical_cleaned, !is.na(vital_status))

# Remove rows from exposure that don't have cigarettes
exposure_cleaned <- subset(exposure, !is.na(cigarettes_per_day))

describe(clinical_cleaned)
summary(clinical_cleaned)
