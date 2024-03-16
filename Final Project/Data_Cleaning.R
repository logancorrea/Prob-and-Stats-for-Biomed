
library(psych)


setwd("~/Documents/GitHub/Prob-and-Stats-for-Biomed/Final Project")

# Import TSV files
clinical <- read.table("Data/clinical.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
exposure <- read.table("Data/exposure.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
family_history <- read.table("Data/family_history.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
follow_up <- read.table("Data/follow_up.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
aliquot <- read.table("Data/pathology_detail.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
analyte <- read.table("Data/pathology_detail.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
portion <- read.table("Data/pathology_detail.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
sample <- read.table("Data/pathology_detail.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
pathology_detail <- read.table("Data/pathology_detail.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
slide  <- read.table("Data/slide.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
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

clinical_cleaned = subset(clinical, select = c("case_id", "ethnicity", "gender", "race", "vital_status", "age_at_diagnosis", "ajcc_pathologic_stage", "treatment_or_therapy", "treatment_type"))
str(clinical_cleaned)

# Convert age column to int and years
clinical_cleaned$age_at_diagnosis <- as.integer(clinical_cleaned$age_at_diagnosis)

clinical_cleaned$age_at_diagnosis <- ifelse(clinical_cleaned$age_at_diagnosis > 150,
                                            clinical_cleaned$age_at_diagnosis / 365,
                                            round(clinical_cleaned$age_at_diagnosis))

# Remove rows that don't have vital_status
clinical_cleaned <- subset(clinical_cleaned, !is.na(vital_status))

describe(clinical_cleaned)
