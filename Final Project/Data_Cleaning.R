library(psych)
library(dplyr)
library(gridExtra)

# Import TSV files
clinical <- read.table("Data/clinical.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
exposure <- read.table("Data/exposure.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
mutation <- read.table("Data/frequent-mutations.2024-03-18.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")
genes <- read.table("Data/frequently-mutated-genes.2024-03-18.tsv", header = TRUE, sep = "\t", fill = TRUE, quote = "")

# Convert empty values to NA and remove empty columns
data_frames <- list(clinical = clinical, 
                    exposure = exposure, 
                    mutation = mutation,
                    genes = genes)

replace_dash_with_NA <- function(df) {
  df[df == "'--"] <- NA
  df <- df[, colSums(is.na(df)) < nrow(df)]
  return(df)
} 

modified_data_frames <- lapply(data_frames, replace_dash_with_NA)

list2env(modified_data_frames, envir = .GlobalEnv)

# remove irrelevant rows from exposure
exposure <- subset(exposure, !is.na(cigarettes_per_day))

# combine clinical and exposure based on case id
clinical_cleaned <- merge(clinical, exposure, by = "case_id", all.x = TRUE, all.y = TRUE)

# Clinical cleaning
## filter columns in clinical df
clinical_cleaned = subset(clinical_cleaned, select = c(case_id, 
                                               ethnicity, 
                                               gender, 
                                               race, 
                                               vital_status, 
                                               age_at_diagnosis, 
                                               days_to_death,
                                               ajcc_pathologic_stage,
                                               primary_diagnosis,
                                               treatment_type,
                                               cigarettes_per_day,
                                               pack_years_smoked))


## Convert columns to numeric
clinical_cleaned[c("age_at_diagnosis", "days_to_death", 
                   "cigarettes_per_day", 
                   "pack_years_smoked")] <- lapply(clinical_cleaned[c("age_at_diagnosis", "days_to_death", 
                                                                                      "cigarettes_per_day", 
                                                                                      "pack_years_smoked")], as.numeric)
## Convert to days to years
clinical_cleaned$age_at_diagnosis <- ifelse(clinical_cleaned$age_at_diagnosis > 150,
                                            clinical_cleaned$age_at_diagnosis / 365,
                                            round(clinical_cleaned$age_at_diagnosis, digits = 2))

## Remove rows that don't have vital_status
clinical_cleaned <- subset(clinical_cleaned, !is.na(vital_status))

## Calculate age at death
clinical_cleaned <- clinical_cleaned %>% mutate(age_at_death = age_at_diagnosis + days_to_death/365)
clinical_cleaned <- clinical_cleaned %>% relocate(age_at_death, .after = age_at_diagnosis)

# Create smoker and non smoker subsets
clinical_cleaned$smoker <- NA
clinical_cleaned$smoker <- ifelse(!is.na(clinical_cleaned$cigarettes_per_day) & clinical_cleaned$cigarettes_per_day > 0, "Yes", "No")
clinical_cleaned <- clinical_cleaned %>% relocate(smoker, .before = cigarettes_per_day)

smoker <- subset(clinical_cleaned, smoker == "Yes")
non_smoker <- subset(clinical_cleaned, smoker == "No")


# Gene cleaning
## Remove unnecessary columns from genes
gene_cleaned <- subset(genes, select = -c(num_gdc_ssm_affected_cases, 
                                          num_gdc_ssm_cases, 
                                          gdc_ssm_affected_cases_percentage,
                                          annotations))

# Descriptive statistics

## Clinical_cleaned categorical descriptive statistics
clinical_table <- apply(subset(clinical_cleaned, select = -c(case_id, 
                                                             age_at_diagnosis, 
                                                             age_at_death, 
                                                             days_to_death,
                                                             cigarettes_per_day,
                                                             pack_years_smoked)),2,table)

print(clinical_table)

smoker_table <- apply(subset(smoker, select = -c(case_id, 
                                                             age_at_diagnosis, 
                                                             age_at_death, 
                                                             days_to_death,
                                                             cigarettes_per_day,
                                                             pack_years_smoked)),2,table)

print(smoker_table)

non_smoker_table <- apply(subset(non_smoker, select = -c(case_id, 
                                                             age_at_diagnosis, 
                                                             age_at_death, 
                                                             days_to_death,
                                                             cigarettes_per_day,
                                                             pack_years_smoked)),2,table)

print(non_smoker_table)

## smoker and non-smoker continuous descriptive statistics
## smoker
smoker_stats <- describe(subset(smoker, select = c(age_at_diagnosis, 
                                                               age_at_death, 
                                                               days_to_death,
                                                               cigarettes_per_day,
                                                               pack_years_smoked)))

smoker_stats <- subset(smoker_stats, select = -c(range, skew, kurtosis, se))
png("smoker_stats.png", height = 50*nrow(smoker_stats), width = 200*ncol(smoker_stats))
grid.table(smoker_stats)
dev.off()

## non_smoker
non_smoker_stats <- describe(subset(non_smoker, select = c(age_at_diagnosis, 
                                                       age_at_death, 
                                                       days_to_death,
                                                       cigarettes_per_day,
                                                       pack_years_smoked)))

non_smoker_stats <- subset(non_smoker_stats, select = -c(range, skew, kurtosis, se))
png("non_smoker_stats.png", height = 50*nrow(non_smoker_stats), width = 200*ncol(non_smoker_stats))
grid.table(non_smoker_stats)
dev.off()

## clinical
clinical_stats <- describe(subset(clinical_cleaned, select = c(age_at_diagnosis, 
                                                           age_at_death, 
                                                           days_to_death,
                                                           cigarettes_per_day,
                                                           pack_years_smoked)))

clinical_stats <- subset(clinical_stats, select = -c(range, skew, kurtosis, se))
png("clinical_stats.png", height = 50*nrow(clinical_stats), width = 200*ncol(clinical_stats))
grid.table(clinical_stats)
dev.off()

## Create image for gene descriptive statistics
gene_stats <- describe(subset(gene_cleaned, select = -c(gene_id, symbol, name, cytoband, type)))
gene_stats <- subset(gene_stats, select = -c(range, skew, kurtosis, se))
png("gene_stats.png", height = 50*nrow(gene_stats), width = 200*ncol(gene_stats))
grid.table(gene_stats)
dev.off()
