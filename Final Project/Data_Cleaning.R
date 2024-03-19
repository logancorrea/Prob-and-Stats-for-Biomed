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
clinical_cleaned <- subset(clinical_cleaned, !is.na(vital_status) & vital_status != "Not Reported" & vital_status != "Unknown")

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

## clinical
clinical_stats <- describe(subset(clinical_cleaned, select = c(age_at_diagnosis, 
                                                               age_at_death, 
                                                               days_to_death,
                                                               cigarettes_per_day,
                                                               pack_years_smoked)))
print(clinical_stats)
clinical_stats <- subset(clinical_stats, select = -c(range, skew, kurtosis, se))
png("clinical_stats.png", height = 50*nrow(clinical_stats), width = 200*ncol(clinical_stats))
grid.table(clinical_stats)
dev.off()

clinical_table <- apply(subset(clinical_cleaned, select = -c(case_id, 
                                                             age_at_diagnosis, 
                                                             age_at_death, 
                                                             days_to_death,
                                                             cigarettes_per_day,
                                                             pack_years_smoked)),2,table)

print(clinical_table)


## Create image for gene descriptive statistics
gene_stats <- describe(subset(gene_cleaned, select = -c(gene_id, symbol, name, cytoband, type)))
gene_stats <- subset(gene_stats, select = -c(range, skew, kurtosis, se))
png("gene_stats.png", height = 50*nrow(gene_stats), width = 200*ncol(gene_stats))
grid.table(gene_stats)
dev.off()

## Create bar chart for gene distributions
gene_10 <- gene_cleaned[1:10,]
gene_bar <- setNames(gene_10$cohort_ssm_affected_cases_percentage, gene_10$symbol)

# Open a PNG device to save the plot
png(file = "gene_distribution_barplot.png", width = 800, height = 600)

# Creating the bar chart with genes on the x-axis and percentages on the y-axis
barplot(gene_bar,
        main = "Distribution of Most Frequently Mutated Genes in Lung Adenocarcinomas",
        xlab = "Gene",
        ylab = "% of Cases Affected",
        col = "seagreen",
        las = 1, # Makes gene names perpendicular to the axis for better readability
)

# Close the device to save the file
dev.off()