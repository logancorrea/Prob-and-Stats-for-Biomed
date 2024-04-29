# Get the names of all objects in the environment
all_objects <- ls()

# Remove all objects except "df"
objects_to_remove <- setdiff(all_objects, c("part1", "part2", "part3", "part4", "part5", "part6", "genes", "bigdata"))
rm(list = objects_to_remove)


# Initialize vectors to store results
log2FC <- numeric(ncol(combinedData))
pValues <- numeric(ncol(combinedData))

for (i in 1:(ncol(combinedData))) {
  geneExpressionNormal <- normalData[,i]
  geneExpressionTumor <- tumorData[,i]
  
  # Calculate log2 Fold Change (tumor vs normal)
  log2FC[i] <- log2(mean(geneExpressionTumor) / mean(geneExpressionNormal))
  
  # Calculate p-value using Welch's t-test
  pValues[i] <- t.test(geneExpressionTumor, geneExpressionNormal)$p.value
}

# Create a data frame to store results
results <- data.frame(
  Gene = colnames(df)[-1],  # Exclude the Diagnosis column
  Log2FoldChange = log2FC,
  PValue = pValues
)