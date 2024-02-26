# A biomedical informatics researcher is investigating the relationship between smoking and multiple biological measurements. 
# We want to compare the mean age for both smokers and non-smokers and we want to sample participants from each group 
# proportionally to their representation in the population. The dataset smoking.csv contains this information, 
# with two columns: “age” (numeric) and “smoking” (factor with 2 levels “0” and “1” [yes/no]). 
# Answer the following questions using the concepts seen in class.

# 1. Point Estimate:
## Calculate the mean age for the entire population based on the collected sample.
setwd("/Users/logancorrea/Documents/GitHub/Prob-and-Stats-for-Biomed/Homework/HW03")
smoking <- read.csv("smoking.csv")
smoking_mean <- mean(smoking$age)
print(smoking_mean)

# 2. Random Sampling:
##  Randomly select a subset of 50 patients from the dataset without replacement. Calculate the mean age for this subset. 
set.seed(1)
subset_smoking <- smoking[sample(nrow(smoking), 50, replace = FALSE), ]
mean_age_subset <- mean(subset_smoking$age)
print(mean_age_subset)

#3. Resampling:
##  Perform bootstrapping on the entire dataset to estimate the sampling distribution of the mean age for the cohort 
# Use 1000 bootstrap samples and calculate the mean age for each sample.

#4. Confidence Intervals:
##  Calculate a 95% confidence interval for the population mean age level using the bootstrap 
# distribution obtained in the previous step.

#5. Standard Error of the Mean (SEM):
##  Calculate the standard error of the mean (SEM) of your estimate.


#2.
# Markov Chain: physical exercise training method A is used only 5% of the time, a person using method A will stay 
# with this method 85% of the time, and a person not using method A will switch to method A about 65% time. 
# At the beginning of the experiment only 5% of people used method A.
#1. Generate a transition matrix for this Markov chain

#2. Generate a transition plot (using R or by hand as an image it’s valid)

#3. Plot the change in the probabilities over time for both methods until the 10th time unit.


#3.
#Random Walk: Another simpler example of a random walk is a one-dimensional random walk. first we place a marker at zero 
# (our initial state), we flip a coin, if it lands on heads, the marker is moved one unit to the right (1), 
# if it lands on tails it is moved one unit to the left.
#1. Generate a function that randomly draws from our initial state and populates a vector with the different transitions.

#2. Generate a plot that shows 500 independent one-dimensional walks, differentiating walks that end above 0 or below 0.

#3. What is the frequency of walks that ended in a positive cumulative count, in zero, or negative?
  