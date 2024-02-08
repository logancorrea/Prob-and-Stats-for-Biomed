# Problem Set 2

setwd("C:/Users/logan/Documents/GitHub/Prob-and-Stats-for-Biomed/Homework/HW02/")

# Load dataset 
B_Samples = read.table("birthwt.txt", sep = "", header = T)
head(B_Samples)

# 1A How many observations are in the dataset?
  num_observations <- nrow(B_Samples)
  cat("Number of observations in the dataset: ", num_observations, "\n")

# 1B Examine the features, determine what type of variable each represents, 
# and indicate whether each one is discrete or continuous. Then, go on to determine 
# the distribution or descriptive statistics as appropriate
  
  # For Discrete Variables:
    # Indicate whether the feature is nominal, ordinal, or binary
    # How many levels each variable has
    
    # Discrete Variables:
      # low - binary
      B_Samples$low <- as.factor(B_Samples$low)
      nlevels(B_Samples$low)
      
      # race - nominal
      B_Samples$race <- as.factor(B_Samples$race)
      nlevels(B_Samples$race)
      
      # smoke - binary
      B_Samples$smoke <- as.factor(B_Samples$smoke)
      nlevels(B_Samples$smoke)
      
      # ptl - ordinal
      B_Samples$ptl <- as.factor(B_Samples$ptl)
      nlevels(B_Samples$ptl)
      
      # ht - binary
      B_Samples$ht <- as.factor(B_Samples$ht)
      nlevels(B_Samples$ht)
      
      # ui - binary
      B_Samples$ui <- as.factor(B_Samples$ui)
      nlevels(B_Samples$ui)
      
      # ftv - ordinal
      B_Samples$ftv <- as.factor(B_Samples$ftv)
      nlevels(B_Samples$ftv)
  
  # For Continuous variables:
    # Determine the mean, standard deviation, and median
  
    # Continuous Variable:
      # age
      mean(B_Samples$age)
      sd(B_Samples$age)
      median(B_Samples$age)
      
      # lwt
      mean(B_Samples$lwt)
      sd(B_Samples$lwt)
      median(B_Samples$lwt)
      
      # bwt
      mean(B_Samples$bwt)
      sd(B_Samples$bwt)
      median(B_Samples$bwt)

# 1C How many individuals older than 30 smoke?
  sum(B_Samples$smoke == 1 & B_Samples$age > 30)

# 1D Plot a histogram for birth weight.
  hist(B_Samples$bwt,
       main = "Birth Weight",
       xlab = "Weight in Grams",
       )

# 1G Calculate the probability of randomly selecting an individual that has either a low birth weight or a mother who was a smoker.
  total_individuals <- nrow(B_Samples)
  
  # Probability of individuals with low birth weight P(A)
  low_prob <- sum(B_Samples$low == 1)/total_individuals
  
  # Probability of individuals with a mother who smoked P(B)
  smoke_prob <- sum(B_Samples$smoke == 1)/total_individuals
  
  # Probability of individuals with low birth weight and mother who smoked P(A and B)
  low_smoke_prob <- sum(B_Samples$low == 1 | B_Samples$smoke == 1)/total_individuals
  
  # Calculate the probability P(A ⋃ B) = P(A)+P(B)-P(A and B)
  low_prob + smoke_prob - low_smoke_prob

# 1H Calculate the probability of randomly selecting an individual that is white and
# has more than 3 physician visits during the first trimester.
  
  # Probability of individual being white P(A)
  white_prob <- sum(B_Samples$race == 1)/total_individuals
  
  # Probability of having more than 3 physician visits P(B)
  phys_prob <- sum(as.numeric(B_Samples$ftv) > 3) / total_individuals
  
  # Probability of being white and 3 visits P(A and B) = P(A)*P(B)
  white_prob * phys_prob
  
#2A What is the probability that given a positive mammogram exam, a woman has a positive cancer diagnosis?
# Assume that the breast cancer incidence rate is 1%, the positivity rate for the exam if a patient has cancer is 90%,
# and there is a false positive rate of 8% for the exam.
  
  
  
  # P(Cancer) = 0.01
  # P(Positive|Cancer) = 0.9
  # P(Positive|No Cancer) = 0.08
  # P(No Cancer) = 1 - P(Cancer)
  
  pCancer <- 0.01
  pPositive_Cancer <- 0.9
  pPositive_No_Cancer <- 0.08
  PNo_Cancer <- 1-pCancer
  
  # P(Positive) = P(Positive|Cancer)*P(Cancer)+P(Positive|No Cancer)*P(No Cancer)
  pPositive <- pPositive_Cancer * pCancer + pPositive_No_Cancer * PNo_Cancer
  
  # P(Cancer|Positive) = P(Positive|Cancer)*P(Cancer)/P(Positive)
  pPositive_Cancer * pCancer / pPositive

# 2B For every attempt to call your friend, there is a 70% probability of actually speaking with them. 
# Calculate the probability of having exactly 12 successes in 20 attempts.  
  
  # number of attempts
  n <- 20
  
  # number of successes
  k <- 12
  
  # Probability of success
  p <- 0.7
 
  # Calculate probability 
  dbinom(k, size = n, prob = p)
  
# 2C If a sample of test scores is normally distributed with a mean of 42 and a standard deviation of 8, 
# what percent of the scores is:
  # (i) Greater than 25?
  # (ii) Smaller than 31?
  # (iii) Between 25 and 31?
    
# 3 Naïve Bayes classifier
# For this exercise we are going to build a Naïve Bayes classifier to try to predict
# bening/malignant from measurements taken from breast mass using characteristics
# of cell nuclei taken from digitized. The dataset should be available on canvas
  
  # 1. Explain the performance of your classifier.
  # 2. Investigate a bit about different types of naïve bayes classifiers, using that
  # information what could you change that might improve the performance of
  # this classifier for this dataset?
    
  

  
