# 2. Problem Set R

### Now is yoiur turn to practice!!

#### We are using a new Dataset, This is a frequently used dataset for multiple applications in statistics and machine learning. This dataset is deposited in a website that is pretty useful in bioinformatics to look for tutorials, datasets and advice Kaggle (https://www.kaggle.com)

#### Go to https://www.kaggle.com/saurabh00007/diabetescsv and download the csv file

#### This dataset consists on clinical variables for 768 patients to evaluate a few variables to predict whether a patient has diabetes.

#### Please write the R code necessary to run the next items:

#### 1. Load the dataset and show the first 5 lines<br>

setwd("C:/Users/logan/Documents/GitHub/Prob-and-Stats-for-Biomed/Homework/HW01/")

list.files()

D_Samples = read.csv("diabetes.csv", sep = ",", header = T)
head(D_Samples, 5)
  
#### 2. How many patients have diabetes?<br>

nrow(D_Samples %>% filter(Outcome == 1))

#### 3. How many patients have diabetes that are older than 45?<br>

D_Samples %>% filter(Outcome == 1, Age > 45) %>% tally()

#### 4. What is the mean and variance of glucose levels for individuals without diabetes<br>

summary_Glucose <- D_Samples %>% filter(Outcome == 1)  %>%
  summarize(mean = mean(Glucose, na.rm = TRUE), variance = var(Glucose, na.rm = TRUE))
summary_Glucose

#### 5. Create a new discrete variable that has 1 if the individual has diabetes and high blood pressure (above 100), 2 if an indivual has diabetes and low blood pressure and 3 if the individual does not have diabetes.<br>

D_Samples = D_Samples %>%
  mutate(BP_Diabetes = if_else(BloodPressure > 100 & Outcome == 1, 1,
                               if_else(BloodPressure <= 100 & Outcome == 1, 2,
                                       if_else(Outcome == 0, 3, 0))))

head(D_Samples)

#### 6. Construct two plots of the distribution of BMI for individuals with diabetes and without diabetes<br><br>

ggplot(data = D_Samples %>% filter(Outcome == 1), mapping = aes(x = BMI)) +
  geom_histogram(color = "white", bins = 25)+
  theme_classic()+
  ggtitle("BMI of Patients with Diabetes")
  
ggplot(data = D_Samples %>% filter(Outcome == 0), mapping = aes(x = BMI)) +
  geom_histogram(color = "white", bins = 25)+
  theme_classic()+
  ggtitle("BMI of Patients without Diabetes")