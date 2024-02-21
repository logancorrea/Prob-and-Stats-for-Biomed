
## A. It is thought that smoking affects fetus growth and development and we want to test whether smoking during pregnancy reduces fetal weight. You want to test this idea.
#### (i) What kind of statistical test should you use? Is this a parametric or non-parametric test, and what are the specific assumptions about data that underlie this test?
# two-sample t-test
# parametric test

#### (ii) What is your null hypothesis?
# H0 = There is no difference in mean birth weight between babies born to mothers who smoked during pregnancy and those who did not.

#### (iii) What is your alternative hypothesis?
# HA = Babies born to mothers who smoked during pregnancy will have a lower birth weight than those whose mothers did not smoke.

#### (iv) Run the test.
#### (v) What is your conclusion?



## B. You want to compare the birth weights between individuals whose mothers identified as different races in this study.
#### (i) What kind of statistical test should you use? Is this a parametric or non-parametric test, and what are the specific assumptions about data that underlie this test?
#### (ii) What is your null hypothesis?
#### (iii) What is your alternative hypothesis?
#### (iv) Run the test/tests.
#### (v) What is your conclusion, with respect to which pairs of specific groups have significant differences in birth weight?



## C. A study was conducted to investigate the effect of physical training on the serum cholesterol level.
### Thirty subjects participated in the study. Prior to training, blood samples were taken to determine the cholesterol level of each subject. Then the subjects were put through a training program that centered on daily running and jogging. At the end of the training period, blood samples were taken again and a second reading on the serum cholesterol level was obtained.
#### (i) What kind of statistical test should you use? Is this a parametric or non-parametric test, and what are the specific assumptions about data that underlie this test?
#### (ii) What is your null hypothesis?
#### (iii) What is your alternative hypothesis?
#### (iv) Run the test/tests.
#### (v) What is your conclusion, Is there an effect of the training on cholesterol levels?

### Is there an effect of the training on cholesterol levels?
### The data is located in this working directory with the name training.csv

##Load data
training = read.csv("training.csv")


## D. A group of 24 patients with chronic pain was randomized into three groups receiving different treatments.
### The pain level (on a scale from 1 to 10) was measured after 1 week of treatment. Test the hypothesis that there is no difference in pain level among the three treatments test. Use a significance level of 0.05.
#### 1.Treatment A: 7, 6, 8, 9, 5, 6, 7, 8
#### 2.Treatment B: 4, 5, 6, 4, 7, 6, 5, 4
#### 3.Treatment C: 2, 1, 3, 4, 3, 2, 1, 2
#### (i) What kind of statistical test should you use? Is this a parametric or non-parametric test, and what are the specific assumptions about data that underlie this test?
#### (ii) What is your null hypothesis?
#### (iii) What is your alternative hypothesis?
#### (iv) Run the test/tests.
#### (v) What is your conclusion, with respect to which pairs of specific groups have significant differences in pain level (hint: pairwise.wilcox.test function in R)?


a = c(7, 6, 8, 9, 5, 6, 7, 8 )
b = c(4, 5, 6, 4, 7, 6, 5, 4 )
c = c(2, 1, 3, 4, 3, 2, 1, 2)

df = data.frame(vals = c(a,b,c),treat = c(rep("a",8),rep("b",8),rep("c",8)))



## The following questions do not require R.
## E. Distinguish between parametric vs non-parametric tests and explain the tradeoffs in choosing one vs the other for hypothesis testing.
## F. A survey was conducted to determine the average income of households in a certain city.
#### A random sample of 500 households was selected, and the mean income was found to be \$60,000, with a standard deviation of \$10,000.
#### (i) Calculate the standard error of the sample mean.
#### (ii) What is the 95% confidence interval for the population mean income?
#### (iii) How is the interpretation of the value obtained for the standard error different from the one obtained for the standard deviation?
