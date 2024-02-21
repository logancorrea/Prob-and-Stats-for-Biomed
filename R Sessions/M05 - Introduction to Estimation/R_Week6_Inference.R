####Wednesday February 15th#####
####R Session Inference####
  
###1. We are going to use our use of R and probabilities to be able to determine which features help to separate diabetes patietns

###First load the dataset

#install.packages("qqplotr", dependencies = TRUE)

library(qqplotr)
library(MASS)
library(tidyverse)
library(FSA)


###Where is your dataset located? you can either import it from where it is located or set your working directory.

setwd("/Users/javier/Documents/Jupyter/MBIO_6490_2023/R_Sessions/Week_3")


#Binomial test -- Earlobe Example

##1. What is the probability of obtaining the number of free ear lobes? Attached ear lobes?

p = 0.65
x = 47 # of people with "free" earlobes written on the board here
n = 54 #total number of individuals here

47/54
domain = 0:n
dbinom(x, n, p) #Given that the probability of a “free” ear lobe is 0.65, 
#what is the probability of collecting your sample (i.e. X people with a “free” ear lobe in a group of n = _______ people?)


###the probability of every possible X value using the same distribution parameters
null.dist = dbinom(domain, n, p) 
print(paste(c("The null distribution is: ", null.dist)))

barplot(null.dist, names.arg=domain)

###Are our probability Corrects?
sum(null.dist)


#What is the “expected” number of individuals with “free” ear lobes given the null hypothesis (= expected number of successes)?

#E = n*p

expected = n*p
print(paste(c("The expected value under the null is: ", expected)))

#This result tell us the expected number of successes if the null hypothesis was true?

##Next let's see how far our real data deviated from the expectation under the null:
obs.diff = x - expected
print(paste("The difference between the observed and the expected was", obs.diff))

##And quantify the extreme values
as.extreme = abs(domain - expected) >= abs(obs.diff)
print("The tails of the distribution will correspond to TRUE values:")
print(as.extreme)

##Let's plot these extreme values in our distributions
col = c("white", "red")
x.col = col[1 + as.numeric(as.extreme)]
barplot(null.dist, names.arg=domain, col=x.col)

###What is the probability of getting the extreme values?
prob.as.extreme = sum(null.dist[as.extreme])
z = paste("If the null is true, the prob. of a stat at least as extreme as",             x, "is", prob.as.extreme)
print(z)

##What is the p-value?
binom.test(x,n,0.65)


#T-test 

##Golub et al. (1999) measured the level of expression of human genes in 38 patients suffering from two cancer types: acute lymphoblastic leukemia (ALL, 27 patients) and acute myeloid leukemia (AML, 11 patients).
#The aim of such experiments is to detect a subset of genes which can be used for diagnostic tests, in order to asses whether a new patient suffers from either of these cancer types.

#For this practical, we will use a pre-normalized and pre-filtered subset of 3051 genes, and select those which show a significant difference in expression between AML and ALL patients.

#We will thus apply a Student test to each gene g independendly, to test the null hypothesis.

##Read the data
golub.expr <- read.table("Golub_1999_train_Z.tab.txt",sep='\t', header=T)

###Generate column names
n.ALL <- 27
n.AML <- 11
cancer.type <- c(rep("ALL", n.ALL), rep("AML", n.AML))

## Add the cancer type to the column name, for the display
names(golub.expr) <- paste(names(golub.expr), cancer.type,sep="_")

## t.test with a single gene

#g <- 347 



## Alternatively, you can select a gene randomly
set.seed(10)
g <- sample(1:nrow(golub.expr),1)

###Extract a vector with the selected gene
g.profile <- as.vector(as.matrix(golub.expr[g,]))


## Draw a barpplot with color-coded cancer type
plot.col <- c('ALL'='#4444BB', 'AML'='#FFFF88')
#x11(width=12,height=4)
barplot(g.profile,main=paste("Golub (1999), gene", g), col=plot.col[cancer.type])
legend('topright', c("ALL","AML"),col=plot.col[c("ALL","AML")],pch=15,bty="o",bg='white')
legend('topright', c("ALL","AML"),col='black',pch=22,bty="n")

## separate data in two vectors
sample.ALL <- g.profile[cancer.type=="ALL"]
sample.AML <- g.profile[cancer.type=="AML"]


##Is the variance between samples equal or different

var.test(sample.ALL,sample.AML) ##If the test is not significant then the variance between samples is the same

var.test(sample.ALL,sample.AML)[[3]]<0.05
## Apply the Student-Fischer t-test 
## (this assumes that the two populations have an equal variance)
t.student <- t.test(sample.ALL,sample.AML, var.equal=TRUE)
print(t.student) 


##Unequal variance
t.welch <- t.test(sample.ALL,sample.AML, var.equal=FALSE)
print(t.welch) 


###but We want to see all genes, we can loop over the number of genes.

t.statistics <- vector()
P.values <- vector()
for (g in 1:nrow(golub.expr)) {
  #  print(paste("Testing gene", g))
  g.profile <- as.vector(as.matrix(golub.expr[g,]))
  sample.ALL <- g.profile[cancer.type=="ALL"]
  sample.AML <- g.profile[cancer.type=="AML"]
  if (var.test(sample.ALL,sample.AML)[[3]]<0.05){
    t <- t.test(sample.ALL,sample.AML,var.equal = F)
  }else{
    t <- t.test(sample.ALL,sample.AML)
  }
  t.statistics <- append(t.statistics, t$statistic)
  P.values <- append(P.values, t$p.value)
}
print(P.values)

##Apply bonferroni correction
Padj = P.values * length(P.values)

which(Padj<0.05)

Padj_bonf = p.adjust(P.values, method = "bonferroni")

which(Padj_bonf<0.05)

##Apply FDR

Padj_FDR = p.adjust(P.values, method = "BH")

which(Padj_FDR<0.05)




#Body Temperature Dataset
#Paired T-test

##Platelet dataset
platelet = read.table("platelet.txt",sep='\t', header=T)

##Before is skewed
platelet %>%
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_boxplot() 
  
##Deviation from Normality?
platelet %>%
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(alpha = 0.5) 


t.test(platelet$Before,platelet$After,paired = T)


##
#whitefly dataset

whitefly = read.csv("whitefly.csv")
t.test(whitefly$White,whitefly$Yellow, paired = TRUE, var.equal = TRUE)



#Anova
library(MASS)
?Cushings
data("Cushings")
view(Cushings)

Cushings %>%
  ggplot(aes(x=Type,Tetrahydrocortisone)) + 
  geom_boxplot() +
  geom_jitter()

Cushings %>%                               # Summary by group using dplyr
  group_by(Type) %>% 
  summarize(min = min(Tetrahydrocortisone),
            q1 = quantile(Tetrahydrocortisone, 0.25),
            median = median(Tetrahydrocortisone),
            mean = mean(Tetrahydrocortisone),
            q3 = quantile(Tetrahydrocortisone, 0.75),
            max = max(Tetrahydrocortisone))


cus_aov = aov(Cushings$Tetrahydrocortisone ~ Cushings$Type)

summary(cus_aov)

plot(cus_aov)


##PostHoc Analysis
TukeyHSD(cus_aov)


#####
#Mann-Whitney test

#Import the data
dat<-read.csv("InsectSprays.csv")

#Designate spray as a categorical factor
dat$spray<-as.factor(dat$spray)

table(dat$spray)
library(tidyselect)
#Produce descriptive statistics by group
dat %>%  group_by(spray) %>% 
  summarise(n = n(), 
            mean = mean(bugs, na.rm = TRUE), 
            sd = sd(bugs, na.rm = TRUE),
            stderr = sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median = median(bugs, na.rm = TRUE),
            min = min(bugs, na.rm = TRUE), 
            max = max(bugs, na.rm = TRUE),
            IQR = IQR(bugs, na.rm = TRUE))

#Produce Boxplots and visually check for outliers
ggplot(dat, aes(x = spray, y = bugs, fill = spray)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot(fill = "light blue") + 
  geom_jitter() +
  stat_summary(fun=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplot of Treatments C and D") + 
  theme_bw() + theme(legend.position="none")

#Test each group for normality
dat %>%
  group_by(spray) %>%
  summarise(`W Stat` = shapiro.test(bugs)$statistic,
            p.value = shapiro.test(bugs)$p.value)

#Perform QQ plots by group
ggplot(data = dat, mapping = aes(sample = bugs, color = factor(spray))) +
  stat_qq_line() +
  stat_qq_point(col="black") +
  facet_wrap(~ spray, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()


##We want to compare group C V D, so we need to subset the data

dat2 = dat[dat$spray=="C" | dat$spray=="D",]

#Perform the Mann-Whitney U test
m1<-wilcox.test(bugs ~ spray, data=dat2, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m1)



#Wilcoxon Signed Rank Test

#whitefly dataset measures effectiveness of greenhouse sticky traps in catching whitefly
wilcox.test(whitefly$White,whitefly$Yellow,paired = T,exact = F)



#Kruskal-Wallis - ANOVA

#Perform the Kruskal-Wallis on all spray types

m1<-kruskal.test(bugs ~ spray, data=dat)
print(m1)

##PostHoc analysis
library(FSA)

PT = dunnTest(bugs ~ spray, data=dat,
              method="bh")    # Can adjust p-values; 
# See ?p.adjust for options 

PT

##
data("airquality")
?airquality
view(airquality)
str(airquality)
summary(airquality)
kruskal.test(airquality$Ozone ~ airquality$Month)

PT = dunnTest(Ozone ~ Month, data=airquality,
              method="bh")    # Can adjust p-values; 
PT
