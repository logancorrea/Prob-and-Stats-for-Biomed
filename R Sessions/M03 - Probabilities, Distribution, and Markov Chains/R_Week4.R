####Monday January 30#####
####R Session Probability####
  

##Markov Chains

library(markovchain)

#Lets define our states
weatherStates <- c("Rain", "Nice", "Snow")
byRow <- TRUE

##lets create the transitional matrix
weatherMatrix <- matrix(data = c(0.5, 0.25, 0.25,
                                 0.5, 0.0, 0.5,
                                 0.25, 0.25, 0.5), byrow = byRow, nrow = 3,
                        dimnames = list(weatherStates, weatherStates))

##make the matrix a markovchain class
mcWeather <- new("markovchain", states = weatherStates, byrow = byRow,
                 transitionMatrix = weatherMatrix, name = "Weather")

##define the initial state
initialState <- c(0, 1, 0)

weatherMatrix

mcWeather

##State of probabilities after 3 days
mcWeather ^3

after3Days <- initialState * (mcWeather ^3)
after3Days

plot(mcWeather)


##Random Walks
###

##Let's generate the transition matrix
P=matrix(0,5,5)
P[1,] = c(1,0,0,0,0)
P[2,] = c(0.5,0,0.5,0,0)
P[3,] = c(0,0.5,0,0.5,0)
P[4,] = c(0,0,0.5,0,0.5)
P[5,] = c(0,0,0,0,1)
#P


##We need to write a function that takes an initial state, a number of steps and a transition matrix to simulate the random walk
##Remember for the intial state it needs to be the same size vector for the number of states in the event.


N = 10 ##Number of steps to run
P0 = c(0.0005,0.4,0.199,0.4,0.0005)

X = matrix(0,1,10)
##We can start on any state let's start in 3

a = 3
X[1] = a

##We can generate the next step by randomly sampling one value using the transition matrix probs.
sample(c(1:5),1,replace=T,prob = P[a,]) ##Sampling with replacement, prob P[a,]


##for loop that iterates over the number of steps in the walk and randomly select (samples) a new step based on the probabilities
##from the transition matrix
for (i in 2:10){
  a=sample(c(1:5),1,replace=T,P[a,])
  X[i]=a
}

X

b = as.vector(X)
b

###Putting it all together in a functions
markov = function(N,Po,P){
  P = P*P0
  X=matrix(0,1,N) ##Empty one dimentional matrix to add values to each step
  a = 3
  X[1] = a
  for (i in 2:N){
    a=sample(c(1:5),1,replace=T,P[a,])
    X[i]=a
  }
  b = as.vector(X)
  return(b)
}

P0 = c(0.0005,0.4,0.199,0.4,0.0005)
markov(10,P0,P)



###Lets plot it
N =10
#plot(NA, xlim=c(0,20), ylim=c(0,5))#empty plot
datas = matrix(ncol = N, nrow = 100)
freq = vector()
for (i in 1:100){
  Sys.sleep(0.1)
  datas[i,] = markov(N,P0,P)##add walks of 20 steps to the matrix
  condir = datas[i,]##
  col = (condir[N]==1)
  freq[i] = datas[i,N]
  barplot(table(freq))
  #lines(condir, lwd=2,col = ifelse(col, "coral","forestgreen"))
}

table(datas[,20])

##############Let's continue here with probability distributions#################

##2. Probability Distributions

# R provides a series of function to calculate properties of the distributions from a large number of discrete and
#continuous distributions

n <- 8

p <- 0.15

barplot(dbinom(0:n, n, p), names.arg = 0:n)
###

##CDF
plot(pbinom(0:20,n,p),type = "l")

# R has several commands for working with probability distributions like the binomial
# distribution. These commands are prefixed with d, p, and r. They take a
# suffix that describes the distribution.For the binomial distribution, these commands are the
# following:
  
#dbinom(k,n,p) Computes P(X = k)
#pbinom(k,n,p) Computes P(X ≤ k)
#rbinom(k,n,p) Simulates k random variables


##In a field of 100 trees, each tree has a 10% chance of being infected by a root disease 
##independently of other trees.What is the probability that more than five trees are infected?

##k = ??
##n = ??
##p = ??

plot(pbinom(0:20,n,p),type = "l")
1-pbinom(5,100,0.10)

#or pbinom(5, 100, 0.10, lower.tail = FALSE)

#According to Leder et al. [2002], many airlines consistently report
#that about 12% of all booked passengers do not show up to the gate due to cancellations
#and no-shows. If an airline sells 110 tickets for a flight that seats 100 passengers, 
#what is the probability that the airline overbooked (sold more tickets than seats) 
#in terms of the number of ticket holders who show up?


#X be the number of ticket holders who arrive at the gate
##If we assume that passengers’ gate arrivals are independent, then X has a binomial distribution
##n = 110 and p = 1 − 0.12 = 0.88

###Overbooking occurs when more passengers show up than the number of available seats. 
#Thus, the desired probability is

plot(pbinom(0:200, 110, 0.88))
1-pbinom(100, 110, 0.88)
plot(dbinom(0:200, 110, 0.88))


##Poisson

##Examples of data from the poisson distribution
##The number of babies born on a maternity ward in one day.
##The number of blood cells recorded on a hemocytometer

##The Poisson distribution is a common model in genetics. The distribution
##is used to describe occurrences of mutations and chromosome crossovers.
##Crossovers occur when two chromosomes break and then reconnect at different end pieces resulting in an exchange of genes. This process is known as genetic recombination.

##Suppose a genetics lab has a means to count the number of crossovers between
#two genes on a chromosome. In 100 samples, 50 cells have no crossovers, 25 cells
#have one crossover, 20 cells have two crossovers, and 5 cells have three crossovers.

#Find the probability that a new sample will show at least one crossover.

#The average number of crossovers from the sample is

50*(0) + 25*(1) + 20*(2) + 5*(3)

##80 crossovers per 100 cells
##0.80 crossovers per cell

#Probability of no crossovers
dpois(0,0.80)

#probability of at least 1
1-dpois(0,0.80)


# Tenzin spends $2 in supplies to set up his lemonade stand. He
# charges 25 cents a cup. Suppose the number of cups he sells in a day has a Poisson
# distribution with 𝜆 = 10. Describe his profit as a function of a random variable and
# find the probability that the lemonade stand makes a positive profit.
#If he sells x cups, then his profit is 25x − 200 cents.

##The probability that Tenzin makes a positive profit is 200/25
1-ppois(8, 10)

##Lets simulate the data
reps <- 10000
simlist <- rpois(reps, 10)*25 - 200
plot(simlist)
mean(simlist)
median(simlist)
hist(simlist)



##Geometric

## Suppose a baseball player has a 30% chance of getting a hit during 
##any at bat. After three times at bat, the player has not had a hit. What is the
##probability the player will not get a hit after seven times at bat?

##The number of times at bat until getting a hit is a geometric random variable, X, with p = 0.3.

1-pgeom(3,0.3)

plot(1-pgeom(0:20,0.3))

##Hypergeometric

#Suppose there are 100 political independents in the student body of 1000. A sample of 50 students is picked. 
#What is the probability there will be six independents in the sample?


dhyper(6, 100, 900, 50)

#There are 500 deer in a wildlife preserve. A sample of 50 deer are caught and
#tagged and returned to the population. Suppose that 20 deer are caught later
#and examined to see if they are tagged.

#Find the probability that the sample contains at least three tagged deer.
#Find the mean of the number of tagged deer in the sample.


#x = 0:3
#m = 50
#n = 450
#k = 20
1-phyper(2,50,450,20)

plot(dhyper(0:10,50,450,20),type="l")
dear <-replicate(10000, sum(sample(1:500, 50)<=20))
mean(dear)
var(dear)
table(dear)/10000
fre=table(dear)/10000
sum(fre[4:10])

##Mean is
(50*20)/500


#In a bridge hand (13 cards from a standard 52-card deck), what is the mean and variance of the number of aces?
plot(dhyper(0:10,13,39,4),type="l")

aces <-replicate(10000, sum(sample(1:52, 13)<=4))
mean(aces)
var(aces)
(13*4)/52

##Exponential
# A school’s help desk receives calls throughout the day. The time T (in minutes) between calls is modeled with 
##an exponential distribution with mean 4.5. 
#A call just arrived. What is the probability no call will be received in the next 5 minutes?

#The parameter of the exponential distribution is 𝜆 = 1∕4.5. If no call is received
#in the next 5 minutes, then the time of the next call is greater than five. The desired
# probability is

1-pexp(5,1/4.5)

##Normal

#Babies’ birth weights are normally distributed with mean 𝜇 = 120
#and standard deviation 𝜎 = 20 ounces.What is the probability that a random baby’s
#birth weight will be greater than 140 ounces?

1-pnorm(140,120,20)

##About 16%

##Convert to a standardize normal distribution

##Standard normal distribution allows us to quickly estimate the probability of specific values 
##befalling in our distribution or compare data sets with varying means and standard deviations.

##z = (X – μ) / σ

#Among females in the US between the ages of 18 and 74, 
##diastolic blood pressure is normally distributed with mean 77 mm Hg and standard deviation of 11.6 mm Hg. 
##What is the probability that a randomly selected woman has a diastolic blood pressure less than 60 mm Hg? 
##greater than 90 mm Hg? between 60 and 90 mm Hg?
  
#  X ~ N(77, 134.56)

#1.
pnorm(60,77,11.6)
##P(Z < ((60 - 77)/11.6)) = 
##P(Z < -1.46551724137931) = 
##pnorm(-1.46551724137931) = 
##0.0713899259455208
#7% chance a randomly selected woman has diastolic bp less than 60mmHg

#2. greater than 90

1-pnorm(90,77,11.6)

#P(Z > ((90 - 77)/11.6)

##= P(Z < 1.12068965517241)
##= pnorm(1.12068965517241)
##= 1 - pnorm(1.12068965517241)
##= 0.131209993551487

#3.


(1-pnorm(60,77,11.6))-(1-pnorm(90,77,11.6))

.869-.071

#P(((60 - 77)/11.6) < Z < ((90 - 77)/11.6)) =
#(-1.46551724137931 < Z < 1.12068965517241) =
#(1 - 0.131209993551487) - 0.0713899259455208 = 
##0.797400080502992


