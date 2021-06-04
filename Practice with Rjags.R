# Info ----
#Author: Victor Feagins
#This script is my first attempt at using Rjags. I will try do some modeling.

# Packages ----
library(rjags) #Uses JAGS to create bayesian models
library(coda)
library(tidyverse) #Utility functions

# Functions ----
Examine <- function(vector){
  #To examine the variable to grab info from it
  H <- head(vector)
  C <- class(vector)
  S <- summary(vector)
  M <- sum(is.na(vector))
  D <- sum(duplicated(vector))
  print("First 6 obs")
  print(H)
  print(S)
  print(paste("Class ", C))
  print(paste("Missing ", M))
  print(paste("Duplicates", D))
  
}
ScatterPlot <- function(data, x, y){
  ggplot(data, mapping = aes(x, y))+
    geom_point()+
    geom_smooth() +
    xlab(deparse(substitute(x)))+
    ylab(deparse(substitute(y)))
}

# Classical Modeling ----
#The following will be a typical modeling situation. Fitting a linear regression 
#with continuous variables the data set I will be using is the Auto dataset from
#the ISLR package

## Data ----
df.A <- ISLR::Auto #A dataset that contains various variables related to cars

attach(df.A) #This allows me to avoid writing the df.A$ notation and can call variables as is

## Exploring Data ----
#With the Auto dataset we will try to model Miles per gallon (mpg) for a car.
pairs(df.A) #Get a glimpse of the behavior of the data
#We can see that there are variables that seem to have a linear relationship with
#mpg. Mainly displacement, horsepower, and weight. We will focus on these variables.


### mpg ----
#miles per gallon
Examine(mpg)
# We can see no missing data. 
# The range is 9 - 46.60 with a mean of 23.45

hist(mpg)
# In the histogram we see the data seems to have right skewness

### displacement ----
#Engine displacement (cu. inches)
Examine(displacement)
# No missing data.
# The range is 68 - 455 with a mean of 194.4

hist(displacement)
#Very right skewed 

#### Relationship displacement ----

ScatterPlot(data = df.A, x = displacement, y = mpg)
cor(displacement, mpg)
# The relationship between displacement and mpg is negative and not linear.


### horsepower ----
#Engine horsepower
Examine(horsepower)
#No missings
#Ranges 46 - 230 with a mean of 104.
hist(horsepower)
#Again right skewed

#### Relationship horsepower ----
ScatterPlot(df.A, horsepower, mpg)
cor(horsepower, mpg)
# The relationship between horsepower and mpg is negative and not linear.

### weight ----
#Vehicle weight (lbs.)
Examine(weight)
#No Missings
# Range 1613 - 5140 with mean 2978
##### Relationship weight ----

ScatterPlot(df.A, weight, mpg)
cor(weight, mpg)

# The relationship between weight and mpg is negative and looks pretty linear.


## Modeling ----
# From our variables displacement, horsepower and weight they seem all good variables to explain
# mpg.

### Setting up Rjags ----
#### Uninformative priors ----

#This model show uninformative priors. 

df.A.model <- "model {
#Model
for (i in 1:length(Y)) {
Y[i] ~ dnorm(mu[i], s^(-2))
mu[i] <- b0 + b1*x1[i] + b2*x2[i] + b3*x3[i]
}

#Prior
b0 ~ dnorm(0, 200^(-2))
b1 ~ dnorm(0, 200^(-2))
b2 ~ dnorm(0, 200^(-2))
b3 ~ dnorm(0, 200^(-2))

s ~ dunif(0,200)

}"

#### Data ----

A.data <- list(
  Y = mpg,
  x1 = displacement,
  x2 = horsepower,
  x3 = weight
)

#### Initial ----
A.n.chains <- 3
A.inital.coef <- coef(lm(mpg ~ displacement + horsepower + weight))
A.inits = list()
for(i in 1:A.n.chains){
  A.inits[[i]] <- list(b0 = rnorm(1,A.inital.coef['(Intercept)'],200),
                     b1 = rnorm(1,A.inital.coef["displacement"], 200),
                     b2 = rnorm(1,A.inital.coef["horsepower"], 200),
                     b3 = rnorm(1,A.inital.coef["weight"], 200),
                     .RNG.name = "base::Wichmann-Hill",
                     .RNG.seed = 10)
}





### Running JAGS ----

df.A.jags <- jags.model(
  file = textConnection(df.A.model),
  data = A.data,
  inits = A.inits,
  n.chains = A.n.chains
)


### Drawing from posterior
A.n.iter <- 100000

df.A.sim <- coda.samples(
  model = df.A.jags,
  variable.names = c("b0", "b1", 'b2', 'b3', 's'),
  n.iter = A.n.iter
)


### Diagnostics ----

plot(df.A.sim)
#I am curious if it looks better just because it has more points so can't see the detail.
plot(window(df.A.sim, A.n.iter-1000))
# When looking at the small window it is not converging. 


summary(df.A.sim)

# we see the coefficients are negative which agree with our preliminary data. 
gelman.diag(df.A.sim)
gelman.plot(df.A.sim)


