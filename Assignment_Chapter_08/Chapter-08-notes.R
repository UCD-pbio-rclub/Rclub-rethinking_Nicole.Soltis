#notes Chapter 8

#8.1
#simulate markov chain -- King Markov's island nation
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
  # record current position (vector of island positions)
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  # move?
  prob_move <- proposal/current
  current <- ifelse( runif(1) < prob_move , proposal , current )
}

#8.2
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

#8.3
#old method: fit interaction model 
#predict log-gdp using terrain ruggedness, continent, interaction
m8.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ) ,
  data=dd )
precis(m8.1)

#8.4
#next: fit model using HMC
#preprocess variable transformations
#prepare the dataframe for rstan: remove extra columns
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim) #x

#8.5
#new method: use rstan to sample from posterior
#using a half-cauchy prior instead of uniform prior for sigma
  #thick-tailed probability distribution: weakly regularizing for standard deviations
library(rethinking)
m8.1stan <- map2stan(
                      alist(
                        log_gdp ~ dnorm( mu , sigma ) ,
                        mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
                        a ~ dnorm(0,100),
                        bR ~ dnorm(0,10),
                        bA ~ dnorm(0,10),
                        bAR ~ dnorm(0,10),
                        sigma ~ dcauchy(0,2)
                      ) ,
                      data=dd.trim )


#8.6 compare estimates from the map2stan estimate
precis(m8.1stan)

#8.7
#draw additional samples from map2stan
#run 4 independent chains of model 8.1 and distribute them to separate processors
#cores option automatically parallelizes chains
m8.1stan_4chains <- map2stan( m8.1stan , chains=4 , cores=4 )
precis(m8.1stan_4chains)

#8.8
#plot the samples- look at how Gaussian (quadratic) the posterior density is
post <- extract.samples( m8.1stan )
str(post)

#8.9  plot all samples at once
pairs(post)

#8.10 use pairs directly on fit model
#displays parameter names and correlations
#quadratic approximation does almost as well as HMC
pairs(m8.1stan)

#8.11 extract DIC and WAIC
DIC(m8.1stan)
WAIC(m8.1stan)
show(m8.1stan)

#8.12 trace plot shows a healthy chain
#healthy chain: stationarity and good mixing
plot(m8.1stan)
plot(post$a, type="l")

#8.13 example of "wild chain" - broad, flat region of posterior density
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )

#8.14 precis output
#weird values, huge CIs
precis(m8.2)
post8.2 <- extract.samples( m8.2 )
plot(post8.2$alpha, type="l")

#8.15  now with weakly informative priors
m8.3 <- map2stan(
                  alist(
                    y ~ dnorm( mu , sigma ) ,
                    mu <- alpha ,
                    alpha ~ dnorm( 1 , 10 ) ,
                    sigma ~ dcauchy( 0 , 1 )
                  ) ,
                  data=list(y=y) , start=list(alpha=0,sigma=1) ,
                  chains=2 , iter=4000 , warmup=1000 )
precis(m8.3)
post8.3 <- extract.samples( m8.3 )
plot(post8.3$alpha, type="l") #that looks better!!
#stationary chains and good mixing

#8.16 cauchy sampling: mean trace unpredictable due to thick tails
y <- rcauchy(1e4,0,5)
mu <- sapply( 1:length(y) , function(i) sum(y[1:i])/i )
plot(mu,type="l")

#8.17 example of non-identifiable model
y <- rnorm( 100 , mean=0 , sd=1 )

#8.18 run markov chain on unidentifiable models
#can only estimate SUM of a1 and a2.
m8.4 <- map2stan(
                  alist(
                    y ~ dnorm( mu , sigma ) ,
                    mu <- a1 + a2 ,
                    sigma ~ dcauchy( 0 , 1 )
                  ) ,
                  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
                  chains=2 , iter=4000 , warmup=1000 )
precis(m8.4)

#8.19 weak priors to the rescue!
m8.5 <- map2stan(
                  alist(
                    y ~ dnorm( mu , sigma ) ,
                    mu <- a1 + a2 ,
                    a1 ~ dnorm( 0 , 10 ) ,
                    a2 ~ dnorm( 0 , 10 ) ,
                    sigma ~ dcauchy( 0 , 1 )
                  ) ,
                  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
                  chains=2 , iter=4000 , warmup=1000 )
precis(m8.5)