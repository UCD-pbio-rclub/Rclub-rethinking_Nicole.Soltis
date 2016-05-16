#Chapter 06 Notes

#Nicole E Soltis
#----------------------------------------
#6.1
#example of overfitting: average brain vol and body mass for 7 hominin spp
#building a dataframe from scratch (3 vectors)
sppnames <- c( "afarensis","africanus","habilis","boisei", 
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )
#to what extent do species have brains that are larger than we'd expect after taking body size into account?
#linear regression: brain size as body size - "statistical control" strategy

#use lm to fit models vs. map (p. 159) and polynomial regression (p. 110)

#6.2
m6.1 <- lm( brain ~ mass , data=d )

#6.3
#R2 : proportion of variance explained by the model
#remaining variation once model has accounted for some of the variation == variation of the residuals
1 - var(resid(m6.1))/var(d$brain)
#also seen in:
summary(m6.1)

#6.4 increasingly complex models: second degree (added parameter B2)
m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )

#6.5: third degree to sixth degree models
#as degree increases, fit always improves
#but the model is increasingly ridiculous
#when degree = n, fit is perfect : 1 parameter per observation. ZERO data compression.
m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3) , data=d )
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) ,
            data=d )
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) , data=d )
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) + I(mass^6) , data=d )

#6.6
#simplest model ignoring mass
m6.7 <- lm( brain ~ 1 , data=d )
#underfitting: inaccurate within AND out-of-sample

#6.7
#drop one observation at a time
#overfit models highly sensitive to dropped data points
#underfit models insensitive to the sample
d.new <- d[ -i , ]

#6.8
#loop regression over rows: drop one row (observation) at a time
plot( brain ~ mass , d , col="slateblue" )
for ( i in 1:nrow(d) ) {
  d.new <- d[ -i , ]
  m0 <- lm( brain ~ mass, d.new )
  abline( m0  )
}

#plot the multivariate fit
plot( brain ~ mass , d , col="slateblue" , ylim = c(-500,3000))
for ( i in 1:nrow(d) ) {
  d.new <- d[ -i , ]
  m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
                I(mass^5) + I(mass^6) , data=d.new )
  minMax = range(d.new$mass)
  xVals = seq(minMax[1], minMax[2], len = 100)
  yVals = predict(m6.6, newdata = data.frame(mass=xVals))
  lines(xVals,yVals)
}

#6.9
#information: the reduction in uncertainty derived from learning an outcome
#information entropy: the uncertainty in a probability distribution is average log-probability of an event
#calculate information entropy for 2 events with p = 0.3, 0.7
#uncertainty (info entropy) is lower if one event is very rare -- less uncertainty for any given observation
p <- c( 0.3 , 0.7 )
-sum( p*log(p) )
#events that never happen drop out (H -> 0)
#divergence: additional uncertainty by using probabilities from one distribution to describe another distribution
#divergence: average difference in log probability between target and model = difference between two entropies

#6.10
#deviance: a measure of model fit from divergence
#deviance summed across all cases
# fit model with lm
m6.1 <- lm( brain ~ mass , d )
# compute deviance by cheating
(-2) * logLik(m6.1)

#6.11
#example of computing deviance
# standardize the mass before fitting
library(rethinking)
d$mass.s <- (d$mass-mean(d$mass))/sd(d$mass)
m6.8 <- map(
  alist(
    brain ~ dnorm( mu , sigma ) ,
    mu <- a + b*mass.s
  ) ,
  data=d ,
  start=list(a=mean(d$brain),b=0,sigma=sd(d$brain)) ,
  method="Nelder-Mead" )
# extract MAP estimates ... log-likelihood for each observation
theta <- coef(m6.8)
# compute deviance : sum * -2
dev <- (-2)*sum( dnorm(
  d$brain ,
  mean=theta[1]+theta[2]*d$mass.s ,
  sd=theta[3] ,
  log=TRUE ) )
dev

# alternatively, compute deviance by cheating
(-2) * logLik(m6.8)

#6.12
#simulated model trainging and testing
N <- 20 
kseq <- 1:5
dev <- sapply( kseq , function(k) {
  print(k);
  r <- replicate( 1e4 , sim.train.test( N=N, k=k ) );
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )

#6.13: parallel simulations if on Linux
#mc.cores: use multiple processor cores
r <- mcreplicate( 1e4 , sim.train.test( N=N, k=k ) , mc.cores=4 )

#6.14 plot the simulation
plot( 1:5 , dev[1,] , ylim=c( min(dev[1:2,])-5 , max(dev[1:2,])+10 ) ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5)+0.1 , dev[2,] )
for ( i in kseq ) {
  pts_in <- dev[1,i] + c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1,+1)*dev[4,i]
  lines( c(i,i) , pts_in , col=rangi2 )
  lines( c(i,i)+0.1 , pts_out )
}

# section 6.3, 6.4
#6.15
#demonstration of WAIC calculations
#regression fit with MAP
data(cars)
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars )
post <- extract.samples(m,n=1000)


#6.16
#log-likelihood of each observation i at each sample s from the posterior:
n_samples <- 1000 
ll <- sapply( 1:n_samples ,
              function(s) {
                mu <- post$a[s] + post$b[s]*cars$speed
                dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
              } )

#6.17
#now compute Bayesian deviance (lppd)
n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(ll[i,]) - log(n_samples) )

#6.18 pWAIC: effective number of parameters
#compute variance across samples for each observation, then add those values
pWAIC <- sapply( 1:n_cases , function(i) var(ll[i,]) )

#6.19 compute WAIC
-2*( sum(lppd) - sum(pWAIC) )

#6.20 compute standard error of WAIC
waic_vec <- -2*( lppd - pWAIC )
sqrt( n_cases*var(waic_vec) )

#Section 6.5
#6.21
library(rethinking)
data(milk)
#remove NA values: compared models must be fit to exactly the same observations
d <- milk[ complete.cases(milk) , ]
#rescale neocortex
d$neocortex <- d$neocortex.perc / 100
dim(d)
#17 cases, 9 variables

#6.22
#predict kilocalories per gram of milk
#fit four different models
#and: constrain sd of outcome to be positive
a.start <- mean(d$kcal.per.g) 
sigma.start <- log(sd(d$kcal.per.g))
#first model: intercept only
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm( a , exp(log.sigma) )
  ) ,
  data=d , start=list(a=a.start,log.sigma=sigma.start) )
#second model: neocortex only
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex
  ) ,
  data=d , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
#third model: log mass only
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bm*log(mass)
  ) ,
  data=d , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
#fourth model: neocortex and log mass
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex + bm*log(mass)
  ) ,
  data=d , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

#6.23
WAIC( m6.14 )

#6.24
( milk.models <- compare( m6.11 , m6.12 , m6.13 , m6.14 ) )

#6.25
plot( milk.models , SE=TRUE , dSE=TRUE )

#6.26
diff <- rnorm( 1e5 , 6.7 , 7.26 )
sum(diff<0)/1e5

#6.27
coeftab(m6.11,m6.12,m6.13,m6.14)

#6.28
plot( coeftab(m6.11,m6.12,m6.13,m6.14) )

#6.29
# compute counterfactual predictions
# neocortex from 0.5 to 0.8
nc.seq <- seq(from=0.5,to=0.8,length.out=30)
d.predict <- list(
  kcal.per.g = rep(0,30), # empty outcome
  neocortex = nc.seq, # sequence of neocortex
  mass = rep(4.5,30) # average mass
)
pred.m6.14 <- link( m6.14 , data=d.predict )
mu <- apply( pred.m6.14 , 2 , mean )
mu.PI <- apply( pred.m6.14 , 2 , PI )
# plot it all
plot( kcal.per.g ~ neocortex , d , col=rangi2 )
lines( nc.seq , mu , lty=2 )
lines( nc.seq , mu.PI[1,] , lty=2 )
lines( nc.seq , mu.PI[2,] , lty=2 )

#6.30
milk.ensemble <- ensemble( m6.11 , m6.12 , m6.13 , m6.14 , data=d.predict )
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI )
lines( nc.seq , mu )
shade( mu.PI , nc.seq )