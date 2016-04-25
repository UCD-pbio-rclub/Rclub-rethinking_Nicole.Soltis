#5.1 fit a linear regression model of divorce, marriage age
# load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
#standardized units: can change the way you conceptualize your data
#if standardize 2 predictors, much easier to see relative effects of either. Particularly if orders of magnitude difference in units.
# standardize the predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
sd(d$MedianAgeMarriage)
# fit model
m5.1 <- map(
alist(
Divorce ~ dnorm( mu , sigma ) ,
mu <- a + bA * MedianAgeMarriage.s ,
a ~ dnorm( 10 , 10 ) ,
bA ~ dnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 10 )
) , data = d )

#5.2 plot the raw data, draw posterior mean regression line, draw shaded region
#sequence defines the range (in standardized units) over which we'll try to predict the model. This actually includes all data.
# compute percentile interval of mean
MAM.seq <- seq( from=-3 , to=3.5 , length.out=30 )
mu <- link( m5.1 , data=data.frame(MedianAgeMarriage.s=MAM.seq) )
mu.PI <- apply( mu , 2 , PI )
# plot it all
plot( Divorce ~ MedianAgeMarriage.s , data=d , col=rangi2 )
abline( m5.1 )
shade( mu.PI , MAM.seq )
precis(m5.1) #includes estimated slope and 89% CI for slope (under zero --> strong evidence for likely relationship)

#5.3 fit a regression for relationship between marriage rate and divorce rate
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
alist(
Divorce ~ dnorm( mu , sigma ) ,
mu <- a + bR * Marriage.s ,
a ~ dnorm( 10 , 10 ) ,
bR ~ dnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 10 )
) , data = d )
precis(m5.2)
plot(d$Marriage.s, d$Divorce)
abline(m5.2)

#compare to unstandardized!
#identical plots. slight slope variation likely due to randomness in model run
m5.2a <- map(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 1 , 5 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data = d )
precis(m5.2a)
plot(d$Marriage, d$Divorce)
abline(m5.2a)

#5.4 fit a map: effect of both marriage age and marriage rate
#combines the 2 previous models
#bR (marriage rate) crosses 0, no longer confident in negative interaction
m5.3 <- map(
alist(
Divorce ~ dnorm( mu , sigma ) ,
mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s ,
a ~ dnorm( 10 , 10 ) ,
bR ~ dnorm( 0 , 1 ) ,
bA ~ dnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 10 )
) ,
data = d )
precis( m5.3 )

#5.5 visualize the posterior distribution estimates of a, bR, bA, sigma
#including point estimates and percentile intervals
plot( precis(m5.3) )

#5.6 compute the model using just marriage age
m5.4 <- map(
alist(
Marriage.s ~ dnorm( mu , sigma ) ,
mu <- a + b*MedianAgeMarriage.s ,
a ~ dnorm( 0 , 10 ) ,
b ~ dnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 10 )
) ,
data = d )

#5.7
# compute expected value at MAP, for each State
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# compute residual for each State
#residual = observed - predicted
m.resid <- d$Marriage.s - mu

#5.8
plot( Marriage.s ~ MedianAgeMarriage.s , d , col=rangi2 )
abline( m5.4 )
# loop over States
for ( i in 1:length(m.resid) ) {
x <- d$MedianAgeMarriage.s[i] # x location of line segment
y <- d$Marriage.s[i] # observed endpoint of line segment
# draw the line segment
lines( c(x,x) , c(mu[i],y) , lwd=0.5 , col=col.alpha("black",0.7) )
}

#5.9
#counterfactual plots: effect of variation in each predictor on the model
#impossible data sets (can't manipulate humans)
# prepare new counterfactual data
A.avg <- mean( d$MedianAgeMarriage.s )
R.seq <- seq( from=-3 , to=3 , length.out=30 )
pred.data <- data.frame(
Marriage.s=R.seq,
MedianAgeMarriage.s=A.avg
)
# compute counterfactual mean divorce (mu)
mu <- link( m5.3 , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
# simulate counterfactual divorce outcomes
R.sim <- sim( m5.3 , data=pred.data , n=1e4 )

#5.10
R.avg <- mean( d$Marriage.s )
A.seq <- seq( from=-3 , to=3.5 , length.out=30 )
pred.data2 <- data.frame(
Marriage.s=R.avg,
MedianAgeMarriage.s=A.seq
)
mu <- link( m5.3 , data=pred.data2 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
A.sim <- sim( m5.3 , data=pred.data2 , n=1e4 )
A.PI <- apply( A.sim , 2 , PI )
plot( Divorce ~ MedianAgeMarriage.s , data=d , type="n" )
mtext( "Marriage.s = 0" )
lines( A.seq , mu.mean )
shade( mu.PI , A.seq )
shade( A.PI , A.seq )

#5.11
# call link without specifying new data
# so it uses original data
mu <- link( m5.3 )
# summarize samples across cases
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
# simulate observations
# again no new data, so uses original data
divorce.sim <- sim( m5.3 , n=1e4 )
divorce.PI <- apply( divorce.sim , 2 , PI )

#5.12
plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) ,
xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) )
lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
col=rangi2 )

#5.13
identify( x=d$Divorce , y=mu.mean , labels=d$Loc , cex=0.8 )

#5.14
# compute residuals
divorce.resid <- d$Divorce - mu.mean
# get ordering by divorce rate
o <- order(divorce.resid)
# make the plot
dotchart( divorce.resid[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
j <- o[i] # which State in order
lines( d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
points( d$Divorce[j]-c(divorce.PI[1,j],divorce.PI[2,j]) , rep(i,2),
pch=3 , cex=0.6 , col="gray" )
}

#5.15
N <- 100 # number of cases
x_real <- rnorm( N ) # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm( N , x_real ) # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real ) # y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur) # bind all together in data frame

#Sections 5.2 and 5.3 for 04/18/16