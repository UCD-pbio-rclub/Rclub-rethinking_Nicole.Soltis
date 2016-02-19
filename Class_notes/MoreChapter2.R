#example 2.3

# define grid
#20 is an arbitrary choice
p_grid <- seq( from=0 , to=1 , length.out=20 )
p_grid <- seq( from=0 , to=1 , by=0.2) #another option, gives 5 points

# define prior
prior <- rep( 1 , 20 ) #how many times to repeat
prior
#p grid is ~ various weightings of the coin

# compute likelihood at each value in grid
#likelihood of get 6 waters out of 9 tosses assuming TRUE proportion of water is... (value from p_grid)
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
plot(p_grid, likelihood)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior #r interprets this for you
#prior was = 1 for all, so unstd.posterior = likelihood

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior, type="b")

#section 2.5
#now, changing the prior
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
plot(p_grid, prior, type="l")
prior <- exp( -5*abs( p_grid - 0.5 ) )
plot(p_grid, prior, type="l")

#section 2.6
#not always going to use a binomial distribution for the likelihood
library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(w=6) )
# display summary of quadratic approximation
precis( globe.qa )
