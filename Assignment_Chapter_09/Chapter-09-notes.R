#Chapter 09 notes

library("rethinking")

#9.1 list of pebble distributions (10 pebbles, 5 buckets)
p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)

#9.2 normalize list to make probability distributions
p_norm <- lapply( p , function(q) q/sum(q))

#9.3 compute information entropy of each
( H <- sapply( p_norm , function(q) -sum(ifelse(q==0,0,q*log(q))) ) )

#9.4 compute log ways per pebble for each distribution
#information entropy approximates this!
ways <- c(1,90,1260,37800,113400)
logwayspp <- log(ways)/10

#9.5 number of ways to draw blue or white marbles
# build list of the candidate distributions
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)
# compute expected value of each
sapply( p , function(p) sum(p*c(0,1,1,2)) )
#9.6
# compute entropy of each distribution
sapply( p , function(p) -sum( p*log(p) ) )

#9.7 binomial distribution if 7 blue marbles
p <- 0.7
( A <- c( (1-p)^2 , p*(1-p) , (1-p)*p , p^2 ) )

#9.8 information entropy of the distribution
-sum( A*log(A) )

#9.9 simulate random probability distributions with any expected value
sim.p <- function(G=1.4) { 
  x123 <- runif(3)
  x4 <- ( (G)*sum(x123)-x123[2]-x123[3] )/(2-G)
  z <- sum( c(x123,x4) )
  p <- c( x123 , x4 )/z
  list( H=-sum( p*log(p) ) , p=p )
}

#9.10 invoke function 100k times and plot distribution of entropies
H <- replicate( 1e5 , sim.p(1.4) )
dens( as.numeric(H[1,]) , adj=0.1 )

#9.11 split out entropies and distributions
entropies <- as.numeric(H[1,])
distributions <- H[2,]

#9.12 largest observed entropy
max(entropies)

#9.13 the distribution with the max entropy
#almost exactly binomial distribution!
distributions[ which.max(entropies) ]
