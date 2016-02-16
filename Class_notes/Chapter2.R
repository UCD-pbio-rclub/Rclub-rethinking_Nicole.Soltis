#Book Examples Ch 2

#2.2
models <- seq( from=0 , to=1 , length.out=20 ) 
prior <- rep( 1 , 20 )
likelihood <- dbinom( 6 , size=9 , prob=models )
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
plot(posterior)

#2.3
prior <- ifelse( models < 0.5 , 0 , 1 )
prior <- exp( -5*abs( models - 0.5 ) )

#2.4
library(rethinking)
globe.qa <- map(
                list(
                    nw ~ dbinom( prob=p , size=9 ) ,
                    p ~ dunif(0,1)
                    ) ,
                data=list(nw=6) , start=list(p=0.5) )
precis( globe.qa )

#2.5
dbinom( 6 , prob=2 , size=9 )
