Black-Sholes Binom Approximation ReadMe

This project uses Binomial distributions and Markov chains to approximate Black-Sholes continuous option pricing model 
in discrete time (the European version). Instead of integrating over continuous time until the maturity date, my
model divides this total time into n individual pieces, and then runs a binomial distribution weighted by the amount
of money earned by the option for each of these n chunks (and adds them to get the final price). This result is then 
adjusted for depreciation and compared to the result of the actual Black-Sholes model (with my model and the real one
being given the same parameters). 

The goal of this project was to show that as n (the number of chunks the time until expiration is divided into) increases, 
my binomial model result gets closer and closer to the result of the real Black-Sholes equation. The graph on the RShiny
website shows this with a visual example, since as you drag the slider to increase n the light blue line representing
my model gets closer and closer to the real result (dark blue dotted line). 
