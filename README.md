# Probably Maybe

An Elm webpage to convert between probability representations.

[See it here](https://www.martelle.dev/probably-maybe/) | [Github](https://github.com/Mcmartelle/probably-maybe)

In high availability systems the concept of uptime in terms of ["Nines"](https://en.wikipedia.org/wiki/High_availability#%22Nines%22) refers to the number of nines in the uptime percentage, for example 99.999% is 5 nines. The "Nines" representation of probability is easier to understand than standard deviation or [Six Sigma](https://en.wikipedia.org/wiki/Six_Sigma). However, I wanted to represent probabilities with the number of coin flips in a row, or dice rolls to meet a given probability.

I assumed the math would involve a log somehow. It ended up being just one log, but the base of the log depends on the number of sides on the coin or dice. So calculating the number of rolls for a 6 sided die is log base 6 of the 1 in x chance.

Another interesting probability that I added later is the number of rolls of a given 1 in x chance to have a certain chance of having at least one occurance. These number are much harder for my human brain to predict. For example it takes about 7 rolls of a 1 in 10 chance to have a 50% chance of at least 1 occurance. I would assume the number would be closer to 5 rolls, since 5 is 50% of 10, but that is not the case. It seems to be a helpful way to bridge the gap between statistics of huge sample sizes and the statistics of smaller everyday life sample sizes.
