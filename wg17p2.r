# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

# Our population size.
n <- 5500000

population <- rep(0, n)
beta <- rlnorm(n, 0, 0.5)
beta_prob <- beta / mean(beta)
lambda <- 0.4 / n

gamma <- 1.4 / n # = daily prob of going from S to E
delta <- 1 / 3 # = daily prob of going from E to I
theta <- 1 / 5 # = daily prob of going from I to R

#initialize suceptibles
x <- rep(0, n)

#create 10 exposed
x[1:10] <- 1

#STorage for pop in each stage for 100 days
S <- E <- I <- R <- rep(0, 100)