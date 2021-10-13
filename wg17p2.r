# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

# Our population size.
n <- 5500000

population <- rep(0, n)
beta <- rlnorm(n, 0, 0.5)
beta_prob <- beta / mean(beta)
lambda <- 0.4 / n