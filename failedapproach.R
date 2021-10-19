SEIR <- function(pop_size = 5500000, exposed = 10, days = 100, gamma = 1/3, delta = 1/5, lambda = 0.4/5500000) {
  states <- rep(0, pop_size) #vector for storage (all in susceptible)
  beta <- rlnorm(pop_size,0,0.5); beta <- beta/mean(beta)
  states[1:exposed] <- 1
  S <- E <- I <- R <- new_I <-rep(0,days)#storage vector for daily totals of each state
  S[1] <- pop_size - exposed #number susceptible on day 1
  E[1] <- exposed #number exposed on day 1

  
  #loop over the days
  for (i in 2:days) {
    u <- runif(pop_size)
    states[states == 2 & u < delta] <- 3 #I to R with prob delta
    states[states == 1 & u < gamma] <- 2 #E to I with prob gamma
    states_beta <- cbind(states, beta)
    infected_beta <- states_beta[states_beta[states] == 2, ]
    infected_beta_sum <- sum(infected_beta[, 2])
    prob_S_E <- beta * lambda * infected_beta_sum
    states[states == 0 & u < prob_S_E] <- 1 #S to E with prob beta*lambda*infected_beta_sum
    S[i] <- sum(states == 0)
    E[i] <- sum(states == 1)
    I[i] <- sum(states == 2)
    R[i] <- sum(states == 3)
    new_I[i-1] <- (I[i]-I[i-1]) - (R[i]-R[i-1]) #this isnt right, sorry
  }
  list(S=S, E=E, I=I, R=R, new_I=new_I)
}

SEIR()




