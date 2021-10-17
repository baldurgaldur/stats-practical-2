# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

pass_one_day <- function(pop_states, daily_state_changes, lambda, i) {
    p_leaving_infected <- 1 / 5
    p_leaving_exposed <- 1 / 3
    # One day in our model goes by.
    # Each row in population_states represent an individual in our model
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    removed_at_start <- sum(pop_states["state"] == 3)
    infected_at_start <- sum(pop_states["state"] == 2)
    susceptible <- sum(pop_states["state"] == 0)
    exposed <- sum(pop_states["state"] == 1)
    print(c("No of susceptible ppl ", susceptible, "at time: ", i))
    print(c("No of exposed ppl ", exposed))
    print(c("No of infected ppl ", infected_at_start))
    print(c("No of removed ppl ", removed_at_start))

    random <- runif(n = length(pop_states))

    # If we do not do this in "reverse", ie resolve the later states first,
    # we run the risk of a person going over more than one state in a single day
    pop_states[, "state" == 2 & random < p_leaving_infected] <- 3
    pop_states[, "state" == 1 & random < p_leaving_exposed] <- 2

    # Because individuals do not immediately enter the infected state,
    # we can use the same no_of_infected for each person.
    # One could argue that we should use mean of infected at beginning and infected at end of day.
    # I chose beginning here.

    #TODO: SUM the betas of the infected instead of counting infected
    infected_beta_sum <- sum(pop_states$beta[pop_states["state"] == 2]) * lambda
    print(c("infected beta sum", infected_beta_sum))
    infection_prob <- pop_states[, "beta"] * infected_beta_sum
    pop_states[, "state" == 0 & random < infection_prob] <- 1

    removed_at_end <- sum(pop_states["state"] == 3)
    infected_at_end <- sum(pop_states["state"] == 2)

    removed_today <- removed_at_end - removed_at_start
    print(c("Removed today", removed_today))

    infected_today <- (infected_at_end - infected_at_start) + removed_today
    # This is not very R, find a way to set the whole row
    daily_state_changes[i, 1] <- infected_today
    daily_state_changes[i, 2] <- removed_today
}

## Constants
# Population of Scotland
pop_size <- 5500000
simulation_days <- 2
lambda <- 0.4 / pop_size
beta <- rlnorm(pop_size, 0, 0.5)
beta_prob <- beta / mean(beta)

# An appropriate data structure to monitor each person is a data frame.
# It has the "state" column and the "beta" column corresponding to a single
# individuals _current_ state and beta value. The states in our model are:
# 0:= Susceptible, 1:= Exposed, 2:= Infected, 3:= Removed(Recovered or dead)
pop_states <- data.frame(state = rep(0, pop_size), beta = beta_prob)

# Prime the model by choosing 10 random ppl to be exposed
starting_exposed <- sample(1:pop_size, 10)
pop_states[starting_exposed, "state"] <- 1

daily_state_changes <- matrix(nrow = simulation_days, ncol = 2)
for (i in 1:simulation_days) {
   pass_one_day(pop_states, daily_state_changes, lambda, i)
}

str(daily_state_changes)
print(daily_state_changes)
# simulation_days are over. Report


gamma <- 1.4 / n # = daily prob of going from S to E
delta <- 1 / 3 # = daily prob of going from E to I
theta <- 1 / 5 # = daily prob of going from I to R

#STorage for pop in each stage for 100 days
S <- E <- I <- R <- rep(0, 100)

#set up vector of beta and states
states <- matrix(c(beta_prob, rep(0, n)), n, 2)
states[1:10, 2] <- 1

                 