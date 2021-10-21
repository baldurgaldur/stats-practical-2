# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

update_states <- function(pop_states, lambda, i) {
    # One day in our model goes by.
    # Each row in population_states represent an individual in our model
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.

    #calculate the probability of person j moving into the exposed state
    infected_beta_sum <- sum(pop_states$beta[pop_states$state == 2]) * lambda

    infection_prob <- pop_states$beta * infected_beta_sum

    rand <- runif(pop_size)

    now_removed <- pop_states$state == 2 & rand < p_leaving_infected
    now_infected <- pop_states$state == 1 & rand < p_leaving_exposed
    now_exposed <- pop_states$state == 0 & rand < infection_prob

    # Combine the 3 vectors to determine which individuals will move up a state
    moving_states <- now_exposed + now_infected + now_removed

    new_states <- moving_states + pop_states$state
    data.frame(state = new_states, beta = pop_states$beta, rand = pop_states$rand)
}

day_passed <- function(lambda, beta_prob, susceptible, exposed, infected, removed) {

    rand <- runif(pop_size)
    infected_beta_sum <- sum(beta_prob[infected]) * lambda
    infection_prob <- beta_prob * infected_beta_sum

    ppl_to_remove <- infected[rand[infected] < p_leaving_infected]
    ppl_to_be_infected <- exposed[rand[exposed] < p_leaving_exposed]

    ppl_to_expose <- susceptible[rand[susceptible] < infection_prob[susceptible]]

    removed <- c(removed, ppl_to_remove)
    infected <- infected[! infected %in% ppl_to_remove]

    infected <- c(infected, ppl_to_be_infected)
    exposed <- exposed[! exposed %in% ppl_to_be_infected]

    exposed <- c(exposed, ppl_to_expose)
    # Remove ppl_to_expose from susceptible
    susceptible <- susceptible[! susceptible %in% ppl_to_expose]

    list(susceptible = susceptible, exposed = exposed, infected=infected, removed=removed)
}

#TODO: Do this
calc_state_changes <- function(pop_states, new_pop, daily_state_changes, i, pop_size) {
    
    susceptible_yesterday <- sum(pop_states$state == 0)
    removed_yesterday <- sum(pop_states$state == 3)

    susceptible_today <- sum(new_pop$state == 0)
    removed_today <- sum(new_pop$state == 3)

    new_infections <- susceptible_yesterday - susceptible_today
    total_infections <- pop_size - susceptible_today - removed_today

    daily_state_changes[i, 1] <- susceptible_today
    daily_state_changes[i, 2] <- removed_today
    daily_state_changes[i, 3] <- new_infections
    daily_state_changes[i, 4] <- total_infections

    daily_state_changes
}

## Constants
# Population of Scotland
pop_size <- 5500000
cautious_pop_size <- pop_size/10
rand_pop_size <- pop_size/1000
simulation_days <- 150
lambda <- 0.4 / pop_size
beta <- rlnorm(pop_size, 0, 0.5)
beta_prob <- beta / mean(beta)
p_leaving_infected <- 1 / 5
p_leaving_exposed <- 1 / 3


#Experiment
everybody <- c(1:pop_size)
exposed <- sample(everybody, 10, replace= FALSE)
susceptible <- everybody[-exposed]
print(head(susceptible, 10))
print(length(susceptible))
infected <- vector()
removed <- vector()

#print(system.time(for(i in 1:simulation_days) {
#    new_day <- day_passed(lambda, beta_prob, susceptible, exposed, infected, removed)
#    susceptible <- new_day$susceptible
#    exposed <- new_day$exposed
#    infected <- new_day$infected
#    removed <- new_day$removed
#}))
print("Done")
print("no of exposed:")
print(length(exposed))
print("no of susceptible:")
print(length(susceptible))
print("no of infected:")
print(length(infected))
print("no of removed:")
print(length(removed))

print("no of susceptible:")
#finds the lowest 10% of betas and the indicies of the corresponding people
ordered_beta <- sort(beta_prob)
cautious_beta <- ordered_beta[c(1: pop_size/10)]
cautious_index <- match(cautious_beta, beta_prob)

max_beta <- ordered_beta[pop_size/10]
max_rand <- pop_size/1000

# An appropriate data structure to monitor each person is a data frame.
# It has the "state" column and the "beta" column corresponding to a single
# individuals _current_ state and beta value. The states in our model are:
# 0:= Susceptible, 1:= Exposed, 2:= Infected, 3:= Removed(Recovered or dead)
random <- sample(c(1:pop_size), pop_size, replace= FALSE)

pop_states <- data.frame(state = rep(0, pop_size), beta = beta_prob, rand = random )


# Prime the model by choosing 10 random ppl to be exposed
starting_exposed <- sample(1:pop_size, 10)
pop_states[starting_exposed, "state"] <- 1

#creates data frames for state_changes for the 3 population samples
daily_state_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
cautious_daily_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
random_samp_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)

print(system.time(for (i in 1:simulation_days) {
    
    new_states <- update_states(pop_states, lambda, i)

    #daily_state_changes <- calc_state_changes(pop_states, new_states, daily_state_changes, i, pop_size)

    #finds the cautious people in pop_states and new_states
    #cautious_pop <- pop_states[pop_states$beta <= max_beta,]
    #cautious_new <- new_states[pop_states$beta <= max_beta,]

    #cautious_daily_changes <- calc_state_changes(cautious_pop, cautious_new, cautious_daily_changes, i, cautious_pop_size)

    #finds people in the random 0.1% sample

    #rand_pop <- pop_states[pop_states$rand <= max_rand,]
    #rand_new <- new_states[new_states$rand <= max_rand,]

    #random_daily_changes <- calc_state_changes(rand_pop, rand_new, random_samp_changes, i, rand_pop_size)
    
    pop_states <- new_states
    
}))

print('above is time for whole program')
print("Susceptible:")
print(sum(pop_states$state == 0))
print("Exposed:")
print(sum(pop_states$state == 1))
print("Infected:")
print(sum(pop_states$state == 2))
print("Removed:")
print(sum(pop_states$state == 3))
print(daily_state_changes)
# print(cautious_daily_changes)
# print(random_daily_changes)
print("Pandemic over")