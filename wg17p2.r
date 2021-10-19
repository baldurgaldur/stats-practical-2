# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

report <- function(state_vector, day) {
    removed_at_start <- sum(state_vector == 3)
    infected_at_start <- sum(state_vector == 2)
    susceptible <- sum(state_vector == 0)
    exposed <- sum(state_vector == 1)
    print(c("No of susceptible ppl ", susceptible, "at beginning of day: ", i))
    print(c("No of exposed ppl ", exposed))
    print(c("No of infected ppl ", infected_at_start))
    print(c("No of removed ppl ", removed_at_start))

    #print("who is exposed?")
    #print(which(pop_states["state"] == 1))
    #print("who is infected?")
    #print(which(pop_states["state"] == 2))
}

update_states <- function(pop_states, lambda, i) {
    # One day in our model goes by.
    # Each row in population_states represent an individual in our model
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    p_leaving_infected <- 1 / 5
    p_leaving_exposed <- 1 / 3

    report(pop_states$state, i)

    #calculate the probability of person j moving into the exposed state
    infected_beta_sum <- sum(pop_states$beta[pop_states["state"] == 2]) * lambda
    infection_prob <- pop_states$beta * infected_beta_sum

    # We could random fewer and put them in the right place
    # Eg. no point in randoming for all the ppl in the removed state
    rand <- runif(pop_size)

    now_removed <- pop_states$state == 2 & rand < p_leaving_infected
    now_infected <- pop_states$state == 1 & rand < p_leaving_exposed
    now_exposed <- pop_states$state == 0 & rand < infection_prob

    # Combine the 3 vectors to determine which individuals will move up a state
    moving_states <- now_exposed + now_infected + now_removed

    new_states <- moving_states + pop_states$state
    data.frame(state = new_states, beta = pop_states$beta)
}

#TODO: Do this
calc_state_changes <- function(pop_states, new_pop, daily_state_changes, i) {
    
    removed_yesterday <- sum(pop_states["state"] == 3)
    infected_yesterday <- sum(pop_states["state"] == 2)

    susceptible <- sum(pop_states["state"] == 0)
    exposed <- sum(pop_states["state"] == 1)
    
    removed_new <- sum(new_pop[,1] == 3)
    infected_new <- sum(new_pop[,1] == 2)


    difference_in_removed <- removed_new - removed_yesterday
    print(c("Removed today", difference_in_removed))

    new_infections <- (infected_new - infected_yesterday) + difference_in_removed
    print(c("Infected today", new_infections))
    
    daily_state_changes[i, 1] <- new_infections
    daily_state_changes[i, 2] <- difference_in_removed
    
    
}


## Constants
# Population of Scotland
pop_size <- 5500000
simulation_days <- 3
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

daily_state_changes <- matrix(data = 0, nrow = simulation_days, ncol = 2)

for (i in 1:simulation_days) {
    print(system.time(new_states <- update_states(pop_states, lambda, i)))
    print("one day time above")
    if (i > 1){
        calc_state_changes(pop_states, new_states, daily_state_changes, i)
    }
    pop_states <- new_states 
}

print("Pandemic over")