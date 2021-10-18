# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

update_states <- function(pop_states, lambda, i) {
    # One day in our model goes by.
    # Each row in population_states represent an individual in our model
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    p_leaving_infected <- 1 / 5
    p_leaving_exposed <- 1 / 3

    removed_at_start <- sum(pop_states["state"] == 3)
    infected_at_start <- sum(pop_states["state"] == 2)
    susceptible <- sum(pop_states["state"] == 0)
    exposed <- sum(pop_states["state"] == 1)
    print(c("No of susceptible ppl ", susceptible, "at time: ", i))
    print(c("No of exposed ppl ", exposed))
    print("who is exposed?")
    print(which(pop_states["state"] == 1))
    print("who is infected?")
    print(which(pop_states["state"] == 2))
    print(c("No of infected ppl ", infected_at_start))
    print(c("No of removed ppl ", removed_at_start))

    #calculate the probability of person j moving into the exposed state
    infected_beta_sum <- sum(pop_states$beta[pop_states["state"] == 2]) * lambda
    #print(c("infected beta sum", infected_beta_sum))
    infection_prob <- pop_states$beta * infected_beta_sum

    #3 vectors of binomal realisations (S->E, E-> I and I->R) to determine whether an individual moves into the next state
    #the vector of interest for each individual is determined by which state they are already in
    # A person cannot move more than one state forward when the model is like this.
    now_removed <- rbinom(pop_size, 1, p_leaving_infected) * (pop_states == 2)
    now_infected <- rbinom(pop_size, 1, p_leaving_exposed) * (pop_states == 1)
    now_exposed <- rbinom(pop_size, 1, infection_prob) * (pop_states == 0)
    
    #sum the 3 vectors to determine which individuals will move up a state
    moving_states <- now_exposed[, 1] + now_infected[, 1] + now_removed[, 1]
    print("The ones who will move this turn")
    print(which(moving_states != 0))

    #the new states of each individual
    new_states <- moving_states + pop_states$state
    print("Who is exposed after run")
    print(which(new_states == 1))
    print("Who is infected after run")
    print(which(new_states == 2))
    print("Who is removed after run")
    print(which(new_states == 3))

    data.frame(state = new_states, beta = pop_states$beta)
}


## Constants
# Population of Scotland
pop_size <- 5500000
simulation_days <- 100
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

# whole_simulation <- function(initial_pop_states, intial_daily_state_changes, lambda, i, simulation_days){
#     new_pop_states <- initial_pop_states
#     new_daily_state_changes <- initial_daily_state_changes
#     for (i in 1:simulation_days) {
#     new_pop_states, new_daily_state_changes <- pass_one_day(new_pop_states, new_daily_state_changes, lambda, i)
#     }
# }


for (i in 1:simulation_days) {
    new_states <- update_states(pop_states, lambda, i)
    #daily_state_changes <- calc_state_changes(pop_states, new_states, i)
    pop_states <- new_states
}

print(c("done: ", daily_state_changes))

#TODO: Do this
calc_state_changes <- function(pop_states, new_pop, daily_state_changes, i) {
    removed_yesterday <- sum(pop_states["state"] == 3)
    infected_yesterday <- sum(pop_states["state"] == 2)

    susceptible <- sum(pop_states["state"] == 0)
    exposed <- sum(pop_states["state"] == 1)
    
    #is this supposed to be new_pop[, 1] instead of pop_states[, 1]?
    removed_at_end <- sum(pop_states[,1] == 3)
    infected_at_end <- sum(pop_states[,1] == 2)


    removed_today <- removed_at_end - removed_at_start
    print(c("Removed today", removed_today))

    infected_today <- (infected_at_end - infected_at_start) + removed_today
    print(c("Infected today", infected_today))
    
    daily_state_changes[i, ] <- c(infected_today, removed_today)
}