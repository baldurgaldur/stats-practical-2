# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

update_states <- function(pop_states, lambda, p_leaving_infected, p_leaving_exposed, i) {
    #Purpose: to update the state of each individual after the model has run for 1 
    #Input: data frame of which state each individual is in, before the model has ran
    #       overall viral infectivity parameter
    #       probability of leaving the infected state on a given day
    #       probability of leaving the exposed state on a given day
    #       day of the simulation
    #Output: updated data frame of the state each individual is in


    #calculate the probability of person j moving into the exposed state
    infected_beta_sum <- sum(pop_states$beta[pop_states$state == 2]) * lambda
    infection_prob <- pop_states$beta * infected_beta_sum

    rand <- runif(pop_size)

    #3 vectors, where 1 corresponds to the indiviual changing state
    # and 0 corresponds to no change of state
    now_removed <- pop_states$state == 2 & rand < p_leaving_infected
    now_infected <- pop_states$state == 1 & rand < p_leaving_exposed
    now_exposed <- pop_states$state == 0 & rand < infection_prob

    # Combine the 3 vectors to determine which individuals will move up a state
    moving_states <- now_exposed + now_infected + now_removed

    #add the vector representing those who are moving states to the current states
    #to get the new state of each individual
    new_states <- moving_states + pop_states$state
    data.frame(state = new_states, beta = pop_states$beta, rand = pop_states$rand)
}

calc_state_changes <- function(pop_states, new_pop, daily_state_changes, i, pop_size) {
    #Purpose: to update the data frame of state changes on each day, so that it contains
    #         today's state changes
    #       
    #Input: population specific data frame of which state each individual was in yesterday
    #       "                   "                   "            "        is in today
    #       current data frame of state changes on each day, to be updated by the function
    #       the day of the simulation
    #       the size of the population
    #Output: the updated data frame of state changes on each day

    
    #finds how many people were susceptible and removed on previous day
    susceptible_yesterday <- sum(pop_states$state == 0)
    removed_yesterday <- sum(pop_states$state == 3)

    #finds how many people were susceptible and removed on current day
    susceptible_today <- sum(new_pop$state == 0)
    removed_today <- sum(new_pop$state == 3)

    #the number of new infections today is the number of people
    #who have left the susceptible state in the past day
    new_infections <- susceptible_yesterday - susceptible_today

    #the total infections for today is the total population
    # minus those in state S or state R
    total_infections <- pop_size - susceptible_today - removed_today
    
    #fills in todays row of the data frame consisting of the 
    #state changes for that day
    daily_state_changes[i, 1] <- susceptible_today
    daily_state_changes[i, 2] <- removed_today
    daily_state_changes[i, 3] <- new_infections
    daily_state_changes[i, 4] <- total_infections
    
    daily_state_changes
}

# Calculate the maximum number of infections and standardise
max_infections_scaled <- function(population, states) {
    max_number_infections <- max(states[,4])
    max_number_scaled <- round((max_number_infections/population)*100000, digits = 0)
}

# Calculate the day that the maximum number of infections occur on
max_day <- function(population, states) {
    max_number_infections <- max(states[,4])
    max_number_scaled <- (max_number_infections/population)*100000
    days_of_max <- which(states[,4] == max_number_infections)
    avg_days_max <- sum(days_of_max)/length(days_of_max)
}

## Constants

#the sizes of the 3 population samples we will consider
pop_size <- 5500000
cautious_pop_size <- pop_size/10
rand_pop_size <- pop_size/1000

simulation_days <- 160
lambda <- 0.4 / pop_size
beta <- rlnorm(pop_size, 0, 0.5)
beta_prob <- beta / mean(beta)
p_leaving_infected <- 1 / 5
p_leaving_exposed <- 1 / 3

#finds the lowest 10% of betas and the indicies of the corresponding people
ordered_beta <- sort(beta_prob)
cautious_beta <- ordered_beta[c(1: pop_size/10)]
cautious_index <- match(cautious_beta, beta_prob)

#finds the cut off beta value, we only want people with betas lower than this value
max_beta <- ordered_beta[pop_size/10]

#to store total infections on each day for each of the 10 simulations
ten_simulations <- matrix(data = 0, nrow = 10, ncol = simulation_days)


#RUNNING THE SIMULATION
for (j in 1:10){
    print(j)
    # 0:= Susceptible, 1:= Exposed, 2:= Infected, 3:= Removed(Recovered or dead)

    #a vector of randomly ordered integers from 1 to the size of the whole population with no repeats
    random <- sample(c(1:pop_size), pop_size, replace= FALSE)

    #Each row in population_states represent an individual in our model
    #Column 1: the state the individual is in, we set this to be 0 for all individuals
    #Column 2: the beta values we generated earlier
    #Column 3: the [population size] randomly ordered integers 
    pop_states <- data.frame(state = rep(0, pop_size), beta = beta_prob, rand = random )

    #Choose 10 random people to be exposed
    starting_exposed <- sample(1:pop_size, 10)
    pop_states[starting_exposed, "state"] <- 1

    #creates data frames for state_changes for the 3 population samples
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    daily_state_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    cautious_daily_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    random_samp_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)


    for (i in 1:simulation_days) {
        
        new_states <- update_states(pop_states, lambda, p_leaving_infected, p_leaving_exposed, i)

        #finds the cautious people in pop_states and new_states
        cautious_pop <- pop_states[pop_states$beta <= max_beta,]
        cautious_new <- new_states[pop_states$beta <= max_beta,]
        
        #to find a 0.1% random sample we choose people with index lower than this value 
        max_rand <- rand_pop_size

        #finds people in the random 0.1% sample
        rand_pop <- pop_states[pop_states$rand <= max_rand,]
        rand_new <- new_states[new_states$rand <= max_rand,]

        #update the corresponding data frame of state changes on each day for each of the 3 populations
        daily_state_changes <- calc_state_changes(pop_states, new_states, daily_state_changes, i, pop_size)
        cautious_daily_changes <- calc_state_changes(cautious_pop, cautious_new, cautious_daily_changes, i, cautious_pop_size)
        random_samp_changes <- calc_state_changes(rand_pop, rand_new, random_samp_changes, i, rand_pop_size)
        
        #set the states of each individual today, as yesterday's states, ready for the next day of the simulation
        pop_states <- new_states
        
        # if (i %% 10 == 0){
        #     ten_simulations[j, i/10] <- daily_state_changes[i, 4]
        # }
        ten_simulations[j, i] <- daily_state_changes[i, 4]
        
    }
    
}


max_daily_scaled <- max_infections_scaled(pop_size, daily_state_changes)
max_daily_day <- max_day(pop_size, daily_state_changes)

max_cautious_scaled <- max_infections_scaled(cautious_pop_size, cautious_daily_changes)
max_cautious_day <- max_day(cautious_pop_size, cautious_daily_changes)

max_rand_scaled <- max_infections_scaled(rand_pop_size, random_samp_changes)
max_rand_day <- max_day(rand_pop_size, random_samp_changes)

plot(1:simulation_days, ((daily_state_changes[,4])/pop_size)*100000, type="l", xlab = "Days", ylab = "Daily Infections per 100 000 people", ylim = c(0,max(max_daily_scaled, max_cautious_scaled, max_rand_scaled)+5000), col = 1, cex = 10)
legend("topleft", legend = c("total population", "cautious population", "random 0.1%"), col = 1:3, lty = 1, cex = 0.5)
points(max_daily_day, max_daily_scaled, pch = 19, col = 1)
text(max_daily_day, max_daily_scaled, labels = paste("(", max_daily_day, ",", max_daily_scaled, ")"), pos = 4, cex = 0.5, col = 1)

lines((cautious_daily_changes[,4]/cautious_pop_size)*100000, col = 2)
points(max_cautious_day, max_cautious_scaled, pch = 19, col = 2)
text(max_cautious_day, max_cautious_scaled, labels = paste("(", max_cautious_day, ",", max_cautious_scaled, ")"), pos = 4, cex = 0.5, col = 2)

lines((random_samp_changes[,4]/rand_pop_size)*100000, col = 3)
points(max_rand_day, max_rand_scaled, pch = 19, col = 3,)
text(max_rand_day, max_rand_scaled, labels = paste("(", max_rand_day, ",", max_rand_scaled, ")"), pos = 2, cex = 0.5, col = 3)

print(ten_simulations)

#continous quantiles plot
ten_median <- rep(0, simulation_days)
ten_lq <- rep(0, simulation_days)
ten_uq <- rep(0, simulation_days)
for (k in 1:simulation_days){
    ten_median[k] <- median(ten_simulations[,k])
    ten_lq[k] <- quantile(ten_simulations[,k])[2]
    ten_uq[k] <- quantile(ten_simulations[,k])[4]
}

plot(1:simulation_days, ten_median/ten_median, type="l", xlab = "days", ylab = "Daily Infections per 100 000 people", ylim = c(0, 5), col = 1, cex = 10)
lines(ten_lq/ten_median, col = 2)
lines(ten_uq/ten_median, col = 3)

#legend("topleft", legend = c("total population", "cautious population", "random 0.1%"), col = 1:3, lty = 1, cex = 0.5)
#points(day_max_daily, max_daily_scaled, pch = 19, col = 1)
#text(day_max_daily, max_daily_scaled, labels = paste("(", day_max_daily, ",", max_daily_scaled, ")"), pos = 4, cex = 0.5)
