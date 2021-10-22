# 17 Ava Napper, Baldur Björnsson, Madeleine Reid
# https://github.com/baldurgaldur/stats-practical-2

update_states <- function(pop_states, lambda, p_leaving_infected, p_leaving_exposed) {
    #Purpose:   To update the state of each individual after the model has
    #           run for 1 day
    #Input:     Data frame of state each individual is in and his beta value.
    #           Overall viral infectivity parameter
    #           Probability of leaving the infected state on a given day
    #           Probability of leaving the exposed state on a given day
    #           day of the simulation
    #Output:    Updated data frame of the state each individual is in when one
    #           day has passed

    # Calculate the probability of person j moving into the exposed state
    infected_beta_sum <- sum(pop_states$beta[pop_states$state == 2]) * lambda
    infection_prob <- pop_states$beta * infected_beta_sum

    rand <- runif(pop_size)

    # 3 vectors, where 1 corresponds to the indiviual changing state
    # and 0 corresponds to no change of state
    now_removed <- pop_states$state == 2 & rand < p_leaving_infected
    now_infected <- pop_states$state == 1 & rand < p_leaving_exposed
    now_exposed <- pop_states$state == 0 & rand < infection_prob

    # Combine the 3 vectors to determine which individuals will move up a state
    # Note that these are boolean vectors, so a person never moves into
    # more than one state each day
    moving_states <- now_exposed + now_infected + now_removed

    # Add the vector representing those who are moving states to the current 
    # states to get the new state of each individual
    new_states <- moving_states + pop_states$state
    data.frame(state = new_states, beta = pop_states$beta, rand = pop_states$rand)
}

calc_state_changes <- function(pop_states, new_pop, daily_state_changes, i, pop_size) {
    #Purpose:   To update the data frame of state changes on each day,
    #           so that it contains today's state changes
    #Input: Population specific data frame of which state each individual was in yesterday
    #       "                   "                   "            "        is in today
    #       current data frame of state changes on each day, to be updated by the function
    #       The day of the simulation
    #       The size of the population
    #Output: The updated data frame of state changes on each day

    # Count how many people were susceptible and removed yesterday
    susceptible_yesterday <- sum(pop_states$state == 0)

    # Count how many people were susceptible and removed today
    susceptible_today <- sum(new_pop$state == 0)
    removed_today <- sum(new_pop$state == 3)

    # The number of new infections today is the number of people
    # who have left the susceptible state in the past day
    new_infections <- susceptible_yesterday - susceptible_today

    # The total infections for today is the total population
    # minus those in state S or R
    total_infections <- pop_size - susceptible_today - removed_today

    # Fill in todays row of the data frame consisting of the
    # state changes for that day
    daily_state_changes[i, 1] <- susceptible_today
    daily_state_changes[i, 2] <- removed_today
    daily_state_changes[i, 3] <- new_infections
    daily_state_changes[i, 4] <- total_infections

    daily_state_changes
}

plot_simulation_peaks <- function(value_of_peak, day_of_peak) {
    # Purpose:  Plot the peaks of each model run
    #           We plot the day of the peak for each group in one plot
    #           And in another we plot the percentage of people infected
    #           at the peak of the model run.
    # Input:    Two matrices. The columns of each are 1 = Total pop,
    #           2 = Cautious 10% and 3 = random 0.1%. Each row is the value
    #           corresponding to one run of our model
    # Output:   Display 2 box plots
    boxplot(main="Day when number of infected peaked", xlab = "Groups of people", names=c("Total", "Cautious 10%", "Random 0.1%"), ylab="Day", day_of_peak)
    boxplot(main="Population percentage infected at pandemic peak", xlab = "Groups of people", names=c("Total", "Cautious 10%", "Random 0.1%"), ylab="Percent of population", value_of_peak)
}
max_infections_scaled <- function(population, states) {
    # Purpose:  Calculate the maximum number of infections and standardise
    max_number_infections <- max(states[, 4])
    max_number_scaled <- round((max_number_infections/population)*100000, digits = 0)
}

max_day <- function(population, states) {
#   Purpose:    Calculate the day that the maximum number of infections occur on
    max_number_infections <- max(states[, 4])
    days_of_max <- which(states[, 4] == max_number_infections)
    avg_days_max <- sum(days_of_max) / length(days_of_max)
}

peak_day_and_total_infections <- function(daily_state_changes) {
    # Purpose:  Extract the greatest value of the infected column within
    #           the daily state changes. Also extract the day that maximum
    #           value occurred.
    # Input:    A matrix where the fourth column represents how many people are
    #           infected on a given day.
    # Output:   A vector of length 2. The first entry is the greatest value
    #           in the infection row and the second is it's index.

    greatest_infected_number <- max(daily_state_changes[, 4])
    all_peak_days <- which(daily_state_changes[, 4] == greatest_infected_number)
    # When 2 days are the peak, we report the average day. E.g. if day 18 and 19
    # have the greatest number of infected, we want 18.5 to be the max
    real_day_of_peak <- sum(all_peak_days) / length(all_peak_days)
    c(greatest_infected_number, real_day_of_peak)
}

## Constants
# The sizes of the 3 population samples we will consider
pop_size <- 5500000
cautious_pop_size <- pop_size / 10
rand_pop_size <- pop_size / 1000

simulation_days <- 160
no_of_runs <- 10
lambda <- 0.4 / pop_size
beta <- rlnorm(pop_size, 0, 0.5)
beta_prob <- beta / mean(beta)
p_leaving_infected <- 1 / 5
p_leaving_exposed <- 1 / 3

# Find the lowest 10% of betas and the indicies of the corresponding people
ordered_beta <- sort(beta_prob)
cautious_beta <- ordered_beta[c(1: pop_size / 10)]
cautious_index <- match(cautious_beta, beta_prob)

# Find the cut off beta value
# because we only want people with betas lower than this value
max_beta <- ordered_beta[pop_size / 10]

# Storage for total infections on each day for each of the 10 simulations
ten_simulations <- matrix(data = 0, nrow = 10, ncol = simulation_days)
cautious_ten_sim <- matrix(data = 0, nrow = 10, ncol = simulation_days)
rand_ten_sim <- matrix(data = 0, nrow = 10, ncol = simulation_days)

day_of_peak <- matrix(data = 0, nrow = no_of_runs, ncol = 3)
value_of_peak <- matrix(data = 0, nrow = no_of_runs, ncol = 3)
 
for (j in 1:no_of_runs) {

    # A vector of randomly ordered integers from 1 to the size of the whole population with no repeats
    random <- sample(c(1:pop_size), pop_size, replace = FALSE)

    # Each row in population_states represent an individual in our model
    # Column 1(state):  The state the individual is in, ividuals all start
    #                   with 0 except 10 randomly chosen.
    # Column 2(beta):    The beta values we generated earlier
    # Column 3(rand):    [population size] randomly ordered integers
    # Valid states are the following:
    # 0:= Susceptible, 1:= Exposed, 2:= Infected, 3:= Removed(Recovered or dead)
    pop_states <- data.frame(state = rep(0, pop_size), beta = beta_prob, rand = random )

    # Choose 10 random people to be exposed
    starting_exposed <- sample(1:pop_size, 10)
    pop_states[starting_exposed, "state"] <- 1

    # Create matrix for each of the 3 population samples.
    # The matrix keeps track of how many people are infected each day,
    # how many are susceptible each day and how many _get_ infected each day.
    # Each row in daily_state_changes is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    daily_state_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    cautious_daily_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    random_samp_changes <- matrix(data = 0, nrow = simulation_days, ncol = 4)

    for (i in 1:simulation_days) {
        new_states <- update_states(pop_states, lambda, p_leaving_infected, p_leaving_exposed)

        # Find the cautious people in pop_states and new_states
        cautious_pop <- pop_states[pop_states$beta <= max_beta, ]
        cautious_new <- new_states[pop_states$beta <= max_beta, ]

        # Take a 0.1% random sample by choosing people with indices
        # lower than this value
        max_rand <- rand_pop_size

        # Find people in the random 0.1% sample
        rand_pop <- pop_states[pop_states$rand <= max_rand, ]
        rand_new <- new_states[new_states$rand <= max_rand, ]

        # Update the corresponding data frame of state changes on each day for each of the 3 populations
        daily_state_changes <- calc_state_changes(pop_states, new_states, daily_state_changes, i, pop_size)
        cautious_daily_changes <- calc_state_changes(cautious_pop, cautious_new, cautious_daily_changes, i, cautious_pop_size)
        random_samp_changes <- calc_state_changes(rand_pop, rand_new, random_samp_changes, i, rand_pop_size)
        
        # Set the states of each individual today, as yesterday's states, ready for the next day of the simulation
        pop_states <- new_states
        
        # if (i%%10 == 0){
        #     ten_simulations[j,i/10] <- daily_state_changes[i,4]
        # }
        ten_simulations[j, i] <- daily_state_changes[i, 4]
        cautious_ten_sim[j, i] <- cautious_daily_changes[i, 4]
        rand_ten_sim[j, i] <- random_samp_changes[i, 4]
    }

    total_pop_peak_data <- peak_day_and_total_infections(daily_state_changes)
    value_of_peak[j, 1] <- total_pop_peak_data[1] / pop_size
    day_of_peak[j, 1] <- total_pop_peak_data[2]

    cautious_pop_peak_data <- peak_day_and_total_infections(cautious_daily_changes)
    value_of_peak[j, 2] <- cautious_pop_peak_data[1] / cautious_pop_size
    day_of_peak[j, 2] <- cautious_pop_peak_data[2]

    random_pop_peak_data <- peak_day_and_total_infections(random_samp_changes)
    value_of_peak[j, 3] <- random_pop_peak_data[1] / rand_pop_size
    day_of_peak[j, 3] <- random_pop_peak_data[2]
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


#continous quantiles plot
ten_median <- rep(0, simulation_days)
cautious_median <- rep(0, simulation_days)
rand_median <- rep(0, simulation_days)

# ten_lq <- rep(0, simulation_days)
# ten_uq <- rep(0, simulation_days)
for (k in 1:simulation_days) {
    ten_median[k] <- median(ten_simulations[, k])
    cautious_median[k] <- median(cautious_ten_sim[, k])
    rand_median[k] <- median(rand_ten_sim[, k])
#     ten_lq[k] <- quantile(ten_simulations[, k])[2]
#     ten_uq[k] <- quantile(ten_simulations[, k])[4]
 }

#CTS GRAPH- do we want this
# plot(1:simulation_days, ten_median/ten_median, type="l", xlab = "days", ylab = "Daily Infections per 100 000 people", ylim = c(0, 5), col = 1, cex = 10)
# lines(ten_lq / ten_median, col = 2)
# lines(ten_uq / ten_median, col = 3)

#legend("topleft", legend = c("total population", "cautious population", "random 0.1%"), col = 1:3, lty = 1, cex = 0.5)
#points(day_max_daily, max_daily_scaled, pch = 19, col = 1)
#text(day_max_daily, max_daily_scaled, labels = paste("(", day_max_daily, ",", max_daily_scaled, ")"), pos = 4, cex = 0.5)

# The difference between the percentage infected at the pandemic peak
# between the cautious 10% and the total implies that if we had the
# ZOE app data reporting some number of infections, we know that those
# people are half as likely to have covid than if you were to randomly
# sample the pop. This highlights how obviously skewed any statistical inference
# on the general population would be using the ZOE app data.
plot_simulation_peaks(value_of_peak, day_of_peak)

#day 10, 20 30 etc. of ten_simulations
#ten_simulations[,seq(10,160,10)]

#10 medians for day 10, 10 medians for day 20 etc
#ten_median[rep(seq(10,160,10),each = 10)]

standardised_ten_sim <- ten_simulations[,seq(10,160,10)] / ten_median[rep(seq(10,160,10),each = 10)]
standardised_cautious_ten_sim <- cautious_ten_sim[,seq(10,160,10)] / cautious_median[rep(seq(10,160,10),each = 10)]
standardised_rand_ten_sim <- rand_ten_sim[,seq(10,160,10)] / rand_median[rep(seq(10,160,10),each = 10)]
boxplot(standardised_ten_sim, main = "Variation in total infections over 10 simulations, each 10 days, \n for the whole population", xlab = "Day", names = seq(10,160,10))
boxplot(standardised_cautious_ten_sim, main = "Variation in total infections over 10 simulations, each 10 days,\n for the cautious 10% of the population", xlab = "Day", names = seq(10,160,10))
boxplot(standardised_rand_ten_sim, main = "Variation in total infections over 10 simulations, each 10 days,\n for the random sample of 0.1% of the population", xlab = "Day", names = seq(10,160,10))
