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

calc_state_changes <- function(pop_states, new_pop, state_info_total, i, pop_size) {
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
    state_info_total[i, 1] <- susceptible_today
    state_info_total[i, 2] <- removed_today
    state_info_total[i, 3] <- new_infections
    state_info_total[i, 4] <- total_infections

    state_info_total
}

plot_simulation_peaks <- function(peak_infection_value, peak_day) {
    # Purpose:  Plot the peaks of each model run
    #           We plot the day of the peak for each group in one plot
    #           And in another we plot the percentage of people infected
    #           at the peak of the model run.
    # Input:    Two matrices. The columns of each are 1 = Total pop,
    #           2 = Cautious 10% and 3 = random 0.1%. Each row is the value
    #           corresponding to one run of our model
    # Output:   Display 2 box plots
    boxplot(main="Day when number of infected peaked", xlab = "Groups of people", names=c("Total", "Cautious 10%", "Random 0.1%"), ylab="Day", peak_day)
    boxplot(main="Population percentage infected at pandemic peak", xlab = "Groups of people", names=c("Total", "Cautious 10%", "Random 0.1%"), ylab="Percent of population", peak_infection_value)
}

standardise <- function(population, states_vec) {
    # Purpose: Calculate the number of cases per 100 000 people for comparison between populations
    standardised <- (states_vec/population) * 100000
}

max_day <- function(states_vec) {
    # Purpose: Calculate the day that the maximum number of infections occur on
    # Input: A matrix where the fourth column represents how many people are
    #        infected on a given day.
    # Output: a number representing the day of the maximum total infections

    max_number_infections <- max(states_vec)
    days_of_max <- which(states_vec == max_number_infections)

    # When 2 days are the peak, we report the average day. E.g. if day 18 and 19
    # have the greatest number of infected, we want 18.5 to be the day of the max
    avg_days_max <- sum(days_of_max) / length(days_of_max)
}

plot_trajectory <- function(pop_size, cautious_pop_size, rand_pop_size, state_info_total_vec, state_info_cautious_vec, state_info_random_vec, title){
    # Purpose: to plot 3 lines that are standardised according to their respective population
    # Input: the sizes of the 3 populations
    #        the vector that we want to plot for each population
    #        the title of the plot
    # Output: a plot of 3 lines and points where they reach their maximum value

    # Calculate the standardised maximum number of daily infections and the day
    # on which it occurs for all three populations
    max_daily_scaled <- round(max(standardise(pop_size, state_info_total_vec)), digits = 0)
    max_daily_day <- max_day(state_info_total_vec)
    
    max_cautious_scaled <- round(max(standardise(cautious_pop_size, state_info_cautious_vec)), digits = 0)
    max_cautious_day <- max_day(state_info_cautious_vec)
    
    max_rand_scaled <- round(max(standardise(rand_pop_size, state_info_random_vec)), digits = 0)
    max_rand_day <- max_day(state_info_random_vec)
    
    # Plot standardised daily trajectories for each population, labeling peaks
    plot(1:simulation_days, standardise(pop_size, state_info_total_vec), type="l",
         xlab = "Days", ylab = "Daily Infections per 100 000 people",
         ylim = c(0,max(max_daily_scaled, max_cautious_scaled, max_rand_scaled)+5000),
         main = title, col = 1, cex = 10)
    legend("topleft", legend = c("Total population", "Cautious population", "Random 0.1%"), col = 1:3, lty = 1, cex = 1)
    points(max_daily_day, max_daily_scaled, pch = 19, col = 1)
    text(max_daily_day, max_daily_scaled, labels = paste("(", max_daily_day, ",", max_daily_scaled, ")"), pos = 4, cex = 1, col = 1)
    
    lines(standardise(cautious_pop_size, state_info_cautious_vec), col = 2)
    points(max_cautious_day, max_cautious_scaled, pch = 19, col = 2)
    text(max_cautious_day, max_cautious_scaled, labels = paste("(", max_cautious_day, ",", max_cautious_scaled, ")"), pos = 4, cex = 1, col = 2)
    
    lines(standardise(rand_pop_size, state_info_random_vec), col = 3)
    points(max_rand_day, max_rand_scaled, pch = 19, col = 3,)
    text(max_rand_day, max_rand_scaled, labels = paste("(", max_rand_day, ",", max_rand_scaled, ")"), pos = 2, cex = 1, col = 3)
    
}   

## Constants
# The sizes of the 3 population samples we will consider
pop_size <- 100000
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
n_sims_total <- matrix(data = 0, nrow = no_of_runs, ncol = simulation_days)
n_sims_cautious <- matrix(data = 0, nrow = no_of_runs, ncol = simulation_days)
n_sims_random <- matrix(data = 0, nrow = no_of_runs, ncol = simulation_days)

peak_day <- matrix(data = 0, nrow = no_of_runs, ncol = 3)
peak_infection_value <- matrix(data = 0, nrow = no_of_runs, ncol = 3)

total_over_cautious_ratio <- vector()
 
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
    # Each row in state_info_total is the difference how many individuals
    # were in that state between the start of the day and the end of the day.
    state_info_total <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    state_info_cautious <- matrix(data = 0, nrow = simulation_days, ncol = 4)
    state_info_random <- matrix(data = 0, nrow = simulation_days, ncol = 4)

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
        state_info_total <- calc_state_changes(pop_states, new_states, state_info_total, i, pop_size)
        state_info_cautious <- calc_state_changes(cautious_pop, cautious_new, state_info_cautious, i, cautious_pop_size)
        state_info_random <- calc_state_changes(rand_pop, rand_new, state_info_random, i, rand_pop_size)
        
        # Set the states of each individual today, as yesterday's states, ready for the next day of the simulation
        pop_states <- new_states
        
        # Extracting total number of infections into 
        n_sims_total[j, i] <- state_info_total[i, 4]
        n_sims_cautious[j, i] <- state_info_cautious[i, 4]
        n_sims_random[j, i] <- state_info_random[i, 4]
    }

    # Calculate the ratio between total_infected over cautious_infected
    ratio <- sum(state_info_total[, 4]) / sum(state_info_cautious[, 4])
    total_over_cautious_ratio <- c(total_over_cautious_ratio, ratio)

    peak_infection_value[j, 1] <- max(state_info_total[, 4]) / pop_size
    peak_day[j, 1] <- max_day(state_info_total[ ,4])

    peak_infection_value[j, 2] <- max(state_info_cautious[, 4]) / cautious_pop_size
    peak_day[j, 2] <- max_day(state_info_cautious[ ,4])

    peak_infection_value[j, 3] <- max(state_info_random[, 4]) / rand_pop_size
    peak_day[j, 3] <- max_day(state_info_random[, 4])
}
    

plot_trajectory(pop_size, cautious_pop_size, rand_pop_size, state_info_total[, 4], state_info_cautious[, 4], state_info_random[, 4], "Total Daily Infection Trajectory")
plot_trajectory(pop_size, cautious_pop_size, rand_pop_size, state_info_total[, 3], state_info_cautious[, 3], state_info_random[, 3], "New Daily Infection Trajectory")


# Vector to store median of each day for the n simulations
# One for each population
ten_median <- rep(0, simulation_days)
cautious_median <- rep(0, simulation_days)
rand_median <- rep(0, simulation_days)

# Storing the median values
for (k in 1:simulation_days) {
    ten_median[k] <- median(n_sims_total[, k])
    cautious_median[k] <- median(n_sims_cautious[, k])
    rand_median[k] <- median(n_sims_random[, k])

 }

# Works out the total infections relative to the median of the 10 simulations, at 10 day intervals
standardised_n_sims <- n_sims_total[,seq(10,160,10)] / ten_median[rep(seq(10,160,10),each = 10)]
standardised_n_sims_cautious <- n_sims_cautious[,seq(10,160,10)] / cautious_median[rep(seq(10,160,10),each = 10)]
standardised_n_sims_random <- n_sims_random[,seq(10,160,10)] / rand_median[rep(seq(10,160,10),each = 10)]

# Plots the corresponding boxplots
boxplot(standardised_n_sims, main = "Variation in total infections over 10 simulations, each 10 days, \n for the whole population", xlab = "Day", ylab = "Total infections / median of total infections for the 10 simulations", names = seq(10,160,10))
boxplot(standardised_n_sims_cautious, main = "Variation in total infections over 10 simulations, each 10 days,\n for the cautious 10% of the population", xlab = "Day", ylab = "Total infections / median of total infections for the 10 simulations", names = seq(10,160,10))
boxplot(standardised_n_sims_random, main = "Variation in total infections over 10 simulations, each 10 days,\n for the random sample of 0.1% of the population", xlab = "Day", ylab = "Total infections / median of total infections for the 10 simulations", names = seq(10,160,10))

#write a couple of lines on what the implications of these results might be for interpreting
#daily infection trajectories reconstructed using the ZOE app data
# Possible points:
  # Appears to be higher number of cases in whole pop compared with cautious pop (at peak approx double)
  # Variation at start of model for cautious is larger than whole pop?
  # Likely to be many more (max double) numbers of real cases than the ZOE app would predict
  # ZOE data not representative of whole pop

# The difference between the infected ratio at the pandemic peak
# between the cautious 10% and the total implies that if we had the
# ZOE app data reporting some number of infections, those
# people are half as likely to have Covid-19 than a randomly sampled person. 
# This highlights how obviously skewed statistical inference
# on the general population would be using the ZOE app data.
plot_simulation_peaks(peak_infection_value, peak_day)

# Continuing with the ZOE data example, here we plot the ratio between
# infected cautious people and infected general population.
# This gives us some way to answer a person that comes and tells us that ZOE is now
# reporting n number of infections, we respond by saying, well then the total population
# has about 16.5 x n number of infections. And our confidence in the number 16.5 is 
# highlighted by the box plot representation
boxplot(total_over_cautious_ratio, main="Ratio of total infections to cautious infections per simulation", ylab = "Average total infections / average cautious total infections")

