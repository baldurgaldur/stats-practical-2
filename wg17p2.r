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

    # We could random fewer and put them in the right place
    # Eg. no point in randoming for all the ppl in the removed state
    rand <- runif(pop_size)

    now_removed <- pop_states$state == 2 & rand < p_leaving_infected
    now_infected <- pop_states$state == 1 & rand < p_leaving_exposed
    now_exposed <- pop_states$state == 0 & rand < infection_prob

    # Combine the 3 vectors to determine which individuals will move up a state
    moving_states <- now_exposed + now_infected + now_removed

    new_states <- moving_states + pop_states$state
    data.frame(state = new_states, beta = pop_states$beta, rand = pop_states$rand)
}

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

#to find a 0.1% random sample we choose people with index lower than this value 
max_rand <- rand_pop_size

ten_simulations <- matrix(data = 0, nrow = 10, ncol = simulation_days/10)
 
for (j in 1:1){

    print(j)
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

    for (i in 1:simulation_days) {
        
        new_states <- update_states(pop_states, lambda, i)

        # daily_state_changes <- calc_state_changes(pop_states, new_states, daily_state_changes, i, pop_size)

        # #finds the cautious people in pop_states and new_states
        # cautious_pop <- pop_states[pop_states$beta <= max_beta,]
        # cautious_new <- new_states[pop_states$beta <= max_beta,]

        # cautious_daily_changes <- calc_state_changes(cautious_pop, cautious_new, cautious_daily_changes, i, cautious_pop_size)

        #finds people in the random 0.1% sample
    
        rand_pop <- pop_states[pop_states$rand <= max_rand,]
        rand_new <- new_states[new_states$rand <= max_rand,]
        random_samp_changes <- calc_state_changes(rand_pop, rand_new, random_samp_changes, i, rand_pop_size)
        
        pop_states <- new_states
        
        if (i %% 10 == 0){
            ten_simulations[j, i/10] <- daily_state_changes[i, 4]
        }
        
    }
    
}



max_daily_state_changes <- max(daily_state_changes[,4])
max_daily_scaled <- (max_daily_state_changes/pop_size) * 100000
day_max_daily <- which(daily_state_changes[,4] == max_daily_state_changes)

max_cautious_daily_changes <- max(cautious_daily_changes[,4])
max_cautious_scaled <- (max_cautious_daily_changes/cautious_pop_size) * 100000
day_max_cautious <- which(cautious_daily_changes[,4] == max_cautious_daily_changes)

max_random_samp_changes <- max(random_samp_changes[,4])
max_rand_scaled <- (max_random_samp_changes/rand_pop_size) * 100000
day_max_random <- which(random_samp_changes[,4] == max_random_daily_changes)

#legend and labels v ugly will fix !
plot(1:simulation_days, ((daily_state_changes[,4])/pop_size)*100000, type="l", xlab = "days", ylab = "Daily Infections per 100 000 people", ylim = c(0,100000), col = 1, cex = 10)
legend("topleft", legend = c("total population", "cautious population", "random 0.1%"), col = 1:3, lty = 1, cex = 0.5)
points(day_max_daily, max_daily_scaled, pch = 19, col = 1)
text(day_max_daily, max_daily_scaled, labels = paste("(", day_max_daily, ",", max_daily_scaled, ")"), pos = 4, cex = 0.5)


lines((cautious_daily_changes[,4]/cautious_pop_size)*100000, col = 2)
points(day_max_cautious, max_cautious_scaled, pch = 19, col = 2)
text(day_max_cautious, max_cautious_scaled, labels = paste("(", day_max_cautious, ",", max_cautious_scaled, ")"), pos = 4, cex = 0.5)

lines((random_samp_changes[,4]/rand_pop_size)*100000, col = 3)
points(day_max_random, max_rand_scaled, pch = 19, col = 3,)
text(day_max_random, max_rand_scaled, labels = paste("(", day_max_random, ",", max_rand_scaled, ")"), pos = 4, cex = 0.5)

print(ten_simulations)
