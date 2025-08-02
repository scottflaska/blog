library(dplyr)
library(rstan)

set.seed(2)
interval_width <- 0.9

tail_width <- (1 - interval_width)/2
upper_bound <- 1 - tail_width
lower_bound <- tail_width

puzzle_results_obs <-  readRDS("puzzle_results_obs.rds")

n_days <- max(puzzle_results_obs$day)

n_players <- puzzle_results_obs$player_id %>% 
  unique() %>% 
  length()

model_data <- list(
  "T" = n_days,
  "N" = nrow(puzzle_results_obs),
  "K" = n_players,
  "day" = puzzle_results_obs$day,
  "player" = puzzle_results_obs$player_id,
  "y" = puzzle_results_obs$solve_time
)

state_space_fit <- stan(
  file = "model.stan",
  data = model_data,
  iter = 40,
  seed = 42,
  # control = list(adapt_delta = 0.8),
  init = 0
)

pars <- state_space_fit %>% 
  extract()

puzzle_ratings <- data.frame(day = 1:n_days)

for (j in 1:n_days) {
  puzzle_ratings$estimate[j] <- median(pars$rho[,j])
  puzzle_ratings$upper[j] <- quantile(pars$rho[,j], probs = upper_bound)
  puzzle_ratings$lower[j] <- quantile(pars$rho[,j], probs = lower_bound)
}

saveRDS(puzzle_ratings, file = "puzzle_ratings.rds")

player_skill_ratings <- expand.grid("player_id" = 1:n_players,
                                    "day" = 1:n_days)

for (n in 1:nrow(player_skill_ratings)) {
  p <- player_skill_ratings$player_id[n]
  d <- player_skill_ratings$day[n]
  player_skill_ratings$estimate[n] <- median(pars$mu[,d,p])
  player_skill_ratings$upper[n] <- quantile(pars$mu[,d,p], probs = upper_bound)
  player_skill_ratings$lower[n] <- quantile(pars$mu[,d,p], probs = lower_bound)
}

saveRDS(player_skill_ratings, file = "player_skill_ratings.rds")
