library(dplyr)
library(ggplot2)
library(rstan)

set.seed(2)

n_players <- 12
n_days <- 60

global_intercept <- 120

skill_mean <- 0
skill_sd <- 20
skill_delta_sd <- 2

puzzle_mean <- 0
puzzle_sd <- 20

noise_sd <- 10

player_skill <- data.frame("player_id" = integer(),
                           "day" = integer(),
                           "skill" = numeric())

for (i in 1:n_players) {
  skill <- rnorm(n = 1, mean = skill_mean, sd = skill_sd)
  skill_log <- c(skill)
  for (j in 2:n_days) {
    new_skill <- skill + rnorm(n = 1, mean = 0, sd = skill_delta_sd)
    skill_log <- c(skill_log,new_skill)
    skill <- new_skill
  }
  player_i_skill <- data.frame("player_id" = rep(i,n_days),
                               "day" = 1:n_days,
                               "skill"=skill_log)
  player_skill <- player_skill %>% 
    bind_rows(player_i_skill)
}

player_skill %>% 
  mutate(player_id = factor(player_id, ordered = T)) %>% 
  ggplot(mapping = aes(x = day,
                       y = skill)) +
  geom_line() +
  facet_wrap(vars(player_id))

puzzles <- data.frame("day"=1:n_days,
                      "difficulty" = rnorm(n = n_days, mean = puzzle_mean, sd = puzzle_sd))

puzzle_results <- player_skill %>% 
  inner_join(puzzles, by = "day") %>% 
  mutate(noise = rnorm(n = n(), mean = 0, sd = noise_sd)) %>% 
  mutate(solve_time = global_intercept + skill + difficulty + noise) %>% 
  mutate(solve_time = round(solve_time))

hist(puzzle_results$solve_time)
min(puzzle_results$solve_time)

avg_solve_time <- median(puzzle_results$solve_time)

puzzle_results %>% 
  filter(player_id %in% 1:6) %>% 
  ggplot(mapping = aes(x = day)) +
  geom_point(mapping = aes(y = solve_time)) +
  facet_wrap(vars(player_id))

puzzle_results %>% 
  filter(day <= 16) %>%
  ggplot(mapping = aes(x = solve_time)) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = avg_solve_time) +
  facet_wrap(vars(day))

puzzle_results_obs <- puzzle_results %>% 
  group_by(player_id) %>%
  # sample_frac(size = 0.8) %>% 
  as.data.frame() %>% 
  arrange(player_id,
          day)

model_data <- list(
  "T" = n_days,
  "N" = nrow(puzzle_results_obs),
  "K" = n_players,
  "day" = puzzle_results_obs$day,
  "player" = puzzle_results_obs$player_id,
  "y" = puzzle_results_obs$solve_time
)

state_space_fit <- stan(
  file = "model_6.stan",
  data = model_data,
  iter = 2000,
  seed = 42,
  # control = list(adapt_delta = 0.8),
  init = 0
)

pars <- state_space_fit %>% 
  extract()

puzzle_est <- data.frame("day" = integer(),
                         "difficulty_est" = numeric())

puzzles$estimate <- NA
puzzles$upper <- NA
puzzles$lower <- NA

for (j in 1:n_days) {
  puzzles$estimate[j] <- median(pars$p[,j])
  puzzles$upper[j] <- quantile(pars$p[,j], probs = 0.95)
  puzzles$lower[j] <- quantile(pars$p[,j], probs = 0.05)
}

puzzles %>% 
  ggplot(mapping = aes(x = day)) +
  geom_segment(mapping = aes(xend = day,
                             y = lower,
                             yend = upper)) +
  geom_point(mapping = aes(y = estimate)) +
  geom_point(mapping = aes(y = difficulty),
             color = "red")

player_skill$estimate <- NA
player_skill$upper <- NA
player_skill$lower <- NA

for (n in 1:nrow(player_skill)) {
  p <- player_skill$player_id[n]
  d <- player_skill$day[n]
  player_skill$estimate[n] <- median(pars$mu[,d,p])
  player_skill$upper[n] <- quantile(pars$mu[,d,p], probs = 0.95)
  player_skill$lower[n] <- quantile(pars$mu[,d,p], probs = 0.05)
}

player_skill %>% 
  ggplot(mapping = aes(x = day)) +
  geom_ribbon(mapping = aes(ymin = lower,
                            ymax = upper),
              alpha = 0.2) +
  geom_line(mapping = aes(y = estimate)) +
  geom_line(mapping = aes(y = skill),
            color = "red") +
  facet_wrap(vars(player_id))
