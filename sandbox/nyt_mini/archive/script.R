library(dplyr)
library(ggplot2)

set.seed(1)

n_players <- 100
n_days <- 365

skill_mean <- 30
skill_sd <- 5
skill_delta_sd <- 0.5

puzzle_mean <- 30
puzzle_sd <- 5

noise_sd <- 5


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
  filter(player_id %in% 1:6) %>% 
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
  mutate(solve_time = skill + difficulty + noise) %>% 
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





