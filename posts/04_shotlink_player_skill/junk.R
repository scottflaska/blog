shot_level %>% 
  ggplot(mapping = aes(x = yards_out_before_shot,
                       y = strokes_remaining_before_shot)) +
  geom_jitter(width = 0,
              height = 0.1) +
  geom_smooth() +
  scale_y_continuous(breaks = 1:10)

shot_level %>% 
  mutate(yards_out_round = round(yards_out_before_shot/20)*20) %>% 
  group_by(yards_out_round) %>% 
  summarize(Avg = mean(strokes_remaining_before_shot),
            Shots = n()) %>% 
  ggplot(mapping = aes(x = yards_out_round,
                       y = Avg,
                       size = Shots)) +
  geom_point()


shot_level %>% 
  mutate(log_yards_out_before_shot = log(yards_out_before_shot+1)) %>% 
  filter(player %in% c(24924, 24781, 24925, 24663)) %>% 
  ggplot(mapping = aes(x = log_yards_out_before_shot,
                       y = strokes_remaining_before_shot,
                       color = player_last_name)) +
  geom_jitter(width = 0,
              height = 0.1,
              alpha = 0.25) +
  geom_smooth(method = "lm",
              se = F) +
  scale_y_continuous(breaks = 1:10) +
  theme_minimal()

score_card <- shot_level %>%
  mutate(player_id = paste0(tolower(player_first_name),
                            "_",
                            tolower(player_last_name),
                            "_",
                            player)) %>% 
  select(player_id,
         round,
         hole,
         hole_score) %>% 
  unique()

final_scores = shot_level %>% 
  mutate(player_id = paste0(tolower(player_first_name),
                            "_",
                            tolower(player_last_name),
                            "_",
                            player)) %>% 
  group_by(player_id) %>% 
  summarize(final_score = sum(num_of_strokes)) %>% 
  arrange(final_score)

random_int_model <- lmer(formula = hole_score ~ (1|player_id) + (1|hole), 
                         data = score_card)

random_eff <- ranef(random_int_model)

player_effects <- random_eff$player_id %>%
  rownames_to_column(var = "player_id") %>% 
  rename(intercept = `(Intercept)`) %>% 
  arrange(intercept)

library(ggrepel)

player_effects %>% 
  left_join(final_scores) %>% 
  ggplot(mapping = aes(x = final_score,
                       y = intercept,
                       label = player_id)) +
  geom_point(alpha = 0.1)