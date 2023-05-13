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

rs_preds <- xgb_rs %>%
  collect_predictions() %>%
  select(row_id = .row,
         pred = .pred)

preds_join <- shots %>%
  as.data.frame() %>%
  mutate(row_id = row_number()) %>%
  inner_join(rs_preds,
             by = "row_id")


preds_join %>%
  ggplot(mapping = aes(x = yards_out_before_shot,
                       y = pred)) +
  geom_point()

preds_join %>%
  filter(pred >= 4.5,
         yards_out_before_shot < 200) %>%
  select(from_location,
         yards_out_before_shot,
         hole)

preds_join %>%
  filter(yards_out_before_shot >= 179.8,
         yards_out_before_shot <= 179.9) %>%
  select(strokes_remaining_before_shot)



preds_join %>%
  ggplot(mapping = aes(x = pred,
                       y = strokes_remaining_before_shot)) +
  geom_jitter(height = 0.1,
              alpha = 0.25)



preds_join %>%
  group_by(player,
           round,
           hole) %>%
  arrange(player,
          round,
          hole,
          shot) %>%
  mutate(next_strokes = lead(strokes_rolling),
         next_pred = lead(pred)) %>%
  ungroup() %>%
  select(player,
         round,
         hole,
         shot,
         strokes_rolling,
         next_strokes,
         strokes_remaining_before_shot,
         pred,
         next_pred,
         yards_out_before_shot,
         yards_out_after_shot) %>%
  mutate(next_pred = ifelse(is.na(next_pred),0,next_pred)) %>%
  mutate(next_strokes = ifelse(is.na(next_strokes),
                               strokes_rolling + 1,
                               next_strokes)) %>%
  mutate(actual_strokes = next_strokes - strokes_rolling) %>%
  mutate(strokes_gained = pred - next_pred - actual_strokes) %>%
  # filter(yards_out_after_shot == 0) %>%
  arrange(desc(strokes_gained))



```{r}
#| eval: false
round_hole_ids <- shots %>% 
  select(round,
         hole) %>% 
  unique() %>% 
  arrange(round,
          hole) %>% 
  mutate(round_hole_id = row_number())

get_knn_preds <- function(id = 1,
                          round_hole_ids,
                          shots) {
  
  round_hole_shots <- round_hole_ids %>% 
    filter(round_hole_id == id) %>% 
    inner_join(shots,
               by = c('round','hole')) %>% 
    filter(!is.na(x_before_shot)) %>% 
    select(player,
           round,
           hole,
           shot,
           cv_group,
           x_before_shot,
           y_before_shot,
           strokes_remaining_before_shot)
  
  # round_hole_shots %>% 
  #   mutate(strokes_remaining_before_shot = factor(strokes_remaining_before_shot,
  #                                               ordered = T)) %>%
  #   ggplot(mapping = aes(x = x_before_shot,
  #                        y = y_before_shot,
  #                        color = strokes_remaining_before_shot)) +
  #   geom_point() +
  #   coord_equal() +
  #   theme_minimal()
  
  knn_folds <- group_vfold_cv(data = round_hole_shots,
                              group = cv_group)
  
  knn_recipe <- recipe(formula = strokes_remaining_before_shot ~
                         x_before_shot +
                         y_before_shot,
                       data = round_hole_shots)
  
  knn_mod <- nearest_neighbor(mode = "regression",
                              engine = "kknn")
  
  knn_workflow <- workflow() %>%
    add_recipe(knn_recipe) %>%
    add_model(knn_mod)
  
  knn_rs <- fit_resamples(object = knn_workflow,
                          resamples = knn_folds,
                          control = control_resamples(save_pred = T))
  
  rs_preds <- knn_rs %>% 
    collect_predictions() %>% 
    select(row_id = .row,
           pred = .pred)
  
  preds_join <- round_hole_shots %>% 
    as.data.frame() %>% 
    mutate(row_id = row_number()) %>% 
    inner_join(rs_preds,
               by = "row_id") %>% 
    select(player,
           round,
           hole,
           shot,
           knn_pred = pred)
  
  return(preds_join)
} 

knn_preds_long <- round_hole_ids$round_hole_id %>% 
  map(.f = get_knn_preds,
      round_hole_ids,
      shots) %>% 
  bind_rows()

saveRDS(knn_preds_long, file = "knn_preds_long.rds")
```

```{r}
#| echo: false
# knn_preds_long <- readRDS(file = "knn_preds_long.rds")
```

```{r}
#| warning: false
# shots <- shots %>% 
#   left_join(knn_preds_long,
#             by = c('player','round','hole','shot'))
# 
# shots %>% 
#   ggplot(mapping = aes(x = knn_pred,
#                        y = strokes_remaining_before_shot)) +
#   geom_jitter()
```

```{r}
# folds <- group_vfold_cv(data = shots,
#                         group = cv_group)
# 
# with_knn_recipe <- recipe(formula = strokes_remaining_before_shot ~
#                             yards_out_before_shot +
#                             from_location +
#                             knn_pred,
#                           data = shots) %>% 
#   step_dummy(from_location)
# 
# xgb_mod <- boost_tree(mode = "regression",
#                       engine = "xgboost")
# 
# xgb_workflow <- workflow() %>%
#   add_recipe(with_knn_recipe) %>%
#   add_model(xgb_mod)
# 
# xgb_rs <- fit_resamples(object = xgb_workflow,
#                         resamples = folds)
# 
# xgb_rs %>%
#   collect_metrics() %>%
#   select(.metric,
#          mean) %>%
#   as.data.frame()
```

```{r}

# nearby_shots <- shots %>% 
#   filter(!is.na(x_before_shot)) %>% 
#   select(player,
#          cv_group,
#          round,
#          hole,
#          shot,
#          x_before_shot,
#          y_before_shot,
#          strokes_remaining_before_shot) 
# 
# nearby_shots_join <- nearby_shots %>% 
#   left_join(nearby_shots,
#             by = "hole",
#             suffix = c("_a","_b")) %>% 
#   filter(cv_group_a != cv_group_b) %>% 
#   mutate(dist_a_b = sqrt((x_before_shot_a-x_before_shot_b)^2 + (y_before_shot_a-y_before_shot_b)^2)) %>%
#   mutate(dist_a_b = dist_a_b/3)
# 
# nearby_shot_example = nearby_shots_join %>% 
#   filter(player_a == 1810,
#          round_a == 1,
#          hole == 1,
#          shot_a == 2)
# 
# nearby_shot_example %>% 
#   mutate(strokes_remaining_before_shot_b = factor(strokes_remaining_before_shot_b,
#                                                   ordered = TRUE)) %>% 
#   ggplot(mapping = aes(x = x_before_shot_b,
#                        y = y_before_shot_b,
#                        color = strokes_remaining_before_shot_b,
#                        size = 1/dist_a_b)) +
#   geom_point() +
#   coord_equal()
# 
# nearby_shots_join %>% 
#   filter(dist_a_b <= 10) %>% 
#   mutate(weight = 1/dist_a_b) %>% 
#   mutate(weighted_strokes = weight*strokes_remaining_before_shot_b) %>%
#   group_by(player_a,
#            hole,
#            round_a,
#            shot_a,
#            strokes_remaining_before_shot_a) %>% 
#   summarize(avg_strokes = mean(strokes_remaining_before_shot_b),
#             total_weight = sum(weight),
#             total_weighted_strokes = sum(weighted_strokes),
#             .groups = "keep") %>% 
#   mutate(weighted_avg = total_weighted_strokes/total_weight) %>% 
#   ggplot(mapping = aes(x = weighted_avg,
#                        y = strokes_remaining_before_shot_a)) +
#   geom_jitter()
# 
# # nearby_shots_example <- nearby_shots_join %>% 
# #   filter(player_a == 1810,
# #          round_a == 1,
# #          hole == 1,
# #          ) %>% 
# #   mutate(include = ifelse(dist_a_b <= 15,"yes","no"))
# # 
# # nearby_shots_example %>% 
# #   ggplot() +
# #   geom_point(mapping = aes(x = x_before_shot_b,
# #                            y = y_before_shot_b))
# # 
# # subset <- nearby_shots_example %>%
# #   filter(include == 'yes') %>% 
# #   arrange(dist_a_b) %>%
# #   select(dist_a_b,
# #          strokes_remaining_after_shot_b) %>% 
# #   mutate(weight = 1/dist_a_b) %>% 
# #   mutate(weighted_strokes = weight*strokes_remaining_after_shot_b)
# # 
# # subset
# # 
# # subset %>% 
#   summarize(avg_strokes = mean(strokes_remaining_after_shot_b),
#             total_weight = sum(weight),
#             weighted_strokes = sum(weighted_strokes)) %>%
# #   mutate(weighted_avg = weighted_strokes/total_weight)
# #   
# # 
# # nearby_shots_join %>% 
# #   select(hole,)

```

xgb_mod <- boost_tree(trees = tune(),
                      tree_depth = tune(), 
                      min_n = tune(),
                      loss_reduction = tune(),
                      learn_rate = tune()) %>%
  set_mode("regression") %>% 
  set_engine("xgboost",
             monotone_constraints = c(1))

xgb_workflow <- workflow() %>%
  add_recipe(with_location_recipe) %>%
  add_model(xgb_mod)

set.seed(1)

xgb_grid <- grid_latin_hypercube(trees(range = c(50,250)),
                                 tree_depth(),
                                 min_n(),
                                 loss_reduction(),
                                 learn_rate(),
                                 size = 25)

xgb_tune_results <- tune_grid(object = xgb_workflow,
                              resamples = folds,
                              grid = xgb_grid,
                              control = control_grid(save_pred = TRUE))

metrics <- xgb_tune_results %>%
  collect_metrics()

rsq <- metrics %>% 
  filter(.metric == 'rsq') %>% 
  arrange(desc(mean)) %>% 
  select(trees,
         min_n,
         tree_depth,
         learn_rate,
         loss_reduction,
         mean_rsq = mean)

rsq %>% 
  head(10)

best_rsq <- rsq %>% 
  head(1)

best_tune_preds <- xgb_tune_results %>% 
  collect_predictions() %>% 
  inner_join(best_rsq,
             by = c('trees', 
                    'min_n', 
                    'tree_depth', 
                    'learn_rate', 
                    'loss_reduction')) %>% 
  select(row_id = .row,
         fold = id,
         pred = .pred)

preds_join <- shots %>% 
  as.data.frame() %>% 
  mutate(row_id = row_number()) %>% 
  inner_join(best_tune_preds,
             by = "row_id") 

preds_join %>% 
  ggplot(mapping = aes(x = yards_out_before_shot,
                       y = pred,
                       color = from_location)) +
  geom_point() +
  scale_color_manual(values = cut_colors) + 
  theme_minimal() +
  labs(title = "Out-Of-Sample Predicted Strokes Remaining")
