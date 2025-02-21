---
title: "Tracking Strokes Gained Driving with a Kalman Filter"
author: "Scott Flaska"
date: "2024-02-04"
draft: true
categories: [shotlink, golf, r, machine learning]
#image: "images/predictions.png"
format: 
  html:
    toc: true
    toc-location: left
    toc-title: Contents
execute: 
  freeze: auto
---

# Introduction

```{r}
#| output: false
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
```

```{r}
predictions <- readRDS("../04_strokes_gained/predictions.rds")

#NEED TO GO BACK AND GET PENALTY SHOTS AND PROVISIONALS!!!
strokes_gained <- predictions %>%
  group_by(player,
           round,
           hole) %>% 
  arrange(player,
          round,
          hole) %>% 
  mutate(next_pred = lead(pred, n = 1)) %>% 
  tidyr::replace_na(replace = list(next_pred=0)) %>% 
  mutate(strokes_gained = pred - next_pred - num_of_strokes) %>% 
  ungroup()

strokes_gained %>% 
  filter(player == 1810,
         round == 1,
         hole %in% c(1,2)) %>% 
  select(player,
         round,
         hole,
         par_value,
         hole_score,
         shot,
         num_of_strokes,
         yards_out_before_shot,
         strokes_remaining_before_shot,
         pred,
         next_pred,
         strokes_gained) 

strokes_gained %>% 
  filter(shot == 1) %>% 
  group_by(player,
           player_first_name,
           player_last_name) %>% 
  summarize(Shots = n(),
            avg_strokes_gained = mean(strokes_gained),
            .groups = "keep") %>% 
  arrange(desc(avg_strokes_gained)) %>% 
  head()

strokes_gained %>% 
  filter(shot == 1,
         par_value %in% 4:5) %>% 
  filter(player %in% c(31323,24925)) %>% 
  ggplot(mapping = aes(x = player_last_name,
                       y = strokes_gained)) +
  geom_quasirandom()

player_drives <- strokes_gained %>% 
  filter(player == 25686,
         shot == 1,
         par_value %in% 4:5) %>% 
  arrange(round,
          hole) %>% 
  mutate(t = row_number()) %>% 
  select(t,
         round,
         hole,
         strokes_gained)


t_max <- max(player_drives$t)

#Initial estimate
x <- 0

#Initial error estimate
ee <- 0.1

#Error in measurement
em <- 0.01

x_log <- c()

for (i in 1:t_max) {
  g <- ee/(ee + em)
  m <- player_drives[i,]$strokes_gained
  x <- x + g*(m - x)
  ee <- (1 - g)*ee
  x_log <- c(x_log, x)
}

player_drives$k <- x_log

data.frame(t = 0,
           k = 0) %>% 
  bind_rows(player_drives) %>% 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(x = t,
                           y = strokes_gained)) +
  geom_line(mapping = aes(x = t,
                           y = k),
             color = "red")


```
