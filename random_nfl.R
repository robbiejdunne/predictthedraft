library(tidymodels)
library(tidyverse)

nfl <- read_csv('collegedatanfl.csv')

skimr::skim(training)

visdat::vis_miss(training)

names(training)

training1 <- training %>% select(-player, -pos, -college, -team, -short_college)

train_data <- training %>% filter(year < 2022)
test_data <- training %>% filter(year == 2022)

train_data1 <- train_data %>% select(-key, -year)
test_data1 <- train_data %>% select(-key, -year)


new_picks_rec <- 
  recipe(pick ~ age + height + weight +
           forty + bench + vertical +
           threecone + broad + shuttle +
           games + seasons +
           completions + attempts +
           pass_yards + pass_ints + pass_tds + 
           rec_yards + rec_td + receptions +
           rush_att + rush_yds + rush_td +
           solo_tackes + tackles + loss_tackles + ast_tackles +
           fum_forced + fum_rec + fum_tds + fum_yds +
           sacks + int + int_td + int_yards + pd +
           n_college_picks +
           punt_returns + punt_return_td + punt_return_yards +
           kick_returns + kick_return_td + kick_return_yards, factor(pos), factor(short_college), data = train_data) 


lr_mod <- 
  linear_reg() %>% 
  set_engine("lm")

show_engines('linear_reg')


picks_wflow <- 
  workflow() %>%
  add_model(lr_mod) %>% 
  add_recipe(new_picks_rec)

picks_fit <- 
  picks_wflow %>% 
  fit(data = train_data)

picks_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


preds <- predict(picks_fit, test_data)

final <- augment(picks_fit, test_data)


final1 <- final %>% select(player, pos, .pred)

final2 <- final1 %>% arrange(.pred) %>% head(10) %>% select(-.pred)

library(gt)

pff <- c(34, 20, 15, 91, 9, 96, 7, 6, 12, 8)

final2$pff <- pff  

library(gtExtras)

final2 %>% gt() %>%cols_label(pff = 'PFF Rank',
                              pos = 'Position',
                              player = 'Name') %>%
  cols_align(c(2,3), align = "center") %>%
  gt::tab_header(title = 'Predicting the NFL Draft') %>% gt_theme_538() %>%
  tab_source_note('Data from Pro-Football-Reference') %>%
  data_color(
    columns = c(pff),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#40798c", "#cfd7c7", "#70a9a1"),
      domain = NULL
    )
  )

cbs <- final1 %>% filter(pos == 'CB')


set.seed(192)
fit <- rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") %>%
  fit(pick ~ ., data = train_data)

fit$preproc


ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 50) %>%
  set_mode("regression") %>%
  set_engine("ranger")

draft_folds <- bootstraps(test_data1, strata = pick)

ranger_workflow <-
  workflow() %>%
  add_recipe(new_picks_rec) %>%
  add_model(ranger_spec)

set.seed(8577)
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow,
            resamples = draft_folds,
            grid = 11
  )

show_best(ranger_tune, metric = "rmse")

show_best(ranger_tune, metric = "rsq")

autoplot(ranger_tune)

final_rf <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune, metric = "rsq"))

typeof(final_rf)

ikea_fit <- fit(final_rf, train_data)

final <- augment(ikea_fit, test_data)

final1 <- final %>% select(player, pos, .pred)

final2 <- final1 %>% arrange(.pred) %>% head(10) %>% select(-.pred)
