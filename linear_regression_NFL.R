library(tidymodels)

training <- read_csv('collegedatanfl.csv')

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

final_linear <- augment(picks_fit, test_data)

final_linear1 <- final_linear %>% select(player, pos, .pred)


