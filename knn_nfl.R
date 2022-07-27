set.seed(1234)

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_wflow <- 
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(new_picks_rec)

nfl_knn_fit <- 
  knn_wflow %>% 
  fit(data = train_data)

knn_nfl_fit <- fit(nfl_knn_fit, data = train_data)

preds_test_knn <- predict(knn_nfl_fit, test_data)
preds_test_knn

final_knn <- augment(knn_nfl_fit, test_data)
final_knn1 <- final_knn %>% select(player, pos, .pred)

#resampling KNN

nfl_folds <- vfold_cv(train_data, v = 5)

nfl_metrics <- metric_set(rmse, rsq)

resample_knn_nfl <-
  fit_resamples(
    knn_wflow,
    resamples = nfl_folds,
    metrics = nfl_metrics)

collect_metrics(resample_knn_nfl)

resample_knn_nfl



