library(tidyverse)

user_reviews <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv")

user_reviews %>%
  count(grade) %>%
  ggplot(aes(grade, n)) +
  geom_col(fill = "midnightblue", alpha = 0.7)

user_reviews %>% 
  filter(grade > 8) %>% 
  sample_n(5) %>% 
  pull(text)

reviews_parsed <- user_reviews %>% 
  mutate(text = str_remove(text, "Expand$"),
         rating = case_when(grade > 6 ~ "good",
                            TRUE ~ "bad"))
reviews_parsed

library(tidytext)
words_per_review <- reviews_parsed %>% 
  unnest_tokens(word, text) %>% 
  count(user_name, name = "total_words")

words_per_review %>% 
  ggplot(aes(total_words)) +
  geom_histogram()


library(tidymodels)

set.seed(123)
review_split <- initial_split(reviews_parsed, strata = rating)
review_train <-training(review_split)
review_test <- testing(review_split)

library(textrecipes)

review_rec <- recipe(rating ~ text, data = review_train) %>% 
  step_tokenize(text) %>% 
  step_stopwords(text) %>% 
  step_tokenfilter(text, max_tokens = 500) %>% 
  step_tfidf(text) %>% 
  step_normalize(all_predictors())

review_prep <- prep(review_rec)

review_prep

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lasso_wf <- workflow() %>% 
  add_recipe(review_rec) %>% 
  add_model(lasso_spec)

lasso_wf

lambda_grid <- grid_regular(penalty(), levels = 30) 

set.seed(123)
review_folds <- bootstraps(review_train, strata = rating)
review_folds


doParallel::registerDoParallel()
set.seed(2020)
lasso_grid <- tune_grid(
  lasso_wf,
  resamples = review_folds,
  grid = lambda_grid,
  metrics = metric_set(roc_auc, ppv, npv)  
)

lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = show.legend = FALSE) +
  facet_wrap(~.metric) +
  scale_x_log10()

best_auc <- lasso_grid %>% 
  select_best("roc_auc")

final_lasso <- finalize_workflow(lasso_wf, best_auc)

final_lasso

library(vip)

final_lasso %>% 
  fit(review_train) %>% 
  pull_workflow_fit() %>% 
  vi(lambda = best_auc$penalty) %>% 
  group_by(Sign) %>% 
  top_n(20, wt = abs(Importance)) %>% 
  ungroup() %>%
  mutate(Importance = abs(Importance),
         Variable = str_remove(Variable, "tfidf_text_"),
         Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sign, scales = "free_y")



review_final <- last_fit(final_lasso, review_split)

review_final %>% 
  collect_metrics()

review_final %>% 
  collect_predictions() %>% 
  conf_mat(rating, .pred_class)



