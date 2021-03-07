library(tidyverse)

tuition_cost <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv")

diversity_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv")
 
diversity_school <- diversity_raw %>%   
  filter(category == "Total Minority") %>%
  mutate(total_minority = enrollment / total_enrollment)


diversity_school %>% 
  ggplot(aes(total_minority)) +
  geom_histogram(alpha = 0.7)

university_df <- diversity_school %>% 
  transmute(diversity = case_when(total_minority > .3 ~ "high",
                                  TRUE ~ "low"),
            name, state,
            total_enrollment) %>% 
  inner_join(tuition_cost %>%
               select(name, type, degree_length,
                      in_state_tuition:out_of_state_total)) %>% 
  left_join(tibble(state = state.name, region = state.region)) %>% 
  select(-state, -name) %>% 
  mutate_if(is.character, factor)

skimr::skim(university_df)


university_df %>% 
  ggplot(aes(type, in_state_tuition, fill = diversity)) +
  geom_boxplot() +
  facet_wrap(~region) +
  scale_y_continuous(labels = scales::dollar_format())


university_df %>% 
  ggplot(aes(type, total_enrollment, fill = diversity)) +
  geom_boxplot() +
  scale_y_log10()


library(tidymodels)  

set.seed(1234)
uni_split <- initial_split(university_df, strata = diversity)

uni_train <- training(uni_split)
uni_test <- testing(uni_split)

uni_rec <- recipe(diversity ~ ., data = uni_train) %>% 
  step_corr(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

juice(uni_rec)


glm_spec <- logistic_reg() %>% 
  set_engine("glm")

glm_fit <- glm_spec %>% 
  fit(diversity ~ ., data = juice(uni_rec))

glm_fit


knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

knn_fit <- knn_spec %>% 
  fit(diversity ~ ., data = juice(uni_rec))

knn_fit

tree_spec <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_fit <- tree_spec %>% 
  fit(diversity ~ ., data = juice(uni_rec))

tree_fit


set.seed(123)
folds <- vfold_cv(juice(uni_rec), strata = diversity)

set.seed(234)
glm_rs <- glm_spec %>% 
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE))



set.seed(234)
knn_rs <- knn_spec %>% 
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE))

set.seed(234)
tree_rs <- tree_spec %>% 
  fit_resamples(diversity ~ .,
                folds,
                metrics = metric_set(roc_auc, sens, spec),
                control = control_resamples(save_pred = TRUE))


glm_rs %>% 
  unnest(.predictions) %>% 
  mutate(model = "glm") %>% 
  bind_rows(tree_rs %>% 
              unnest(.predictions) %>% 
              mutate(model = "tree")) %>% 
  bind_rows(knn_rs %>% 
              unnest(.predictions) %>% 
              mutate(model = "knn")) %>% 
  group_by(model) %>% 
  roc_curve(diversity, .pred_high) %>% 
  autoplot()

glm_fit %>% 
  predict(new_data = bake(uni_rec, new_data = uni_test),
          type = "prob") %>% 
  mutate(truth = uni_test$diversity) %>%
  roc_auc(truth, .pred_high)
  
