library(tidyverse)


unvotes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv")
issues <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv")


unvotes_df <- unvotes %>%
  select(country, rcid, vote) %>%
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>%
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)


library(tidymodels)

pca_rec <- recipe(~ ., data = unvotes_df) %>% 
  update_role(country, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors()) 

pca_prep <- prep(pca_rec)


bake(pca_prep, new_data = NULL) %>% 
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point(color = "midnightblue", alpha = .7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward")

pca_comps <- tidy(pca_prep, 2) %>% 
  filter(component %in% paste0("PC", 1:4)) %>% 
  left_join(issues %>%  mutate(terms = paste0("rcid_", rcid))) %>% 
  filter(!is.na(issue)) %>% 
  group_by(component) %>% 
  slice_max(abs(value), n = 8) %>% 
  ungroup()

pca_comps %>% 
  mutate(value = abs(value)) %>% 
  ggplot(aes(value, terms, fill = issue)) +
  geom_col(position = "dodge") +
  facet_wrap(~component, scales = "free_y") +
  labs(y = NULL, fill = NULL,
       x = "Absolute value of contirbution")
  





####

library(embed)

umap_rec <- recipe(~ ., data = unvotes_df) %>% 
  update_role(country, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_umap(all_predictors()) 

umap_prep <- prep(umap_rec)


bake(umap_prep, new_data = NULL) %>% 
  ggplot(aes(umap_1, umap_2, label = country)) +
  geom_point(color = "midnightblue", alpha = .7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward")
