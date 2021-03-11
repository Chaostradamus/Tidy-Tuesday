library(tidyverse)
library(tidytuesdayR)
library(scales)
tt <- tt_load("2021-03-09")

movies <- tt$movies %>% 
  mutate(across(contains("gross"), parse_number))
  
  
View(movies)

movies %>% 
  count(year, binary) %>% 
  ggplot(aes(year, n, fill  = binary)) +
  geom_col()

summarize_bechdel <- function(tbl) {
  tbl %>% 
    summarize(n_movies = n(),
              pct_pass = mean(binary == "PASS"),
              median_int_gross_2013 = median(intgross_2013, na.rm = TRUE))
  %>% 
    arrange(desc(n_movies))
}


movies %>% 
  group_by(decade = 10 * (year %% 10)) %>% 
  summarize_bechdel() %>% 
  ggplot(aes(decade, pct_pass)) +
  geom_line() +
  geom_point(aes(size = n_movies)) +
  expand_limits(y = 0)

movies %>% 
  arrange(desc(intgross_2013)) %>% 
  head(25) %>% 
  mutate(title = fct_reorder(title, intgross_2013)) %>% 
  ggplot(aes(intgross_2013, title, fill = binary)) +
  geom_col()


movies %>% 
  filter(year >= 1990) %>% 
  ggplot(aes(intgross_2013, fill = binary)) +
  geom_density(alpha = .5) +
  scale_x_log10()
  

lm(log2(intgross_2013) ~ binary, data = movies) %>% 
  summary()


library(ggrepel)

movie_genres <- movies %>% 
  separate_rows(genre, sep = ", ") %>% 
  filter(year >= 1990, !is.na(genre))

movie_genres %>% 
  group_by(genre) %>% 
  summarize_bechdel() %>% 
  head(12) %>% 
  ggplot(aes(median_intgross_2013, pct_pass)) +
  geom_point(aes(size = n_movies)) +
  geom_text_repel(aes(label = genre)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = percent) +
  labs(x = "Median Intgernational Gross (2013 dollars)",
       y = "% that pass the bechdel test")



movie_genres %>% 
  filter(fct_lump(genre, 9) != "Other") %>% 
  ggplot(aes(intgross_2013, fill = binary)) +
  geom_density(alpha = .5) +
  facet_wrap(~ genre) +
  scale_x_log10(labels = dollar) +
  labs(x = "International Gross (2013 dollars)")


movies %>% 
  filter(fct_lump(genre, 9) != "Other") %>% 
  ggplot(aes(intgross_2013, genre,  fill = binary)) +
  geom_boxplot() +
  scale_x_log10(labels = dollar) +
  labs(x = "International Gross (2013 dollars)")



movies %>% 
  filter(year >= 1990) %>% 
  separate_rows(genre, sep = ", ") %>% 
  mutate(value = 1) %>% 
  spread(genre, value, fill = 0) %>% 
  lm(log2(intgross_2013) ~ Action + Crime + Adventure +
       `Sci-Fi` + Thriller + Fantasy + Comedy +
        binary, data = .) %>% 
  summary()

genre_reasons <- movie_genres %>% 
  count(genre = fct_lump(genre, 8),
        clean_test,
        sort = TRUE) %>%
  group_by(genre) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup()

library(tidytext)

genre_reasons %>% 
  filter(clean_test != "dubious") %>% 
  mutate(clean_test = fct_relevel(clean_test, "nowomen", "notalk", "men"),
         clean_test = fct_recode(clean_test,
                                 "Passes" = "ok",
                                 "Has two women..." = "nowomen",
                                 "...who talk to each other..." = "notalk",
                                 "...about something other than a man" = "men")) %>%
  mutate(genre = reorder_within(genre, pct, clean_test)) %>% 
  ggplot(aes(pct, genre)) +
  geom_col() +
  facet_wrap(~ clean_test, scales = "free_y") +
  scale_x_continuous(labels = percent) +
  scale_y_reordered() +
  labs(title = "Which step do movies fail on",
       y = "",
       x = "% of movies in this genre that fail at this step")




movies %>% 
  filter(!is.na(director)) %>% 
  group_by(director = fct_lump(director, 12)) %>%
  summarize_bechdel() %>% 
  arrange(desc(pct_pass))


movies %>% 
  filter(director == "Martin Scorsese") %>% 
  arrange(desc(year))

movie_words <- movies %>% 
  filter(!is.na(writer)) %>% 
  separate_rows(writer, sep = ", ") %>% 
  mutate(writer = str_remove(writer, " \\(.*")) %>% 
  distinct(title, writer, .keep_all = TRUE) %>% 
  group_by(writer = fct_lump(writer, 10)) %>% 
  summarize_bechdel()




movie_words %>% 
  filter(!is.na(plot)) %>% 
  unnest_tokens(word, plot) %>% 
  anti_join(stop_words, by = "word") %>% 
  distinct(word, title, .keep_all = TRUE) %>%  
  group_by(word = fct_lump(word, 50)) %>% 
  summarize_bechdel() %>% 
  arrange(desc(pct_pass)) 

glmnet

library(glmnet)

movie_features <- bind_rows(
  movie_words %>% distinct(imbd, title, binary, feature = word),
  movie_genres %>% distinct(imbd, title, binary, feature  = genre)) %>% 
  add_count(feature) %>% 
  filter(n >= 10)


movie_features %>% 
  cast_sparse(imbd, feature)

pass <- movies$binary[match(rownames(movie_feature_matric), movies$imbd)] == "PASS"

mod <- cv.glmnet(movie_feature_matric, pass, family = "binomial")

plot(mod)

tidy(mod$glmnet.fit) %>% 
  filter(lamba == mod$lambda.1se,
         term != "(Intercept)") %>% 
  top_n(40, abs(estimate)) %>%
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term)) +
  geom_col() +
  labs(y = "Word/Genre",
       x = "Coefficient towards of passing Bechdel Test")

