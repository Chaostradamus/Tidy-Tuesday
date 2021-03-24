library(tidytuesdayR)
library(scales)
library(tidyverse)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load('2021-03-23')
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

tt <- tt_load("2021-03-23")

unvotes <- tt$unvotes %>% 
  mutate(vote_number = match(vote, c("no", "abstain", "yes")) - 2) %>% 
  left_join(tt$roll_calls %>% 
              select(rcid, date, amend), by = "rcid")



unvotes %>% 
  count(country, sort = T)

summarize_votes <- function(tbl, min_votes = 10) {
  tbl %>% 
    summarize(n_votes = n(),
              n_yes = sum(vote =="yes"),
              pct_yes = n_yes / n_votes,
              .groups = "drop") %>% 
              filter(n_votes >= min_votes) %>% 
              arrange(desc(pct_yes))
}

by_country <- unvotes %>% 
  group_by(country, country_code) %>% 
  summarize_votes()

by_country %>% 
 slice(1:20, (n() - 10:n())) %>% 
  mutate(country = fct_reorder(country, pct_yes)) %>% 
  ggplot(aes(pct_yes, y = country)) +
  geom_point(aes(size = n_votes)) +
  scale_x_continuous(labels = percent) +
  labs(x = "% of yes votes in the UN",
       title = "What countries voted yes gthe least")


library(lubridate)

by_year <- unvotes %>% 
  group_by(year = year(date)) %>% 
  summarize_votes()

by_year %>% 
  ggplot(aes(year, pct_yes)) +
  geom_line() +
  expand_limits(y = 0)

by_country_year <- unvotes %>%
  bind_rows(unvotes %>%  mutate(country = "Overall")) %>% 
  group_by(year = year(date), country, country_code) %>% 
  summarize_votes()

by_country_year %>% 
  filter(country %in% c("United States", "Canada", "Mali")) %>%
  mutate(country = fct_reorder(country, pct_yes)) %>% 
  ggplot(aes(year, pct_yes)) +
  geom_line(data = by_year, size = 2) +
  geom_line(aes(color = country)) +
  scale_y_continuous(labels = percent) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  expand_limits(y = 0) +
  labs(y = "% yes votes")

by_country_year %>% 
  filter(country %in% c("United States", "Canada", "Mali", "Isreal", "Germany", "France")) %>%
  mutate(country = fct_reorder(country, pct_yes)) %>% 
  ggplot(aes(year, pct_yes, color = country)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  expand_limits(y = 0) +
  facet_wrap(~ country) +
  theme(legend.position = "none")

#world map
library(ggthemes)
library(fuzzyjoin)

world_data <- map_data("world") %>% 
  as_tibble() %>% 
  regex_left_join(maps::iso3166 %>% 
                    select(mapname, country_code = a2),
                  c(region = "mapname"))

world_data %>% 
  left_join(by_country, by = "country_code") %>% 
  ggplot(aes(long, lat, fill = pct_yes)) +
  geom_polygon() +
  theme_map() +
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       midpoint = .5,
                       labels = percent) +
  labs(fill = "% of votes yes")

library(countrycode)

plot_by <- function(tbl, category) {
  tbl %>% 
    filter(!is.na({{ category }})) %>%
    mutate(category = fct_reorder({{ category }}, pct_yes)) %>% 
    ggplot(aes(year, pct_yes)) +
    geom_line(aes(color = category)) +
    scale_y_continuous(labels = percent) +
    scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
    expand_limits(y = 0) +
    labs(y = "% yes votes",
         x = "Year")
}



unvotes %>% 
 group_by(continent = countrycode(country_code, "iso2c", "continent"),
          year = year(date)) %>% 
  summarize_votes() %>% 
 plot_by(continent)

library(WDI)           

country_incomes <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.PP.KD",
                                     pop = "SP.POP.TOTL"),
                       start = 2005, end = 2005, extra = TRUE) %>% 
  as.tibble() %>% 
  select(country_code = iso2c, income, gdp_per_capita, pop) %>% 
  filter(!is.na(income)) %>% 
  mutate(income = fct_relevel(income, "Low income", "Lower middle income", "Upper
                              middle income"))
unvotes %>% 
  inner_join(country_incomes, by = "country_code") %>% 
  group_by(income, year = year(date)) %>% 
  summarize_votes() %>% 
  plot_by(income)

##correlation

unvotes %>% 
  filter(country %in% c("India", "Canada")) %>% 
  select(rcid, country, vote_number) %>% 
  spread(country, vote_number, fill = 0) %>% 
  summarize(correlation = cor(Canada, India))

library(widyr)

country_correlations <- unvotes %>% 
  filter(country != "Zanzibar") %>% 
  pairwise_cor(country, rcid, vote_number, sort = TRUE) %>% 
  mutate(continent1 = countrycode(item1, "country.name", "continent"),
         continent2 = countrycode(item2, "country.name", "continent"))

country_correlations %>%   
  filter(item1 == "Australia") %>%
  slice(c(1:16, n() - 0:15)) %>% 
  mutate(country = fct_reorder(item2, correlation)) %>% 
  ggplot(aes(correlation, country)) +
  geom_errorbarh(height = 0, aes(xmin = correlation, xmax = 0)) +
  geom_point() +
  labs(x = "How much this country's voting is correlated with the US",
       y = "")

country_correlations %>% 
  group_by(country = item1) %>% 
             summarize(median_correlation = median(correlation),
                       avg_correlation = mean(correlation)) %>%
             arrange(desc(median_correlation)) %>% 
             View()

country_correlations %>% 
  filter(!is.na(continent1), !is.na(continent2)) %>% 
  group_by(continent1, continent2) %>% 
  summarize(avg_correlation = mean(correlation)) %>% 
  arrange(desc(avg_correlation))
    
country_correlations %>% 
  filter(!is.na(continent1), !is.na(continent2)) %>%
  filter(continent1 == continent2) %>% 
  group_by(item1) %>% 
  summarize(avg_intracontinental_correlation = mean(correlation)) %>% 
  arrange((avg_intracontinental_correlation))

country_correlations %>% 
  filter("German Democratic Republic" == item1)

tt$issues %>% 
  count(issue)

library(tidytext)

rc_words <- tt$roll_calls %>%
  filter(!is.na(short)) %>% 
  unnest_tokens(word, short) %>% 
  anti_join(stop_words, by = "word") %>% 
  distinct(rcid, word) %>%
  add_count(word, name = "word_count") %>%
  filter(word_count >= 100)
  


unvotes %>% 
  inner_join(rc_words, by = "rcid") %>% 
  filter(country %in% c("United Kingdom", "United States")) %>%  
  group_by(word, country) %>% 
  summarize_votes(min_votes = 100) %>% 
  mutate(word = fct_reorder(word, pct_yes, function(x) max(x) - min(x))) %>% 
  ggplot(aes(pct_yes, word)) + 
  geom_point(aes(size = n_votes, color = country)) +
  expand_limits(x = 0) +
  scale_x_continuous(labels = percent) +
  labs(x = "% yes",
       y = "")

by_country_word <- unvotes %>% 
  inner_join(rc_words, by = "rcid") %>% 
  group_by(word, country) %>% 
  summarize_votes(min_votes = 0 )

by_country_word %>% 
  widely_svd(word, country,  pct_yes) %>% 
  filter(dimension == 2) %>% 
  mutate(word = reorder_within(word, value, dimension)) %>% 
  top_n(30, abs(value)) %>%
  ggplot(aes(value, word)) +
  geom_col() +
  scale_y_reordered()

# he reccomends topic modelling with stm package
