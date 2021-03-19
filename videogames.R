library(tidyverse)
library(tidytuesdayR)
library(scales)
library(lubridate)
theme_set(theme_light())


tt <- tt_load("2021-03-16")

tibble(date = seq(as.Date("2020-01-1"), as.Date("2020-12-31"), 1)) %>% 
  mutate(month_number = month(date))

games <- tt$games %>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc) / 100) %>%
  mutate(date = ymd(paste(year, month, 1))) %>% 
  filter(date > min(date)) %>% 
  mutate(month = fct_reorder(month, month(date)))
        

games %>% 
  count(date) %>% 
  ggplot(aes(date, n)) +
  geom_col()



summarize_games <- function(tbl) {
  tbl %>%
    summarize(median_avg = median(avg),
              median_peak = median(peak))
}


games %>%
  group_by(date) %>%
  summarize_games() %>%
  ggplot(aes(date, median_avg)) +
  geom_line() +
  expand_limits(y = 0) +
  labs(x = "Month",
       y = "Median popularity of a game on Steam")

games %>% 
  filter(date < "2020-01-01") %>% 
  group_by(month) %>% 
  summarize_games() %>%  
  ggplot(aes(month, median_avg)) +
  geom_line(group = 1) +
  expand_limits(y= 0) +
  labs(x = "Month of year",
       y = "median popularity of a game on Steam",
       title = "Seasonal trend in Steam games",
       subtitle = "From 2012-2019")

games %>% 
  group_by(year, month) %>% 
  summarize_games() %>% 
  ggplot(aes(month, median_avg)) +
  geom_line(aes(color = factor(year), group = year)) +
  expand_limits(y= 0) +
  labs(x = "Month of year",
       y = "median popularity of a game on Steam",
       title = "Seasonal trend in Steam games",
       subtitle = "From 2012-2019")

games %>% 
  group_by(date) %>% 
  summarize_games() %>% 
  ggplot(aes(date, median_peak)) +
  geom_line() +
  expand_limits(y = 0)


games %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(avg)) +
  geom_histogram() +
  scale_x_log10(labels = comma,
                breaks = 10 ^ seq(0, 5)) +
  labs(x = "Average # of players across Feb 2021") 


games %>% 
  filter(avg >= 100) %>% 
  filter(date == max(date)) %>% 
  arrange((avg_peak_perc))

g <- games %>% 
  filter(avg >= 100) %>% 
  filter(date == "2021-01-01") %>% 
  arrange((avg_peak_perc)) %>% 
  ggplot(aes(avg, 1 / avg_peak_perc, label = gamename)) +
  geom_point() +
 geom_text(vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10(labels = comma) +
  labs(x = "Average # of players in Feb 2021",
       y = "Ratio of peak / average")




games %>% 
  filter(avg >= 1000,
         date == max(date),
         date >= "2018-01-01") %>% 
  arrange((avg_peak_perc)) %>% 
  ggplot(aes(avg, 1 / avg_peak_perc, label = gamename)) +
  geom_point() +
  geom_text(vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10(labels = comma) +
  labs(x = "Average # of players in Feb 2021",
       y = "Ratio of peak / average")



#library(plotly)
#ggplotly(g)

games %>% 
  filter(date == max(date)) %>% 
  arrange(desc(avg))


games %>% 
  filter(fct_lump(gamename, 16, w = avg) != "Other") %>% 
  mutate(gamename = fct_reorder(gamename, -avg)) %>% 
  ggplot(aes(date, avg)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ gamename, scales = "free_y") +
  labs(x = "month",
          y = "avg")
  
apr_feb_ratio <- games %>% 
  filter(year == 2020) %>% 
  select(gamename, month, avg, peak) %>% 
  pivot_wider(names_from = month, values_from = c(avg, peak)) %>% 
  select(gamename, contains("January"), contains("April"), contains("February")) %>% 
  mutate(apr_feb_ratio = avg_April / avg_February) %>% 
  filter(avg_January >= 100,
         avg_February >= 100) %>% 
  arrange(desc(apr_feb_ratio)) 

games %>%
  filter(date >= "2018-01-01") %>% 
  inner_join(apr_feb_ratios %>% 
               top_n(12, apr_feb_ratio), by = "gamename") %>% 
  mutate(gamename = as.character(gamename)) %>% 
  complete(gamename, date, fill = list(avg = 0)) %>% 
  mutate(gamename = fct_reorder(gamename, -avg)) %>% 
  ggplot(aes(date, avg)) +
  geom_line() +
  expand_limits(y = 0) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             color = "red", lty = 2) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ gamename, scales = "free_y") +
  labs(x = "month",
       y = "avg players of this game in this month")


