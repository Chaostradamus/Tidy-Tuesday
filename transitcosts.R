library(tidyverse)
library(tidytuesdayR)
library(scales)
library(glue)
theme_set(theme_light())



tt <- tt_load("2021-01-05")
library(countrycode)

transit_cost <- tt$transit_cost %>%
  filter(!is.na(e)) %>%
  mutate_at(vars(start_year, end_year, real_cost), as.numeric) %>%
  mutate(country_code = ifelse(country == "UK", "GB", country),
         country = countrycode(country_code, "iso2c", "country.name"),
         tunnel_per = tunnel / length,
         rr = ifelse(rr, "Railroad", "Not Railroad"))


transit_cost %>% 
  count(city, country, sort = TRUE)

transit_cost %>% 
  filter(country == "United States") %>%
  mutate(line = fct_reorder(line, year)) %>% 
  ggplot(aes(xmin = start_year, xmax = end_year, y = line,
             color = city,
             size = real_cost)) +
  geom_errorbarh(height = .1) +
  labs(x = "Year",
       y = "",
       color = "City")

transit_cost %>% 
  ggplot(aes(cost_km_millions)) +
  geom_histogram() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of usd)")

transit_cost %>%
  filter(!is.na(cost_km_millions),
         tunnel_per == 1) %>%
  mutate(country = fct_lump(country, 10)) %>%
  add_count(country) %>%
  mutate(country = glue("{ country } ({ n })"),
         country = fct_reorder(country, cost_km_millions)) %>%
  ggplot(aes(cost_km_millions, country)) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "")

transit_cost %>% 
  filter(!is.na(cost_km_millions),
        tunnel_per == 1,
        country == "China") %>%
  mutate(city = fct_lump(city, 10)) %>%
  add_count(city) %>%
  mutate(city = glue("{ city } ({ n })"),
         city = fct_reorder(city, cost_km_millions)) %>%
  ggplot(aes(cost_km_millions, city)) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "") +
  expand_limits(x = 0)


transit_cost %>% 
  filter(country == "China",
         city == "Shanghai",
         !is.na(start_year),
         !is.na(end_year)) %>% 
  mutate(city = fct_lump(city, 5)) %>% 
  mutate(line = fct_reorder(line, year)) %>% 
  ggplot(aes(xmin = start_year, xmax = end_year, y = line,
             color = city,
             size = real_cost)) +
  geom_errorbarh(height = .1) +
  labs(x = "Year",
       y = "",
       color = "City")

transit_cost %>% 
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>%
  
  group_by(year = (year %/% 5) * 5) %>% 
  summarize(median_cost_km = median(cost_km_millions),
            n = n()) %>% 
  ggplot(aes(year, median_cost_km)) +
  geom_line() +
  geom_point(aes(size = n))

transit_cost %>% 
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>%
  mutate(year = (year %/% 5) * 5,
         city = fct_lump(city, 5)) %>% 
  ggplot(aes(year, cost_km_millions, group = year)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(aes(color = city), height = 0, width = 1) +
  expand_limits(y = 0) +
  labs(x = "Cost / km (Real USD, Millions)",
       x = "year",
       title = "Cost distribution / km in China")


transit_cost %>% 
  ggplot(aes(cost_km_millions, real_cost / length)) +
  geom_point()
  

transit_cost %>% 
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>%
  mutate(city = fct_lump(city, 4)) %>% 
  ggplot(aes(stations / length, cost_km_millions, size = length,
             color = city)) +
  geom_point() +
  expand_limits(x = 0, y = 0) +
  labs(x = "Stations / km", "Cost / kilometer",
       y = "Cost / km")
