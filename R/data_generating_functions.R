library(tidyverse)
library(lubridate)
library(magrittr)

df <- as_date(now()) %>% {seq(. - ddays(30), ., by = "1 day")} %>%
  crossing(
    id = seq_len(length(.)),
    group_id = factor(1:3),
    date = .
  ) %>%
  mutate(., val = rnorm(mean = 100, sd = 15, n = nrow(.))) %>%
  arrange(group_id, id, date)

d1 <- crossing(id = 1:100, group_id = factor(1)) %>% mutate(val = rnorm(mean = 0, sd = 5, n = nrow(.)))
d2 <- crossing(id = 1:100, group_id = factor(2)) %>% mutate(val = rnorm(mean = 5, sd = 5, n = nrow(.)))

d <- bind_rows(d1, d2)

ggplot(data = d, aes(x = val, group = group_id, color = group_id, fill = group_id)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = filter(d, group_id == 1) %$% mean(val)) +
  geom_vline(xintercept = filter(d, group_id == 2) %$% mean(val)) +
  ggtitle("Distribution per group") +
  theme_minimal()