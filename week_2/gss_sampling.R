library(tidyverse)
library(gssr)

gss_2022 <- gss_get_yr(year = 2022) |>
  haven::zap_labels()

gss_2022 |>
  select(abany) |>
  group_by(abany) |>
  summarize(n = n())

d <- gss_2022 |>
  select(abany) |>
  drop_na() |>
  mutate(abany = if_else(abany == 1, 1, 0))


my_samples <- tibble(
  sample_id = 1:10000) |>
  rowwise() |>
  mutate(sample_mean = mean(rbinom(1345, 1, .594)))

ggplot(my_samples,
       aes(x = sample_mean)) +
  geom_histogram(binwidth=.005,
                 boundary = .589,
                 color = "white")

rbinom(1345, 1, .594) |>
  mean()

