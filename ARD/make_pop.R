library(safetyData)
library(tidyverse)

adam_adsl <-
  adam_adsl %>%
  mutate(TRT01A = "Total") %>%
  bind_rows(adam_adsl)

# Big N
big_n <-
  adam_adsl %>%
  group_by(TRT01A) %>%
  summarise(N = n())

long_big_n <-
  big_n %>%
  pivot_longer(-TRT01A, names_to = "param")

# Population n's
pop_ard <-
  adam_adsl %>%
  group_by(TRT01A) %>%
  summarise(across(c(SAFFL, ITTFL, EFFFL, starts_with("COMP")),
                   ~sum(. == "Y"))) %>%
  pivot_longer(-TRT01A) %>%
  left_join(big_n, by = "TRT01A") %>%
  mutate(pct = value/N*100) %>%
  select(-N, n = value) %>%
  pivot_longer(c("n", "pct"), names_to = "param") %>%
  mutate(
    # Add order columns
    order = case_when(
      name == "SAFFL" ~ 1,
      name == "ITTFL" ~ 2,
      name == "EFFFL" ~ 3,
      name == "COMP8FL" ~ 4,
      name == "COMP16FL" ~ 5,
      name == "COMP24FL" ~ 6
    ),
    name = case_when(
    name == "SAFFL" ~ "Safety Population",
    name == "ITTFL" ~ "Intent-To-Treat Population",
    name == "EFFFL" ~ "Efficacy Population",
    name == "COMP8FL" ~ "Completers of Week 8 Population",
    name == "COMP16FL" ~ "Completers of Week 16 Population",
    name == "COMP24FL" ~ "Completers of Week 24 Population"
    )
  ) %>%
  bind_rows(long_big_n)

rm(adam_adsl, big_n, long_big_n)
