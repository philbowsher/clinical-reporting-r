library(safetyData)
library(tidyverse)


# Big N
big_n <- adam_adsl %>%
  group_by(TRT01A) %>%
  summarise(N = n())

long_big_n <- big_n %>%
  pivot_longer(-TRT01A, names_to = "param")

# Population n's
map(adam_adsl, function(x){
  attr(x, "label")
})
adam_adsl %>%
  group_by(TRT01A) %>%
  summarise(across(c(SAFFL, ITTFL, EFFFL, starts_with("COMP")),
                   ~sum(. == "Y"))) %>%
  pivot_longer(-TRT01A) %>%
  left_join(big_n, by = "TRT01A") %>%
  mutate(pct = value/N*100) %>%
  select(-N, n= value) %>%
  pivot_longer(c("n", "pct"), names_to = "param") %>%
  case_when(
    name == "SAFFL" ~ "Safety Population",
    name == "ITTFL" ~ "Intent-To-Treat Population",
    name == "EFFFL" ~ "Efficacy Population",
    name == "COMP8FL" ~ "Completers of Week 8 Population",
    name == "COMP16FL" ~ "Completers of Week 16 Population",
    name == "COMP24FL" ~ "Completers of Week 24 Population",

  )


