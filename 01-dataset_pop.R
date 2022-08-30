library(tidyverse)
library(gt)

source("ARD/make_pop.R")

big_n_rows <-
  pop_ard %>%
  filter(param == "N") %>%
  mutate(label = paste0(TRT01A, " \n(N=", value, ")"))

revised_input_data <-
  pop_ard %>%
  arrange(param, order) %>%
  mutate(name = forcats::fct_inorder(name)) %>%
  filter(param != "N") %>%
  pivot_wider(names_from = c(TRT01A, param), values_from = value) %>%
  select(-order)

revised_input_data %>%
  gt(rowname_col = "name") %>%
  fmt_percent(
    columns = ends_with("pct"),
    decimals = 0,
    scale_values = FALSE
  ) %>%
  cols_move_to_end(columns = starts_with("Total")) %>%
  cols_merge_n_pct(col_n = Placebo_n, col_pct = Placebo_pct) %>%
  cols_merge_n_pct(col_n = `Xanomeline High Dose_n`, col_pct = `Xanomeline High Dose_pct`) %>%
  cols_merge_n_pct(col_n = `Xanomeline Low Dose_n`, col_pct = `Xanomeline Low Dose_pct`) %>%
  cols_merge_n_pct(col_n = Total_n, col_pct = Total_pct) %>%
  cols_label(
    Placebo_n = md(big_n_rows[big_n_rows$TRT01A == "Placebo", ][["label"]]),
    `Xanomeline High Dose_n` = md(big_n_rows[big_n_rows$TRT01A == "Xanomeline High Dose", ][["label"]]),
    `Xanomeline Low Dose_n` = md(big_n_rows[big_n_rows$TRT01A == "Xanomeline Low Dose", ][["label"]]),
    Total_n = md(big_n_rows[big_n_rows$TRT01A == "Total", ][["label"]])
  ) %>%
  tab_header(
    title = "Table 14-1.01",
    subtitle = "Summary of Populations"
  )
