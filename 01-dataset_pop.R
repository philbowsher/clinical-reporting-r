library(tidyverse)
library(gt)

# Let's create the `pop_ard` table by sourcing in an .R script
source("ARD/make_pop.R")

# Have a quick look at `pop_ard` in the console (it's a tibble)
pop_ard

#
# Let's do some prep work before we get to making our table...
# We will get the big N values, make labels for our eventual columns,
# and put them into a list for easy access
#

big_n_table <-
  pop_ard %>%
  filter(param == "N") %>%
  mutate(label = paste0(TRT01A, "  \n(N=", value, ")")) %>%
  select(TRT01A, label)

big_n_table

big_n_list <- as.list(big_n_table$label)
names(big_n_list) <- big_n_table$TRT01A

big_n_list

#
# We need to shape our input data so that it is as close as possible
# to a form that could be tabulated (we'll use dplyr and tidyr for this)
#

revised_input_data <-
  pop_ard %>%
  arrange(param, order) %>%
  mutate(name = forcats::fct_inorder(name)) %>%
  filter(param != "N") %>%
  pivot_wider(names_from = c(TRT01A, param), values_from = value) %>%
  select(-order)

revised_input_data

#
# Now, let's make a gt table one step at a time
#

# (1) Introduce `revised_input_data` to `gt()` and specify a stub column;
# a stub is used for row labels, the `name` column will be used for this

gt_tbl <- gt(data = revised_input_data, rowname_col = "name")

gt_tbl


# (2) Let's format any percentage value columns so that they have percent
# signs; we need the `fmt_percent()` formatter function to do this (we'll
# make sure that the values aren't scaled by gt)

gt_tbl <-
  gt_tbl %>%
  fmt_percent(
    columns = ends_with("pct"),
    decimals = 0,
    scale_values = FALSE
  )

gt_tbl


# (3) Those total columns are in the wrong spot (both of them); we can move
# them both to the far right with `cols_move_to_end()`

gt_tbl <-
  gt_tbl %>%
  cols_move_to_end(columns = starts_with("Total"))

gt_tbl


# (4) We actually want single columns with `n` values and percentage values
# in parentheses; there's a special 'column merging' function just for this
# purpose, called `cols_merge_n_pct()`, and we'll invoke it four times
gt_tbl <-
  gt_tbl %>%
  cols_merge_n_pct(col_n = Placebo_n, col_pct = Placebo_pct) %>%
  cols_merge_n_pct(col_n = `Xanomeline High Dose_n`, col_pct = `Xanomeline High Dose_pct`) %>%
  cols_merge_n_pct(col_n = `Xanomeline Low Dose_n`, col_pct = `Xanomeline Low Dose_pct`) %>%
  cols_merge_n_pct(col_n = Total_n, col_pct = Total_pct)

gt_tbl


# (5) We made specialized column labels earlier (and put them into the
# `big_n_list` object); Let's assign those labels to the four columns
# and use the `md()` helper function to interpret those labels as Markdown
gt_tbl <-
  gt_tbl %>%
  cols_label(
    Placebo_n = md(big_n_list$Placebo),
    `Xanomeline High Dose_n` = md(big_n_list$`Xanomeline High Dose`),
    `Xanomeline Low Dose_n` = md(big_n_list$`Xanomeline Low Dose`),
    Total_n = md(big_n_list$Total)
  )

gt_tbl


# (6) Let's add a title and a subtitle to the table; this is done with
# the `tab_header()` function
gt_tbl <-
  gt_tbl %>%
  tab_header(
    title = "Table 14-1.01",
    subtitle = "Summary of Populations"
  )

gt_tbl


# (7) Finally, let's make all of the data columns the same fixed width
# of 125px and the stub column much wider (250px)
gt_tbl <-
  gt_tbl %>%
  cols_width(
    1 ~ px(250),
    everything() ~ px(125)
  )

gt_tbl

#
# These are just some of things you can do with gt, however, for Pharma-specific
# tables, you may want something more in tune with Pharma-based workflows:
#
# █████       ██████                             █████
# ░░███       ███░░███                           ░░███
# ███████    ░███ ░░░  █████████████   ████████  ███████
# ░░░███░    ███████   ░░███░░███░░███ ░░███░░███░░░███░
# ░███    ░░░███░     ░███ ░███ ░███  ░███ ░░░   ░███
# ░███ ███  ░███      ░███ ░███ ░███  ░███       ░███ ███
# ░░█████   █████     █████░███ █████ █████      ░░█████
# ░░░░░   ░░░░░     ░░░░░ ░░░ ░░░░░ ░░░░░        ░░░░░
#
# (tfmrt)
#


