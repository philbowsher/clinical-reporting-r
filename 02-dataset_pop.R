library(tidyverse)
library(tfrmt)

source("ARD/make_pop.R")

# Let's just setup the basics
my_tfrmt <- tfrmt(
  column = TRT01A,
  label = name,
  param = param,
  value = value
)

print_to_gt(tfrmt = my_tfrmt, .data = pop_ard)

# That is okay, but we want to round the percents, so we need a `body_plan`
# To add to a tfrmt you can just layer them together

my_rounded_tfrmt <- my_tfrmt %>%
  tfrmt(
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default",
                     frmt_combine("{n} ({pct}%)",
                                  n = frmt("xx"),
                                  pct = frmt("xx")))
    )
  )
print_to_gt(tfrmt = my_rounded_tfrmt, .data = pop_ard)

# This is looking good, but those population N's need to be in the column labels not the data so
# we need to add a big_n_structure
n_tfrmt <- my_rounded_tfrmt %>%
  tfrmt(
    big_n = big_n_structure(param_val = "N")
  )

print_to_gt(.tfrmt = n_tfrmt, .data = pop_ard)


# The final thing we need to do is sort out the columns
n_tfrmt %>%
  tfrmt(
    col_plan = col_plan(
      starts_with("Xanomeline"),
      "Placebo",
      "Total"
    )
  ) %>%
  print_to_gt(.data = pop_ard)


# If we had built this from a ground-up all in one it would look like
pop_tbl_tfrmt <- tfrmt(
  column = TRT01A,
  label = name,
  param = param,
  value = value,
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{n} ({pct}%)",
                                n = frmt("xx"),
                                pct = frmt("xx")))
  ),
  big_n = big_n_structure(param_val = "N"),
  col_plan = col_plan(
    starts_with("Xanomeline"),
    "Placebo",
    "Total"
  )
)

print_to_gt(tfrmt = pop_tbl_tfrmt, .data = pop_ard)

# We often are make mocks before making tables. To do that you just need to
# change the printing mechanism
print_mock_gt(tfrmt = pop_tbl_tfrmt)

TRT01A = c("Xanomeline Low Dose",
           "Xanomeline High Dose",
           "Placebo",
           "Total")
label = c("Completers of Week 8 Population",
          "Completers of Week 16 Population",
          "Completers of Week 24 Population",
          "Efficacy Population",
          "Intent-To-Treat Population",
          "Safety Population")
mock_data <- crossing(TRT01A = TRT01A,
                      param = c("n", "pct"),
                      name = label
                      ) %>%
  bind_rows(crossing(TRT01A = TRT01A,
                     param = c("N")))

print_mock_gt(tfrmt = pop_tbl_tfrmt, .data = mock_data)
