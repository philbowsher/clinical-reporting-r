library(tidyverse)
library(gt)

source("03-dataset_efficacy.R")

# Use `gt_14_3_05` (this is the formatted table)

# Demonstrate the following:
# - row styling
# - cell styling
# - modifying style of existing footnotes
# - adding source note (e.g., DRAFT)
# - different output formats (e.g., RTF, Word)


gt_14_3_05 %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_stub(rows = matches("  n"))
  ) %>%
  gtsave("gt_14_3_05.docx")
