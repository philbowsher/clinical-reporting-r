library(tidyverse)
library(gt)

source("03-dataset_efficacy.R")

#
# Use `gt_14_3_05` (this is the formatted table)

# Let's have a look at `gt_14_3_05` in the Viewer (it's a `gt_tbl`)
gt_14_3_05

# Demonstrate the following:
# - row styling
# - cell styling
# - modifying style of existing footnotes
# - adding source note (e.g., DRAFT)

#
# We can export tables to RTF and Word document files with
# the `gtsave()` function
#

# To export a gt table to an RTF file, supply a filename with the
# .rtf extension
gtsave(gt_14_3_05, filename = "gt_14_3_05.rtf")

# To export a table as a Word document, use the .docx extension
gtsave(gt_14_3_05, filename = "gt_14_3_05.docx")


# Let's do a few things to modify this built table; after all, it is a
# gt table so we can use gt functions to modify the content and appearance


# (1) Styling Table Cells with `tab_style()`: We can target specific cells
# and modify the styling of cells; let's give all the body cells in the 'n'
# rows a lightblue background
gt_14_3_05 %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(rows = matches("^\\s+n"))
  )

# This can be extended to include the corresponding stub cells by
# adding the `cells_stub()` helper (enclosing in a `list()`)
gt_14_3_05 %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = list(
      cells_body(rows = matches("^\\s+n")),
      cells_stub(rows = matches("^\\s+n"))
    )
  )

# We can target specific cells in the body with a precise use of
# `cells_body()` inside of `tab_style()`; here all the p-values will
# be changed to bold lettering
gt_14_3_05 %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = 4, rows = starts_with("p-value"))
  )

# (2) Using `opt_*()` functions: with `opt_footnote_marks()` we can change
# the marks from numbers to letters
gt_14_3_05 %>%
  opt_footnote_marks(marks = letters)

