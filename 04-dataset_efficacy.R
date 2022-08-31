library(tidyverse)
library(gt)

source("03-dataset_efficacy.R")

# Let's have a look at `gt_14_3_05` in the Viewer (it's a `gt_tbl`)
gt_14_3_05


#
# We can export tables to RTF and Word document files with
# the `gtsave()` function
#

# To export a gt table to an RTF file, supply a filename with the
# .rtf extension
gtsave(gt_14_3_05, filename = "gt_14_3_05.rtf")

# To export a table as a Word document, use the .docx extension
gtsave(gt_14_3_05, filename = "gt_14_3_05.docx")


#
# Let's do a few things to modify this built table; after all, it is a
# gt table so we can use gt functions to modify the content and appearance
#

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
gt_14_3_05 %>% opt_footnote_marks(marks = letters)

# If you'd like the header to be aligned different, like to the left,
# the `opt_align_table_header()` function can be used for that
gt_14_3_05 %>% opt_align_table_header(align = "left")

# The `opt_stylize()` function gives you access to lots of styling options
# to radically change the look of the whole table
gt_14_3_05 %>% opt_stylize(style = 1, color = "blue")


# (3) Using the options in `tab_options()`: there are so many options available
# in that one function; we'll run through just a few examples here, starting
# with super sizing your heading titles
gt_14_3_05 %>%
  tab_options(
    heading.title.font.size = px(26),
    heading.subtitle.font.size = px(18)
  )

# It's possible to change properties of entire table locations like the
# heading, column labels, stub, and footnotes section
gt_14_3_05 %>%
  tab_options(
    heading.background.color = "gray",
    column_labels.font.weight = "bold",
    column_labels.background.color = "bisque",
    stub.background.color = "steelblue",
    footnotes.background.color = "darkgreen"
  )

# (4) Adding a source note with `tab_source_note()`: it's still possible to
# add table notes to the footer (add as many as needed)
gt_14_3_05 %>%
  tab_source_note(source_note = md("Source note added after using **tfrmt**.")) %>%
  tab_source_note(source_note = "Table generated with the 'gt_14_3_05' object.")

#
# This is it!
# See:
# - `gt.rstudio.com` for information/documentation on gt
# - `gsk-biostatistics.github.io/tfrmt/` for everything on tfrmt
#
