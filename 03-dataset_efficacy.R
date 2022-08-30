## Create Efficacy Table (14-3.05) --------------------------------------------------------

library(tfrmt)
library(gt)
library(callr)

## Load Efficacy ARD ---------------------------------------------------------

efficacy_ARD <- callr::r(function(){
  source(here::here("ARD/make_efficacy_ARD.R"))
  efficacy_ARD
})



## Define tfrmt for efficacy tables -------------------------------------------

# Generally organizations would have this pre-defined, and ARD's build to spec
# to match their predefined tfrmts

### Define ARD columns of importance ------------------------------------------

## Sort out which columns exist, and what they contain
head(efficacy_ARD)

tfrmt_eff <- tfrmt(

  ## define parameters of ARD to tfrmt
  column = c(treatment, dose), ## columns in ARD defining cols in table
  group = row_label_group, ## Grouping of rows/data
  label = row_label, ## Row-level labels
  param = param, ## Defines value "types"
  value = values, # values to display in table body

  ## define order to display
  sorting_cols = c(ord_group, ord_row_lab)
)

## NOTE: Since we do not know which columns are actually in the data, if you
## have an object defined in your environment, when you create a tfrmt that
## requires a column name that is the same as the object, use quotes. otherwise
## it will use the objects values:

fake_obj <- "132456"

tfrmt(column = fake_obj)$column
tfrmt(column = "fake_obj")$column

### Define Body Plan - Basics ----------

# - frmt_structure defines:
#    - Group, Row, and format to apply
# - format types:
#     Covered Previously
#      - frmt()
#      - frmt_combine()

efficacy_ARD %>%
  dplyr::filter(param != "big_n") %>%
  dplyr::group_by(row_label) %>%
  dplyr::summarise(
    param_grp = paste(unique(param), collapse = ", ")
  ) %>%
  dplyr::group_by(param_grp) %>%
  dplyr::summarise(
    row_labels = paste(unique(row_label), collapse = ", ")
  )

# Single param into a single cell
frmt_structure(
  group_val = ".default",
  label_val = "n",
  n = frmt("xx.xx")
)

# Combining params into a single cell
frmt_structure(
  group_val = ".default",
  label_val = "Median (Range)",
  frmt_combine("{Median} ({Range_min};{Range_max})",
               Median = frmt("xx.x"),
               Range_min = frmt("xx"),
               Range_max = frmt("xx"), missing = " ")
)

# Add basic frmts

tfrmt_eff <- tfrmt_eff %>%
  tfrmt(
    body_plan = body_plan(

      ## for all cases where row_label is "n" & param is "n", we want to use 2
      ## characters
      frmt_structure(
        group_val = ".default",
        label_val = "n",
        n = frmt("xx")
      ),

      ## these will combine the specified params into a single cell with the
      ## formatting defined for unique combinations of group_val, label_val, and
      ## for each column

      ### when row_label is "Median (Range)", paste together Median, min, max with
      ### respective styling
      frmt_structure(
        group_val = ".default",
        label_val = "Median (Range)",
        frmt_combine("{Median} ({Range_min};{Range_max})",
                     Median = frmt("xx.x"),
                     Range_min = frmt("xx"),
                     Range_max = frmt("xx"), missing = " ")
      ),

      ### when row_label is "Diff of LS Means (SE)", paste together diff and SE
      ### with respective styling
      frmt_structure(
        group_val = ".default",
        label_val = "Diff of LS Means (SE)",
        frmt_combine("{diff} ({diff_se})",
                     diff = frmt("xx.x"),
                     diff_se = frmt("xx.xx"), missing = " ")
      ),

      ### when row_label is "95% CI", it will paste together the upper and lower
      ### confidence intervals and SE with respective styling
      frmt_structure(
        group_val = ".default",
        label_val = "95% CI",
        frmt_combine("({diff_lcl};{diff_ucl})",
                     diff_lcl = frmt("xx.x"),
                     diff_ucl = frmt("xx.x"), missing = " ")
      ),

      ### when row_label is "Mean (SD), paste together Mean and SD with respective
      ### styling
      frmt_structure(
        group_val = ".default",
        label_val = "Mean (SD)",
        frmt_combine("{Mean} ({SD})",
                     Mean = frmt("xx.x"),
                     SD = frmt("xx.xx"), missing = " ")
      )
    )
  )

### Define Body Plan - Conditional Formatting  ----------

#  What about the p.value param?
#
#  Formatting may depend on the value. More than just rounding, but
#  commonly pvalues are limited to when values are less than 0.05 to
#  just displaying "<0.05", or adding asterisks to significant values, etc
#
#  Conditional Formatting with frmt's with frmt_when!
#  ?frmt_when

# Left side evaluates comparing against _input_ value to format.
# Right side is the frmt or output to be applied to the input value

conditional_frmt <- frmt_when(
  ">=10" ~ frmt("xx.x"),
  ">=1" ~ frmt("x.x"),
  "<1" ~ frmt("x.xx **"),
  "TRUE" ~ "MISSING VALUE"
)

# Preview how the frmt_when may impact the content
apply_frmt(
  frmt_def = conditional_frmt,
  .data = tibble::tibble(x = c(11,9,2,.005,NA)),
  values = rlang::quo(x)
)


tfrmt_eff <- tfrmt_eff %>%
  tfrmt(
    body_plan = body_plan(

      ### For the label values, apply conditional formatting to the cases where
      ### param is p.value based on the actual value
      frmt_structure(
        group_val = ".default",
        label_val = c("p-value (Dose Response)",
                      "p-value (Xanomeline High - Xanomeline Low)",
                      "p-value (Xanomeline - Placebo)"),
        p.value = frmt_when(
          ">0.99" ~ ">0.99",
          "<0.001" ~ "<0.001",
          TRUE ~ frmt("x.xxx", missing = " "))
      )
    )
  )


### Define "Big N's" ----------------------------------------------------------

# we know they are included in the ARD, so wh

efficacy_ARD %>%
  dplyr::filter(param == "big_n")


tfrmt_eff <- tfrmt_eff %>%
  tfrmt(
    ## define column "big N" dressings,
    big_n = big_n_structure(
      param_val = "big_n",
      n_frmt = frmt("\n(N=XX)")
    )
)


### Define Row Group Plan -----------------------------------------------------

# Some formatting is related to spacing around groups and row label placement
#
# `row_grp_plan()` is a collection of defining how rows will be displayed
#
#  -`row_grp_structure()` is passed to define how we may style groups and
#  display them. Multiple may be passed to a plan.
#  - `label_loc` argument allows user to define how groups and labels get combined

# set up a structure for blocking based on the groups
# collapse the groups with label and indent

tfrmt_eff <- tfrmt_eff %>%
  tfrmt(
    row_grp_plan = row_grp_plan(

      row_grp_structure(
        group_val = list(row_label_group = "Change from Baseline"),
        element_block(post_space = " ")
      ),

      row_grp_structure(
        group_val = list(row_label_group = "p-value (Dose Response)"),
        element_block(post_space = " ")
      ),
      row_grp_structure(
        group_val = list(row_label_group = "p-value (Xanomeline - Placebo)"),
        element_block(post_space = " ")
      ),

      label_loc = element_row_grp_loc(location = "indented")
    )
  )



### Define col_plan -----------------------------------------------------------

# We need to define the column order for which we want things to appear.
# by default all columns (between column columns and actual columns in ARD)
# are preserved. To drop non-defined columns, set ".drop" in in col_plan to TRUE.
#
# The column plan takes unquoted columns (can also optionally pass) as quoted.
#
# Behavior is _similar_ to dplyr::select, but goes with "last identified" model
# as opposed to "first idenfied" that tidyselect does. Renaming works similarly.
#
# If you want to define column orders for spanning header content, use the `span_structure()`
# function. This expects the argname to be the original column name then the
# values are a vector. Renaming uses named vectors.s
#


tfrmt_eff <- tfrmt_eff %>%
  tfrmt(
    ## Define order of columns
    col_plan = col_plan(
      row_label_group,
      row_label,
      Placebo,
      span_structure(
        treatment = `Xanomeline`,
        dose = c(`Low Dose`,`High Dose`)
      ),
      -starts_with("ord")
    )
  )

### Define footnote_plan -------------------------------------------------------

# - New feature Warning -
#
# Footnote plans are a series of footnote_structures and a definition for the type
# of footnote marks to use (numbers, letters, standard, extended)
#
# footnote_structures accept:
#  - footnote - actual footnote text
#  - group val - which groups that the footnote may apply to
#  - label val - ""
#  - column val - "". use lists with named
#
# If _just_ row or _just_ column defined, will be applied at the column label or
# row label level
#
# If both are defined, footnote is applied to cells that match the row/column
#

tfrmt_eff <- tfrmt_eff %>%
  tfrmt(footnote_plan = footnote_plan(
    footnote_structure(
      "Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate",
      group_val = list(
        row_label_group = c(
          "p-value (Dose Response)",
          "p-value (Xanomeline - Placebo)",
          "p-value (Xanomeline High - Xanomeline Low)"
        )
      )
    ),
    footnote_structure(
      "Test for a non-zero coefficient for treatment (dose) as a continuous variable.",
      group_val = list(row_label_group = c("p-value (Dose Response)"))
    ),
    footnote_structure(
      "Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.",
      group_val = list(
        row_label_group = c(
          "p-value (Xanomeline - Placebo)",
          "p-value (Xanomeline High - Xanomeline Low)"
        )
      )
    ))
  )

## Final tfrmt -----------------------------------------------------------------

tfrmt_eff_final <- tfrmt(

  ## define parameters of ARD to tfrmt
  group = row_label_group,
  label = row_label,
  param = param,
  values = values,
  column = c(treatment, dose),

  ## define order to display
  sorting_cols = c(ord_group, ord_row_lab),

  ## define column "big N" dressings,
  big_n = big_n_structure(
    param_val = "big_n",
    n_frmt = frmt("\n(N=XX)")
  ),

  ## table formatting for each row
  body_plan = body_plan(

    ## most basic formatting for anything that we don't style, auto round to 2
    ## decimals
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt("xx.xx")
    ),

    ## for all cases where row_label is "n" & param is "n", we want to use 2
    ## characters
    frmt_structure(
      group_val = ".default",
      label_val = "n",
      n = frmt("xx")
    ),

    ## these will combine the specified params into a single cell with the
    ## formatting defined for unique combinations of group_val, label_val, and
    ## for each column

    ### when row_label is "Median (Range)", paste together Median, min, max with
    ### respective styling
    frmt_structure(
      group_val = ".default",
      label_val = "Median (Range)",
      frmt_combine("{Median} ({Range_min};{Range_max})",
                   Median = frmt("xx.x"),
                   Range_min = frmt("xx"),
                   Range_max = frmt("xx"), missing = " ")
    ),

    ### when row_label is "Mean (SD), paste together Mean and SD with respective
    ### styling
    frmt_structure(
      group_val = ".default",
      label_val = "Mean (SD)",
      frmt_combine("{Mean} ({SD})",
                   Mean = frmt("xx.x"),
                   SD = frmt("xx.xx"), missing = " ")
    ),

    ### when row_label is "Diff of LS Means (SE)", paste together diff and SE
    ### with respective styling
    frmt_structure(
      group_val = ".default",
      label_val = "Diff of LS Means (SE)",
      frmt_combine("{diff} ({diff_se})",
                   diff = frmt("xx.x"),
                   diff_se = frmt("xx.xx"), missing = " ")
    ),

    ### when row_label is "95% CI", it will paste together the upper and lower
    ### confidence intervals and SE with respective styling
    frmt_structure(
      group_val = ".default",
      label_val = "95% CI",
      frmt_combine("({diff_lcl};{diff_ucl})",
                   diff_lcl = frmt("xx.x"),
                   diff_ucl = frmt("xx.x"), missing = " ")
    ),

    ### For the label values, apply conditional formatting to the cases where
    ### param is p.value based on the actual value
    frmt_structure(
      group_val = ".default",
      label_val = c("p-value (Dose Response)",
                    "p-value (Xanomeline High - Xanomeline Low)",
                    "p-value (Xanomeline - Placebo)"),
      p.value = frmt_when(
        "<0.001" ~ "<0.001",
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx", missing = " "))
    )
  ),


  ## Define order of columns
  col_plan = col_plan(
    row_label_group,
    row_label,
    Placebo,
    span_structure(
      treatment = `Xanomeline`,
      dose = c(`Low Dose`,`High Dose`)
    ),
    -starts_with("ord")
  ),

  footnote_plan = footnote_plan(
    footnote_structure(
      "Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate",
      group_val = list(row_label_group = c("p-value (Dose Response)","p-value (Xanomeline - Placebo)","p-value (Xanomeline High - Xanomeline Low)"))
    ),
    footnote_structure(
      "Test for a non-zero coefficient for treatment (dose) as a continuous variable.",
      group_val = list(row_label_group = c("p-value (Dose Response)"))
    ),
    footnote_structure(
      "Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.",
      group_val = list(row_label_group = c("p-value (Xanomeline - Placebo)","p-value (Xanomeline High - Xanomeline Low)"))
    )
  )

)


## Add Table 14-3.05 specific information ----------------------------------------

# This could be added by the programmer, pulled from some shared storage, etc.
# build table specific tfrmt from organizations predefined tfrmt

tfrmt_14_3_05 <- tfrmt_eff_final %>%
  tfrmt(
    ## Table Specific Decoration
    title = "Table 14-3.05",
    subtitle = "ADAS Cog (11) - Change from Baseline to Week 16 - LOCF",
  )


## Render table ---------------------------------------------------------------
### `print_to_gt()` generates a gt with some pre-defined styling, etc. It may be
### necessary to add styling to comply with organizational requirements or
### styling that is open for the programmers to define

gt_14_3_05 <-print_to_gt(
  tfrmt = tfrmt_14_3_05,
  .data = efficacy_ARD
  )

gt_14_3_05


