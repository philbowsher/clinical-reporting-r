## Create Efficacy Table (14-3.05) --------------------------------------------------------

library(tfrmt)
library(gt)

### Define tfrmt for general efficacy ------------------------------------------
### Generally organizations would have this pre-defined, and ARD's build to spec
### to match their predefined tfrmts

tfrmt_eff <- tfrmt(

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

  ## set up a structure for blocking based on the groups
  ## collapse the groups with label and indent
  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = list(row_label_group="Change from Baseline"),
      element_block(post_space = " ")
    ),
    row_grp_structure(
      group_val = list(row_label_group="p-value (Dose Response)"),
      element_block(post_space = " ")
    ),
    row_grp_structure(
      group_val = list(row_label_group="p-value (Xanomeline - Placebo)"),
      element_block(post_space = " ")
    ),
    label_loc = element_row_grp_loc(location = "indented")
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
  )

)


### Add T. 14-3.05 specific information ----------------------------------------
### This could be added by the programmer, pulled from some shared storage, etc.
### build table specific tfrmt from organizations predefined tfrmt

tfrmt_14_3_05 <- tfrmt_eff %>%
  tfrmt(
    ## Table Specific Decoration
    title = "Table 14-3.05",
    subtitle = "ADAS Cog (11) - Change from Baseline to Week 16 - LOCF\nTable Generated using CDISC Pilot Data via {safetyData} R Package",
  )

### Load Efficacy ARD ---------------------------------------------------------

efficacy_env <- new.env()
sys.source(here::here("ARD/make_efficacy_ARD.R"),envir = efficacy_env)
efficacy_ARD <- efficacy_env$efficacy_ARD

### Render table ---------------------------------------------------------------
### `print_to_gt()` generates a gt with some pre-defined styling, etc. It may be
### necessary to add styling to comply with organizational requirements or
### styling that is open for the programmers to define

gt_14_3_05 <-print_to_gt(
  tfrmt = tfrmt_14_3_05,
  .data = efficacy_env$efficacy_ARD
  ) %>%
  tab_style(
    style = cell_text(whitespace = "pre-wrap"),
    locations = cells_title()
  )

## adding footnotes
gt_14_3_05 %>%
  tab_footnote(
    "Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate",
    locations = cells_stub(rows = grepl("p-value",row_label)),
  ) %>%
  tab_footnote(
    "Test for a non-zero coefficient for treatment (dose) as a continuous variable.",
    locations = cells_stub(rows = grepl("Dose Response",row_label))
  )%>%
  tab_footnote(
    "Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.",
    locations = cells_stub(rows = grepl("Xanomeline",row_label))
  )



