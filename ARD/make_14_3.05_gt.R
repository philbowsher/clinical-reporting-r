library(safetyData)
library(tidyverse)
library(tfrmt)
library(gt)




## Construct ARD ---------------------------------------------------------------

adas_filtered <- adam_adqsadas %>%
  filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y')

### construct big N and total big N --------------------------------------------
adas_big_n <- adas_filtered %>%
  group_by(TRTP) %>%
  summarize(values = length(unique(USUBJID))) %>%
  mutate(param = "big_n") %>%
  separate(
    TRTP,
    c("treatment", "dose"),
    sep = "\\s",
    extra = "merge",
    fill = "left"
  )

adas_total_big_n <- adas_big_n %>%
  filter(!is.na(treatment)) %>%
  group_by(treatment) %>%
  summarize(values = sum(values), .groups = "drop") %>%
  mutate(param = "big_n")


### Calculate summary statistics of a given variable (x) at specific visits ----

calc_summary <- function(x, at, na.rm = TRUE){

  x_at <- x[at]

  if(any(is.na(x_at)) & na.rm == TRUE){
    x_at <- x_at[!is.na(x_at)]
  }

  list(
    tribble(
         ~param,       ~values, ~ row_label,
            "n", length(x_at),         "n",
         "Mean",   mean(x_at), "Mean (SD)",
           "SD",     sd(x_at), "Mean (SD)",
       "Median", median(x_at), "Median (Range)",
    "Range_min",    min(x_at), "Median (Range)",
    "Range_max",    max(x_at), "Median (Range)",
    )
  )

}

adas_summary_stats <- adas_filtered %>%
  group_by(TRTP) %>%
  summarise(
    Baseline = calc_summary(AVAL, VISIT == "BASELINE"),
    `Week 16` = calc_summary(AVAL, VISIT == "WEEK 16"),
    `Change from Baseline` = calc_summary(CHG, VISIT == "WEEK 16")
  ) %>%
  pivot_longer(
    cols = c(Baseline, `Week 16`, `Change from Baseline`),
    names_to = "row_label_group",
    values_to = "values"
  ) %>%
  unnest(cols = values) %>%
  separate(
    TRTP,
    c("treatment","dose"),
    sep = "\\s",
    extra = "merge",
    fill = "left"
  ) %>%
  select(treatment, dose, row_label_group, row_label, param, values) %>%
  mutate(
    ord_group = case_when(
      row_label_group == "Baseline" ~ 1,
      row_label_group == "Week 16" ~ 2,
      row_label_group == "Change from Baseline" ~ 3
    ),
    ord_row_lab = case_when(
      row_label == "n" ~ 1,
      row_label == "Mean (SD)" ~ 2,
      row_label == "Median (Range)" ~ 3
    )
  )


### calculate model results of dose and treatment differences ------------------

#### Code Credit: based on CDISC Pilot Replication by Atorus Research
#### https://github.com/atorus-research/CDISC_pilot_replication/blob/master/programs/t-14-3-05.R

calc_model_dose<- function(.data){

  model <- lm(CHG ~ TRTPN + SITEGR1 + BASE, data=.data)

  ## Dose Response
  model_ancova <- car::Anova(model, type=3)
  dose_response <- tibble(
    treatment = "Xanomeline",
    dose = "High Dose",
    row_label_group = "p-value (Dose Response)",
    row_label = "p-value (Dose Response)",
    param = "p.value",
    values = model_ancova["TRTPN", 'Pr(>F)']
  )
}

calc_model_diff<- function(.data){

  .data['TRTP'] <- factor(.data$TRTP, levels=c('Xanomeline High Dose', 'Xanomeline Low Dose', 'Placebo'))

  model <- lm(CHG ~ TRTP + SITEGR1 + BASE, data=.data)

  ## Treatment Differences

  withr::with_options(
    list(contrasts = c("contr.sum","contr.poly")),{
    lsm <- emmeans::lsmeans(model, ~TRTP, weights='proportional')
    cntrst_p <- emmeans::contrast(lsm, method="pairwise", adjust=NULL)
    cntrst_ci <- confint(cntrst_p)

    })

  constrast_response <- summary(cntrst_p) %>%
    left_join(cntrst_ci, by = c("contrast","estimate","SE","df")) %>%
    select(contrast, p.value, diff = estimate, diff_se = SE, diff_lcl = lower.CL, diff_ucl = upper.CL) %>%
    pivot_longer(
      cols = -contrast,
      names_to = "param",
      values_to = "values"
    ) %>%
    mutate(
      TRTP = trimws(str_extract(contrast, "(.*Dose)\\s(?=[-])")), ## extact first listed treatment
      row_label_group = case_when(
        str_detect(contrast,"Placebo$") ~ "p-value (Xanomeline - Placebo)",
        !str_detect(contrast,"Placebo$") ~ "p-value (Xanomeline High - Xanomeline Low)"
      ),
      row_label = case_when(
        param %in% c("p.value") ~ row_label_group,
        param %in% c("diff","diff_se") ~ "Diff of LS Means (SE)",
        param %in% c("diff_lcl","diff_ucl") ~ "95% CI"
      )
    ) %>%
    separate(
      TRTP,
      c("treatment","dose"),
      sep = "\\s",
      extra = "merge",
      fill = "left"
    ) %>%
    select(treatment, dose, row_label_group, row_label, param, values)

}

adas_model_dose_results <- adas_filtered %>%
  filter(AVISIT == "Week 16") %>%
  calc_model_dose() %>%
  mutate(
    ord_group = 4,
    ord_row_lab = 1
  )

adas_model_diff_results <- adas_filtered %>%
  filter(AVISIT == "Week 16") %>%
  calc_model_diff() %>%
  mutate(
    ord_group = case_when(
      row_label_group == "p-value (Xanomeline - Placebo)" ~ 5,
      row_label_group == "p-value (Xanomeline High - Xanomeline Low)" ~ 6
    ),
    ord_row_lab = case_when(
      str_detect(row_label, "p-value") ~ 1,
      row_label == "Diff of LS Means (SE)" ~ 2,
      row_label == "95% CI" ~ 3
    )
  )



### Combine ARD Components -----------------------------------------------------

table_14_3_05_ARD <- bind_rows(
  adas_big_n,
  adas_total_big_n,
  adas_summary_stats,
  adas_model_dose_results,
  adas_model_diff_results
)

## Create Table 14-3.05 --------------------------------------------------------


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


### Add T. 14-3.05 specific information -----------------------------------------
### This could be added by the programmer, pulled from some shared storage, etc.
### build table specific tfrmt from organizations predefined tfrmt

tfrmt_14_3_05 <- tfrmt_eff %>%
  tfrmt(
    ## Table Specific Decoration
    title = "Table 14-3.05",
    subtitle = "ADAS Cog (11) - Change from Baseline to Week 16 - LOCF\nTable Generated using CDISC Pilot Data via {safetyData} R Package",
  )


### Render table
### ----------------------------------------------------------------
### `print_to_gt()` generates a gt with some pre-defined styling, etc. It may be
### necessary to add styling to comply with organizational requirements or
### styling that is open for the programmers to define

gt_14_3_05 <-print_to_gt(
    tfrmt_14_3_05,
    table_14_3_05_ARD
  ) %>%
  tab_style(
    style = cell_text(whitespace = "pre-wrap"),
    locations = list(cells_title(),gt::cells_footnotes(), gt::cells_source_notes())
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


