library(safetyData)
library(tidyverse)
library(car)
library(withr)
library(emmeans)

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

efficacy_ARD <- bind_rows(
  adas_big_n,
  adas_total_big_n,
  adas_summary_stats,
  adas_model_dose_results,
  adas_model_diff_results
)

## cleanup
rm(
  adas_big_n,
  adas_total_big_n,
  adas_summary_stats,
  adas_model_dose_results,
  adas_model_diff_results,
  adas_filtered,
  calc_summary,
  calc_model_dose,
  calc_model_diff
)
