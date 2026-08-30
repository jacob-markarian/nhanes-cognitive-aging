# Clean and combine NHANES 2011-2014 data
# Author: Jacob Markarian

library(dplyr)
library(here)

phq_items <- sprintf("DPQ%03d", seq(10, 90, 10))

calculate_weekly_minutes <- function(participation, days, minutes) {
  case_when(
    participation == 2 ~ 0,
    participation == 1 &
      days %in% 1:7 &
      minutes >= 10 &
      minutes <= 1440 ~ days * minutes,
    TRUE ~ NA_real_
  )
}

load_cycle <- function(suffix, cycle_label) {
  
  demographics <- readRDS(
    here("data", "raw", paste0("DEMO_", suffix, ".rds"))
  )
  
  physical_activity <- readRDS(
    here("data", "raw", paste0("PAQ_", suffix, ".rds"))
  )
  
  depression <- readRDS(
    here("data", "raw", paste0("DPQ_", suffix, ".rds"))
  )
  
  cognition <- readRDS(
    here("data", "raw", paste0("CFQ_", suffix, ".rds"))
  )
  
  demographics |>
    filter(RIDAGEYR >= 60) |>
    left_join(physical_activity, by = "SEQN") |>
    left_join(depression, by = "SEQN") |>
    left_join(cognition, by = "SEQN") |>
    mutate(cycle = cycle_label)
}

combined_data <- bind_rows(
  load_cycle("G", "2011-2012"),
  load_cycle("H", "2013-2014")
)

analysis_data <- combined_data |>
  mutate(
    across(
      all_of(phq_items),
      ~ ifelse(.x %in% 0:3, .x, NA_real_)
    ),
    
    phq_items_complete =
      rowSums(is.na(across(all_of(phq_items)))) == 0,
    
    phq9_total = if_else(
      phq_items_complete,
      rowSums(across(all_of(phq_items))),
      NA_real_
    ),
    
    vigorous_weekly_minutes = calculate_weekly_minutes(
      PAQ650, PAQ655, PAD660
    ),
    
    moderate_weekly_minutes = calculate_weekly_minutes(
      PAQ665, PAQ670, PAD675
    ),
    
    recreation_met_minutes = if_else(
      !is.na(vigorous_weekly_minutes) &
        !is.na(moderate_weekly_minutes),
      vigorous_weekly_minutes * 8 +
        moderate_weekly_minutes * 4,
      NA_real_
    ),
    
    log_recreation_met = log1p(recreation_met_minutes),
    
    cerad_learning = if_else(
      !is.na(CFDCST1) &
        !is.na(CFDCST2) &
        !is.na(CFDCST3),
      CFDCST1 + CFDCST2 + CFDCST3,
      NA_real_
    ),
    
    cerad_delayed = CFDCSR,
    animal_fluency = CFDAST,
    digit_symbol = CFDDS,
    
    age = RIDAGEYR,
    
    female = case_when(
      RIAGENDR == 1 ~ 0,
      RIAGENDR == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    education = if_else(
      DMDEDUC2 %in% 1:5,
      DMDEDUC2,
      NA_real_
    ),
    
    race_ethnicity = if_else(
      RIDRETH3 %in% 1:7,
      RIDRETH3,
      NA_real_
    ),
    
    poverty_ratio = if_else(
      INDFMPIR >= 0 & INDFMPIR <= 5,
      INDFMPIR,
      NA_real_
    ),
    
    survey_weight = WTMEC2YR / 2,
    psu = SDMVPSU,
    stratum = SDMVSTRA
  ) |>
  select(
    SEQN,
    cycle,
    age,
    female,
    education,
    race_ethnicity,
    poverty_ratio,
    survey_weight,
    psu,
    stratum,
    vigorous_weekly_minutes,
    moderate_weekly_minutes,
    recreation_met_minutes,
    log_recreation_met,
    phq9_total,
    cerad_learning,
    cerad_delayed,
    animal_fluency,
    digit_symbol
  )

dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  analysis_data,
  here("data", "processed", "analysis_data.rds")
)

cat("Participants aged 60+:", nrow(analysis_data), "\n")

analysis_data |>
  summarise(
    across(
      c(
        log_recreation_met,
        phq9_total,
        cerad_learning,
        cerad_delayed,
        animal_fluency,
        digit_symbol
      ),
      ~ sum(!is.na(.x))
    )
  ) |>
  print()
