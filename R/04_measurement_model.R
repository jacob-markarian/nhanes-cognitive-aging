# Confirmatory factor analysis (CFA) of cognitive functioning
# Performing CFA to inspect fit of the model

# 1. Load packages ------------------------------------------------------

library(lavaan)
#lavaan is loaded for purposes of analysis 
library(dplyr)
library(here)

# 2. Load the cleaned dataset ------------------------------------------

analysis_data <- readRDS(
  here("data", "processed", "analysis_data.rds")
)

# 3. Specify the measurement model -------------------------------------

# The =~ I define cognition as a latent variable measured by
# the four observed cognitive tests. I am now creating the model for R to 
#rcecognize. 
cfa_model <- '
  cognition =~ cerad_learning +
               cerad_delayed +
               animal_fluency +
               digit_symbol
'

# 4. Estimate the model -------------------------------------------------

cfa_fit <- cfa(
  model = cfa_model,
  data = analysis_data,
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)

# 5. Extract model-fit statistics --------------------------------------

all_fit_measures <- fitMeasures(cfa_fit)

requested_fit_measures <- c(
  "chisq.scaled",
  "df.scaled",
  "pvalue.scaled",
  "cfi.robust",
  "tli.robust",
  "rmsea.robust",
  "srmr"
)

available_fit_measures <- requested_fit_measures[
  requested_fit_measures %in% names(all_fit_measures)
]

fit_indices <- data.frame(
  measure = available_fit_measures,
  value = round(
    all_fit_measures[available_fit_measures],
    digits = 3
  ),
  row.names = NULL
)

 #Fit Indices evaluation; SRMR is good at .059, generally this is supposed to 
 #less than 0.10. CFI is below .95 at .923, so technically poor fit, although 
 #not horrible. RMSEA is 0.242, which indicates poor fit. TLI is also poor
 #0.768. Based on this, we have poor overall fit in the model. However, the 
 #standardized loadings are strong (cerad_learning is 0.868, cerad_delayed is 
 #0.843, animal_fluency is 0.52, and Digit symbol is 0.602). 

# 6. Extract standardized factor loadings ------------------------------

standardized_results <- standardizedSolution(cfa_fit)

standardized_loadings <- standardized_results |>
  filter(op == "=~") |>
  select(
    latent_variable = lhs,
    indicator = rhs,
    standardized_loading = est.std,
    standard_error = se,
    z_value = z,
    p_value = pvalue
  ) |>
  mutate(
    across(
      where(is.numeric),
      function(x) round(x, digits = 3)
    )
  )

# 7. Save results -----------------------------------------

write.csv(
  fit_indices,
  here("results", "cfa_fit_indices.csv"),
  row.names = FALSE
)

write.csv(
  standardized_loadings,
  here("results", "cfa_standardized_loadings.csv"),
  row.names = FALSE
)

capture.output(
  summary(
    cfa_fit,
    fit.measures = TRUE,
    standardized = TRUE,
    rsquare = TRUE
  ),
  file = here("results", "cfa_full_summary.txt")
)

# 8. Display the key results -------------------------------------------

cat(
  "Observations used:",
  lavInspect(cfa_fit, "nobs"),
  "\n\n"
)

print(fit_indices)
print(standardized_loadings)

#because the fit is poor, but some potential exists, we will need to
#re-specificity 

#9. Model 2: General cognition with shared CERAD variance ------------

# Theory:
# CERAD learning and delayed recall both assess verbal episodic memory
# and use the same word-list procedure from what I can tell from my
#research online. They may therefore share a lot of 
# memory- and task-specific variance beyond general cognition.
model2_cerad <- '
  cognition =~ cerad_learning +
               cerad_delayed +
               animal_fluency +
               digit_symbol

  cerad_learning ~~ cerad_delayed
'

# Estimate Model 2 using the same settings as the original model
fit_model2_cerad <- cfa(
  model = model2_cerad,
  data = analysis_data,
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)

# Review overall fit and standardized estimates
print(
  summary(
    fit_model2_cerad,
    fit.measures = TRUE,
    standardized = TRUE
  )
)

# Examine the CERAD residual covariance
model2_standardized <- standardizedSolution(
  fit_model2_cerad
)

model2_cerad_covariance <- model2_standardized |>
  filter(
    op == "~~",
    lhs == "cerad_learning",
    rhs == "cerad_delayed"
  )

print(model2_cerad_covariance)

# Examine how much variance the cognition factor explains
model2_r_squared <- lavInspect(
  fit_model2_cerad,
  "rsquare"
)

print(model2_r_squared)

# Compare Model 2 with the original one-factor model
print(
  lavTestLRT(
    cfa_fit,
    fit_model2_cerad
  )
)

# Extract and save Model 2 fit statistics
model2_fit_values <- fitMeasures(
  fit_model2_cerad,
  c(
    "chisq.scaled",
    "df.scaled",
    "pvalue.scaled",
    "cfi.robust",
    "tli.robust",
    "rmsea.robust",
    "srmr"
  )
)

model2_fit_table <- data.frame(
  measure = names(model2_fit_values),
  value = round(
    as.numeric(model2_fit_values),
    digits = 3
  )
)

write.csv(
  model2_fit_table,
  here("results", "cfa_model2_cerad_fit.csv"),
  row.names = FALSE
)

capture.output(
  summary( 
    fit_model2_cerad,
    fit.measures = TRUE,
    standardized = TRUE,
    rsquare = TRUE
  ),
  file = here("results", "cfa_model2_cerad_summary.txt")
)

 #This model has very good fit indices, however, this was predictable as the 
 #original model had only a few degrees of freedom. Our fit indices are nearly 
 #perfect in some instances. We can stop here. 


