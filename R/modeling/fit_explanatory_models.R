# Fit Explanatory Late-Delivery Models
#
# Fits two complementary explanatory models using the chronological training
# period only:
#
#   1. Linear probability model with HC3 robust standard errors
#   2. Logistic regression with robust odds ratios and marginal effects
#
# The specification is intentionally concise. It represents current-order
# pressure, seasonality, supplier-material history, supplier-wide history,
# workload, and limited-history status without reproducing the full
# predictive model.
#
# Results describe conditional associations, not causal effects.
# Validation and test datasets are not loaded or evaluated.

library(dplyr)
library(readr)
library(broom)
library(sandwich)
library(lmtest)
library(marginaleffects)

set.seed(424)

# -------------------------------------------------------------------------
# Input file
# -------------------------------------------------------------------------

training_file <- file.path(
  "data",
  "processed",
  "training_data.csv"
)

if (!file.exists(training_file)) {
  stop(
    "training_data.csv was not found. ",
    "Run R/modeling/create_order_level_model_splits.R first."
  )
}

# -------------------------------------------------------------------------
# Load chronological training data
# -------------------------------------------------------------------------

training_data <- read_csv(
  training_file,
  show_col_types = FALSE
) %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    order_date = as.Date(order_date),
    promised_delivery_date = as.Date(promised_delivery_date),
    late_delivery_target = as.integer(late_delivery_target)
  )

stopifnot(
  nrow(training_data) > 0L,
  all(training_data$late_delivery_target %in% c(0L, 1L)),
  all(training_data$scoring_date == training_data$order_date)
)

# -------------------------------------------------------------------------
# Define the concise explanatory specification
# -------------------------------------------------------------------------
#
# One or two representative measures are retained for each business concept.
# Supplier-level history is included because it pools prior delivery evidence
# across materials and provides an observable proxy for persistent supplier
# reliability.

explanatory_predictors <- c(
  # Current-order pressure
  "planned_lead_time_days",
  "lead_time_pressure_days",
  "order_size_ratio",

  # Promised-delivery seasonality
  "promised_delivery_month_sin",
  "promised_delivery_month_cos",

  # Supplier-material history
  "completed_order_count_180d",
  "late_quantity_rate_180d",

  # Supplier-wide history
  "supplier_completed_order_count_180d",
  "supplier_late_quantity_rate_180d",
  "supplier_average_late_days_180d",

  # Supplier workload visible at scoring
  "workload_pressure_90d",

  # Limited supplier-material history indicator
  #
  # Supplier-wide history is available for every record in the chronological
  # training period, so its indicator has no variation and is not estimable.
  "has_180d_relationship_history"
)

history_indicator_fields <- c(
  "has_180d_relationship_history"
)

missing_predictors <- setdiff(
  explanatory_predictors,
  names(training_data)
)

if (length(missing_predictors) > 0L) {
  stop(
    "Required explanatory predictors are missing: ",
    paste(missing_predictors, collapse = ", ")
  )
}

# -------------------------------------------------------------------------
# Validate categorical predictor variation
# -------------------------------------------------------------------------
#
# A factor must contain at least two observed levels in the training period.
# Supplier-wide history is deliberately not included as an indicator because
# every training record has 180-day supplier history after the warm-up year.

history_indicator_level_counts <- vapply(
  training_data[history_indicator_fields],
  function(values) {
    dplyr::n_distinct(values[!is.na(values)])
  },
  integer(1)
)

if (any(history_indicator_level_counts < 2L)) {
  stop(
    "An explanatory history indicator has fewer than two observed training levels: ",
    paste(
      names(history_indicator_level_counts)[
        history_indicator_level_counts < 2L
      ],
      collapse = ", "
    )
  )
}

# -------------------------------------------------------------------------
# Prepare explanatory modeling data
# -------------------------------------------------------------------------

explanatory_data <- training_data %>%
  select(
    late_delivery_target,
    all_of(explanatory_predictors)
  ) %>%
  mutate(
    across(
      all_of(history_indicator_fields),
      ~ factor(
        .x,
        levels = c(FALSE, TRUE),
        labels = c("no_history", "has_history")
      )
    )
  )

# -------------------------------------------------------------------------
# Training-only missing-value treatment
# -------------------------------------------------------------------------
#
# Missing historical values indicate that no qualifying history was
# available. The history indicators preserve that state. Numeric values are
# then imputed using medians calculated from the training period only.

numeric_predictors <- explanatory_data %>%
  select(
    -late_delivery_target,
    -all_of(history_indicator_fields)
  ) %>%
  select(where(is.numeric)) %>%
  names()

training_medians <- vapply(
  explanatory_data[numeric_predictors],
  function(values) {
    median(values, na.rm = TRUE)
  },
  numeric(1)
)

if (anyNA(training_medians)) {
  stop(
    "At least one explanatory predictor has no nonmissing training values."
  )
}

for (field_name in numeric_predictors) {
  explanatory_data[[field_name]][
    is.na(explanatory_data[[field_name]])
  ] <- training_medians[[field_name]]
}

stopifnot(
  !anyNA(explanatory_data),
  all(
    vapply(
      explanatory_data[history_indicator_fields],
      is.factor,
      logical(1)
    )
  )
)

# -------------------------------------------------------------------------
# Model formula
# -------------------------------------------------------------------------

explanatory_formula <- late_delivery_target ~
  planned_lead_time_days +
  lead_time_pressure_days +
  order_size_ratio +
  promised_delivery_month_sin +
  promised_delivery_month_cos +
  completed_order_count_180d +
  late_quantity_rate_180d +
  supplier_completed_order_count_180d +
  supplier_late_quantity_rate_180d +
  supplier_average_late_days_180d +
  workload_pressure_90d +
  has_180d_relationship_history

# -------------------------------------------------------------------------
# Linear probability model
# -------------------------------------------------------------------------

linear_probability_fit <- lm(
  formula = explanatory_formula,
  data = explanatory_data
)

linear_probability_vcov <- sandwich::vcovHC(
  linear_probability_fit,
  type = "HC3"
)

linear_probability_test <- lmtest::coeftest(
  linear_probability_fit,
  vcov. = linear_probability_vcov
)

linear_probability_results <- broom::tidy(
  linear_probability_test,
  conf.int = TRUE
) %>%
  mutate(
    estimate_percentage_points = estimate * 100,
    conf_low_percentage_points = conf.low * 100,
    conf_high_percentage_points = conf.high * 100,
    model = "Linear probability model"
  ) %>%
  select(
    model,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high,
    estimate_percentage_points,
    conf_low_percentage_points,
    conf_high_percentage_points
  )

linear_probability_predictions <- predict(
  linear_probability_fit,
  newdata = explanatory_data
)

linear_probability_prediction_summary <- tibble(
  minimum_prediction = min(linear_probability_predictions),
  first_quartile_prediction = as.numeric(
    quantile(linear_probability_predictions, probs = 0.25)
  ),
  median_prediction = median(linear_probability_predictions),
  mean_prediction = mean(linear_probability_predictions),
  third_quartile_prediction = as.numeric(
    quantile(linear_probability_predictions, probs = 0.75)
  ),
  maximum_prediction = max(linear_probability_predictions),
  predictions_below_zero = sum(linear_probability_predictions < 0),
  predictions_above_one = sum(linear_probability_predictions > 1)
)

# -------------------------------------------------------------------------
# Logistic regression model
# -------------------------------------------------------------------------

explanatory_logit_fit <- glm(
  formula = explanatory_formula,
  data = explanatory_data,
  family = binomial(link = "logit")
)

logit_vcov <- sandwich::vcovHC(
  explanatory_logit_fit,
  type = "HC3"
)

logit_robust_test <- lmtest::coeftest(
  explanatory_logit_fit,
  vcov. = logit_vcov
)

logit_coefficient_results <- broom::tidy(
  logit_robust_test,
  conf.int = TRUE
) %>%
  mutate(
    model = "Logistic regression",
    odds_ratio = exp(estimate),
    odds_ratio_conf_low = exp(conf.low),
    odds_ratio_conf_high = exp(conf.high)
  ) %>%
  select(
    model,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high,
    odds_ratio,
    odds_ratio_conf_low,
    odds_ratio_conf_high
  )

# -------------------------------------------------------------------------
# Average marginal effects
# -------------------------------------------------------------------------

logit_average_marginal_effects <- marginaleffects::avg_slopes(
  explanatory_logit_fit,
  vcov = logit_vcov,
  variables = explanatory_predictors
) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(
    estimate_percentage_points = estimate * 100,
    conf_low_percentage_points = conf.low * 100,
    conf_high_percentage_points = conf.high * 100,
    model = "Logistic regression average marginal effect"
  ) %>%
  select(
    model,
    term,
    contrast,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high,
    estimate_percentage_points,
    conf_low_percentage_points,
    conf_high_percentage_points
  )

# -------------------------------------------------------------------------
# Selected probability contrasts
# -------------------------------------------------------------------------
#
# These contrasts change one predictor while retaining the observed values of
# all other predictors. They are descriptive model-based comparisons, not
# estimates of causal effects.

calculate_probability_contrast <- function(
  fitted_model,
  source_data,
  variable_name,
  comparison_value,
  reference_value,
  contrast_label
) {
  comparison_data <- source_data
  reference_data <- source_data

  comparison_data[[variable_name]] <- comparison_value
  reference_data[[variable_name]] <- reference_value

  comparison_probability <- predict(
    fitted_model,
    newdata = comparison_data,
    type = "response"
  )

  reference_probability <- predict(
    fitted_model,
    newdata = reference_data,
    type = "response"
  )

  probability_difference <-
    comparison_probability - reference_probability

  tibble(
    contrast = contrast_label,
    variable = variable_name,
    reference_value = reference_value,
    comparison_value = comparison_value,
    reference_average_probability = mean(reference_probability),
    comparison_average_probability = mean(comparison_probability),
    average_probability_difference = mean(probability_difference),
    average_percentage_point_difference =
      mean(probability_difference) * 100
  )
}

supplier_late_rate_quartiles <- quantile(
  explanatory_data$supplier_late_quantity_rate_180d,
  probs = c(0.25, 0.75),
  na.rm = TRUE
)

workload_pressure_quartiles <- quantile(
  explanatory_data$workload_pressure_90d,
  probs = c(0.25, 0.75),
  na.rm = TRUE
)

probability_contrasts <- bind_rows(
  calculate_probability_contrast(
    fitted_model = explanatory_logit_fit,
    source_data = explanatory_data,
    variable_name = "order_size_ratio",
    comparison_value = 1.50,
    reference_value = 1.00,
    contrast_label =
      "Order size 50% above typical versus typical order size"
  ),
  calculate_probability_contrast(
    fitted_model = explanatory_logit_fit,
    source_data = explanatory_data,
    variable_name = "lead_time_pressure_days",
    comparison_value = 10,
    reference_value = 0,
    contrast_label =
      "Ten days of lead-time compression versus no compression"
  ),
  calculate_probability_contrast(
    fitted_model = explanatory_logit_fit,
    source_data = explanatory_data,
    variable_name = "late_quantity_rate_180d",
    comparison_value = 0.30,
    reference_value = 0.10,
    contrast_label =
      "Thirty percent versus ten percent relationship late quantity"
  ),
  calculate_probability_contrast(
    fitted_model = explanatory_logit_fit,
    source_data = explanatory_data,
    variable_name = "supplier_late_quantity_rate_180d",
    comparison_value = as.numeric(supplier_late_rate_quartiles[[2]]),
    reference_value = as.numeric(supplier_late_rate_quartiles[[1]]),
    contrast_label =
      "High versus low supplier-wide historical late quantity"
  ),
  calculate_probability_contrast(
    fitted_model = explanatory_logit_fit,
    source_data = explanatory_data,
    variable_name = "workload_pressure_90d",
    comparison_value = as.numeric(workload_pressure_quartiles[[2]]),
    reference_value = as.numeric(workload_pressure_quartiles[[1]]),
    contrast_label =
      "High versus low recent supplier workload pressure"
  )
)

# -------------------------------------------------------------------------
# Model diagnostics
# -------------------------------------------------------------------------

logit_fitted_probabilities <- predict(
  explanatory_logit_fit,
  newdata = explanatory_data,
  type = "response"
)

model_diagnostics <- tibble(
  model = c(
    "Linear probability model",
    "Logistic regression"
  ),
  observations = c(
    nobs(linear_probability_fit),
    nobs(explanatory_logit_fit)
  ),
  outcome_rate = c(
    mean(explanatory_data$late_delivery_target),
    mean(explanatory_data$late_delivery_target)
  ),
  minimum_fitted_value = c(
    min(linear_probability_predictions),
    min(logit_fitted_probabilities)
  ),
  maximum_fitted_value = c(
    max(linear_probability_predictions),
    max(logit_fitted_probabilities)
  ),
  fitted_values_below_zero = c(
    sum(linear_probability_predictions < 0),
    sum(logit_fitted_probabilities < 0)
  ),
  fitted_values_above_one = c(
    sum(linear_probability_predictions > 1),
    sum(logit_fitted_probabilities > 1)
  )
)

# -------------------------------------------------------------------------
# Print explanatory results
# -------------------------------------------------------------------------

cat("\nEXPLANATORY LATE-DELIVERY MODELS\n")
cat(
  "\nTraining observations:",
  format(nrow(explanatory_data), big.mark = ","),
  "\n"
)
cat(
  "Training late-delivery rate:",
  scales::percent(
    mean(explanatory_data$late_delivery_target),
    accuracy = 0.1
  ),
  "\n"
)
cat(
  "Explanatory predictors:",
  length(explanatory_predictors),
  "\n"
)

cat("\nLINEAR PROBABILITY MODEL\n")
cat(
  "Coefficients are shown in probability units and percentage points.\n"
)
print(linear_probability_results, n = Inf, width = Inf)

cat("\nLINEAR PROBABILITY PREDICTION SUMMARY\n")
print(linear_probability_prediction_summary, width = Inf)

cat("\nLOGISTIC REGRESSION ODDS RATIOS\n")
print(logit_coefficient_results, n = Inf, width = Inf)

cat("\nLOGISTIC REGRESSION AVERAGE MARGINAL EFFECTS\n")
print(logit_average_marginal_effects, n = Inf, width = Inf)

cat("\nSELECTED LOGISTIC PROBABILITY CONTRASTS\n")
print(probability_contrasts, n = Inf, width = Inf)

cat("\nMODEL DIAGNOSTICS\n")
print(model_diagnostics, n = Inf, width = Inf)

# -------------------------------------------------------------------------
# Save explanatory outputs
# -------------------------------------------------------------------------

dir.create(
  "models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  list(
    linear_probability_model = linear_probability_fit,
    logistic_model = explanatory_logit_fit,
    linear_probability_vcov = linear_probability_vcov,
    logistic_vcov = logit_vcov,
    training_medians = training_medians,
    explanatory_predictors = explanatory_predictors
  ),
  file = file.path(
    "models",
    "explanatory_models.rds"
  ),
  compress = "xz"
)

write_csv(
  linear_probability_results,
  file.path(
    "data",
    "processed",
    "linear_probability_model_results.csv"
  ),
  na = ""
)

write_csv(
  logit_coefficient_results,
  file.path(
    "data",
    "processed",
    "explanatory_logit_odds_ratios.csv"
  ),
  na = ""
)

write_csv(
  logit_average_marginal_effects,
  file.path(
    "data",
    "processed",
    "explanatory_logit_average_marginal_effects.csv"
  ),
  na = ""
)

write_csv(
  probability_contrasts,
  file.path(
    "data",
    "processed",
    "explanatory_logit_probability_contrasts.csv"
  ),
  na = ""
)

write_csv(
  model_diagnostics,
  file.path(
    "data",
    "processed",
    "explanatory_model_diagnostics.csv"
  ),
  na = ""
)

message(
  "Explanatory late-delivery models completed successfully."
)
message(
  "Supplier-wide history, lead-time pressure, and workload pressure were included."
)
message(
  "The constant supplier-history indicator was excluded from estimation."
)
message(
  "Results describe conditional associations, not causal effects."
)
message(
  "The validation and final test datasets were not loaded or evaluated."
)
