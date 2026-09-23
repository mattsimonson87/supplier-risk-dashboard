# Final One-Time Logistic Test Evaluation
#
# Refit the locked logistic regression workflow on the combined chronological
# training and validation periods, then evaluate it once on the final test
# period.
#
# Locked design carried into this script without further tuning:
#
#   - Predictive model: unpenalized logistic regression
#   - Predictors: the locked 40-predictor model dataset
#   - Preprocessing: missingness indicators, training medians, nominal-level
#     handling, dummy encoding, zero-variance removal, and normalization
#   - Review policy: highest-risk 20% of test orders
#   - Operational-impact rubric: fixed 0-to-100 point rubric
#   - Impact tiers: Low <40, Moderate 40-<60, High 60-<80, Very High 80-100
#   - Watchlist-priority logic: the locked validation-stage policy
#
# This script intentionally accesses the final test outcomes. After it is run,
# test results must be documented without changing the model, preprocessing,
# capacity rule, impact rubric, or watchlist policy in response to performance.

library(tidymodels)
library(dplyr)
library(readr)

set.seed(426)

script_start_time <- Sys.time()

# -------------------------------------------------------------------------
# Locked policy parameters
# -------------------------------------------------------------------------

locked_review_capacity <- 0.20

locked_history_indicator_fields <- c(
  "has_90d_relationship_history",
  "has_180d_relationship_history",
  "has_365d_relationship_history",
  "has_180d_supplier_history"
)

locked_policy <- tibble(
  policy_name = c(
    "Selected predictive model",
    "Development refit population",
    "Final evaluation population",
    "Review-capacity rule",
    "Operational-impact score range",
    "Impact-tier boundaries",
    "Post-test tuning policy"
  ),
  policy_value = c(
    "Unpenalized logistic regression",
    "Chronological training plus validation",
    "Chronological final test period",
    "Top 20% by predicted late-delivery probability",
    "0 to 100 points",
    "Low <40; Moderate 40-<60; High 60-<80; Very High 80-100",
    "Document results without further tuning"
  )
)

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

training_model_file <- file.path(
  "data", "processed", "training_model_data.csv"
)

validation_model_file <- file.path(
  "data", "processed", "validation_model_data.csv"
)

test_model_file <- file.path(
  "data", "processed", "test_model_data.csv"
)

test_business_file <- file.path(
  "data", "processed", "test_data.csv"
)

impact_parameters_file <- file.path(
  "data", "processed", "operational_impact_scoring_parameters.csv"
)

required_files <- c(
  training_model_file,
  validation_model_file,
  test_model_file,
  test_business_file,
  impact_parameters_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required final-evaluation files are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

# -------------------------------------------------------------------------
# Load model datasets
# -------------------------------------------------------------------------

training_model_data <- read_csv(
  training_model_file,
  show_col_types = FALSE
)

validation_model_data <- read_csv(
  validation_model_file,
  show_col_types = FALSE
)

test_model_data <- read_csv(
  test_model_file,
  show_col_types = FALSE
)

restore_model_field_types <- function(data) {
  missing_indicators <- setdiff(
    locked_history_indicator_fields,
    names(data)
  )

  if (length(missing_indicators) > 0L) {
    stop(
      "Required history indicators are missing: ",
      paste(missing_indicators, collapse = ", ")
    )
  }

  data %>%
    mutate(
      late_delivery_target = factor(
        late_delivery_target,
        levels = c("on_time", "late")
      ),
      across(
        all_of(locked_history_indicator_fields),
        ~ factor(
          .x,
          levels = c("no_history", "has_history")
        )
      )
    )
}

training_model_data <- restore_model_field_types(
  training_model_data
)

validation_model_data <- restore_model_field_types(
  validation_model_data
)

test_model_data <- restore_model_field_types(
  test_model_data
)

# -------------------------------------------------------------------------
# Validate model datasets and create development refit data
# -------------------------------------------------------------------------

stopifnot(
  nrow(training_model_data) > 0L,
  nrow(validation_model_data) > 0L,
  nrow(test_model_data) > 0L,
  identical(
    names(training_model_data),
    names(validation_model_data)
  ),
  identical(
    names(training_model_data),
    names(test_model_data)
  ),
  identical(
    levels(training_model_data$late_delivery_target),
    c("on_time", "late")
  ),
  identical(
    levels(validation_model_data$late_delivery_target),
    c("on_time", "late")
  ),
  identical(
    levels(test_model_data$late_delivery_target),
    c("on_time", "late")
  ),
  !anyNA(training_model_data$late_delivery_target),
  !anyNA(validation_model_data$late_delivery_target),
  !anyNA(test_model_data$late_delivery_target)
)

development_model_data <- bind_rows(
  training_model_data,
  validation_model_data
)

stopifnot(
  nrow(development_model_data) ==
    nrow(training_model_data) + nrow(validation_model_data)
)

# -------------------------------------------------------------------------
# Define the locked logistic workflow
# -------------------------------------------------------------------------

final_logistic_recipe <- recipe(
  late_delivery_target ~ .,
  data = development_model_data
) %>%
  step_indicate_na(
    all_numeric_predictors()
  ) %>%
  step_impute_median(
    all_numeric_predictors()
  ) %>%
  step_unknown(
    all_nominal_predictors(),
    new_level = "unknown"
  ) %>%
  step_novel(
    all_nominal_predictors(),
    new_level = "new"
  ) %>%
  step_dummy(
    all_nominal_predictors(),
    one_hot = FALSE
  ) %>%
  step_zv(
    all_predictors()
  ) %>%
  step_normalize(
    all_numeric_predictors()
  )

final_logistic_model <- logistic_reg(
  mode = "classification"
) %>%
  set_engine("glm")

final_evaluation_workflow <- workflow() %>%
  add_recipe(final_logistic_recipe) %>%
  add_model(final_logistic_model)

# -------------------------------------------------------------------------
# Refit on training plus validation
# -------------------------------------------------------------------------

final_evaluation_fit <- fit(
  final_evaluation_workflow,
  data = development_model_data
)

# -------------------------------------------------------------------------
# Score the final test period once
# -------------------------------------------------------------------------

test_probabilities <- predict(
  final_evaluation_fit,
  new_data = test_model_data,
  type = "prob"
)

test_predictions <- bind_cols(
  test_model_data %>%
    select(late_delivery_target),
  test_probabilities
) %>%
  mutate(
    test_row_id = row_number(),
    actual_late = as.integer(late_delivery_target == "late")
  )

stopifnot(
  nrow(test_predictions) == nrow(test_model_data),
  !anyNA(test_predictions$.pred_late),
  all(
    test_predictions$.pred_late >= 0 &
      test_predictions$.pred_late <= 1
  ),
  all(
    test_predictions$.pred_on_time >= 0 &
      test_predictions$.pred_on_time <= 1
  ),
  all(
    abs(
      test_predictions$.pred_late +
        test_predictions$.pred_on_time -
        1
    ) < 0.000001
  )
)

# -------------------------------------------------------------------------
# Calculate final test probability metrics
# -------------------------------------------------------------------------

final_test_metric_set <- metric_set(
  roc_auc,
  pr_auc,
  mn_log_loss,
  brier_class
)

final_test_probability_metrics <- final_test_metric_set(
  test_predictions,
  truth = late_delivery_target,
  .pred_late,
  event_level = "second"
)

# -------------------------------------------------------------------------
# Apply the locked top-20% review-capacity policy
# -------------------------------------------------------------------------

ranked_test_predictions <- test_predictions %>%
  arrange(
    desc(.pred_late),
    test_row_id
  ) %>%
  mutate(
    late_risk_rank = row_number(),
    actual_late = as.integer(late_delivery_target == "late")
  )

test_records <- nrow(ranked_test_predictions)
test_late_orders <- sum(ranked_test_predictions$actual_late)
test_late_rate <- mean(ranked_test_predictions$actual_late)
orders_to_review <- ceiling(
  test_records * locked_review_capacity
)

ranked_test_predictions <- ranked_test_predictions %>%
  mutate(
    risk_review_flag = late_risk_rank <= orders_to_review,
    risk_review_status = if_else(
      risk_review_flag,
      "Top 20% risk review",
      "Outside top 20% risk review"
    ),
    risk_tier = factor(
      case_when(
        late_risk_rank <= ceiling(n() * 0.05) ~ "Very High",
        late_risk_rank <= ceiling(n() * 0.10) ~ "High",
        late_risk_rank <= ceiling(n() * 0.20) ~ "Elevated",
        TRUE ~ "Routine"
      ),
      levels = c("Routine", "Elevated", "High", "Very High"),
      ordered = TRUE
    )
  )

active_test_risk_review <- ranked_test_predictions %>%
  filter(risk_review_flag)

late_orders_found <- sum(active_test_risk_review$actual_late)
test_review_precision <- mean(active_test_risk_review$actual_late)
test_review_recall <- late_orders_found / test_late_orders
test_review_lift <- test_review_precision / test_late_rate
test_probability_boundary <- min(active_test_risk_review$.pred_late)

test_review_capacity_summary <- tibble(
  locked_review_rate = locked_review_capacity,
  test_orders = test_records,
  orders_reviewed = orders_to_review,
  actual_review_rate = orders_to_review / test_records,
  total_late_orders = test_late_orders,
  late_orders_found = late_orders_found,
  precision = test_review_precision,
  recall = test_review_recall,
  baseline_late_rate = test_late_rate,
  lift_over_baseline = test_review_lift,
  minimum_predicted_probability_reviewed =
    test_probability_boundary
)

# -------------------------------------------------------------------------
# Create final test risk-decile summary
# -------------------------------------------------------------------------

test_risk_decile_summary <- ranked_test_predictions %>%
  mutate(
    risk_decile = ntile(late_risk_rank, 10)
  ) %>%
  group_by(risk_decile) %>%
  summarize(
    orders = n(),
    late_orders = sum(actual_late),
    actual_late_rate = mean(actual_late),
    average_predicted_probability = mean(.pred_late),
    minimum_predicted_probability = min(.pred_late),
    maximum_predicted_probability = max(.pred_late),
    .groups = "drop"
  ) %>%
  mutate(
    baseline_late_rate = test_late_rate,
    lift_over_baseline = actual_late_rate / baseline_late_rate,
    share_of_all_late_orders = late_orders / test_late_orders
  )

# -------------------------------------------------------------------------
# Load business fields and verify row alignment
# -------------------------------------------------------------------------

test_business_data <- read_csv(
  test_business_file,
  show_col_types = FALSE
) %>%
  mutate(
    test_row_id = row_number(),
    scoring_date = as.Date(scoring_date),
    order_date = as.Date(order_date),
    promised_delivery_date = as.Date(promised_delivery_date),
    actual_delivery_date = as.Date(actual_delivery_date),
    late_delivery_target = as.integer(late_delivery_target)
  )

stopifnot(
  nrow(test_business_data) == nrow(test_predictions),
  !anyDuplicated(test_business_data$scoring_record_id),
  all(
    test_business_data$late_delivery_target ==
      test_predictions$actual_late
  )
)

# -------------------------------------------------------------------------
# Load the locked training-derived impact parameters
# -------------------------------------------------------------------------

impact_parameters <- read_csv(
  impact_parameters_file,
  show_col_types = FALSE
)

get_impact_parameter <- function(parameter_name) {
  parameter_value <- impact_parameters %>%
    filter(parameter == parameter_name) %>%
    pull(value)

  if (length(parameter_value) != 1L || !is.finite(parameter_value)) {
    stop(
      "Locked impact parameter is missing or invalid: ",
      parameter_name
    )
  }

  as.numeric(parameter_value)
}

order_value_q25 <- get_impact_parameter(
  "order_value_training_q25"
)
order_value_q50 <- get_impact_parameter(
  "order_value_training_q50"
)
order_value_q75 <- get_impact_parameter(
  "order_value_training_q75"
)

# -------------------------------------------------------------------------
# Apply the locked operational-impact rubric to test orders
# -------------------------------------------------------------------------

normalize_material_criticality <- function(values) {
  normalized_values <- tolower(trimws(as.character(values)))

  case_when(
    normalized_values %in% c(
      "low", "noncritical", "non-critical"
    ) ~ "Low",
    normalized_values %in% c(
      "medium", "moderate"
    ) ~ "Medium",
    normalized_values %in% c(
      "high", "critical", "very high", "very_high"
    ) ~ "High",
    TRUE ~ NA_character_
  )
}

test_operational_impact <- test_business_data %>%
  mutate(
    material_criticality_normalized =
      normalize_material_criticality(material_criticality),

    material_criticality_points = case_when(
      material_criticality_normalized == "Low" ~ 0,
      material_criticality_normalized == "Medium" ~ 17.5,
      material_criticality_normalized == "High" ~ 35,
      TRUE ~ NA_real_
    ),

    approved_supplier_points = case_when(
      approved_supplier_count <= 1 ~ 25,
      approved_supplier_count == 2 ~ 15,
      approved_supplier_count == 3 ~ 7.5,
      approved_supplier_count >= 4 ~ 0,
      TRUE ~ NA_real_
    ),

    sourcing_allocation_points =
      pmin(pmax(sourcing_allocation, 0), 1) * 20,

    order_value_band = case_when(
      order_value <= order_value_q25 ~ "At or below training Q1",
      order_value <= order_value_q50 ~
        "Above Q1 through training median",
      order_value <= order_value_q75 ~
        "Above median through training Q3",
      TRUE ~ "Above training Q3"
    ),

    order_value_points = case_when(
      order_value <= order_value_q25 ~ 0,
      order_value <= order_value_q50 ~ 7,
      order_value <= order_value_q75 ~ 14,
      TRUE ~ 20
    ),

    operational_impact_score = round(
      material_criticality_points +
        approved_supplier_points +
        sourcing_allocation_points +
        order_value_points,
      digits = 1
    ),

    operational_impact_tier = factor(
      case_when(
        operational_impact_score >= 80 ~ "Very High",
        operational_impact_score >= 60 ~ "High",
        operational_impact_score >= 40 ~ "Moderate",
        TRUE ~ "Low"
      ),
      levels = c("Low", "Moderate", "High", "Very High"),
      ordered = TRUE
    )
  )

if (anyNA(test_operational_impact$material_criticality_normalized)) {
  stop(
    "The final test period contains an unrecognized material criticality."
  )
}

stopifnot(
  !anyNA(test_operational_impact$operational_impact_score),
  !anyNA(test_operational_impact$operational_impact_tier),
  all(
    test_operational_impact$operational_impact_score >= 0 &
      test_operational_impact$operational_impact_score <= 100
  )
)

# -------------------------------------------------------------------------
# Build the locked final test watchlist
# -------------------------------------------------------------------------

test_risk_output <- ranked_test_predictions %>%
  select(
    test_row_id,
    predicted_late_probability = .pred_late,
    predicted_on_time_probability = .pred_on_time,
    late_risk_rank,
    risk_review_flag,
    risk_review_status,
    risk_tier
  )

final_test_portfolio <- test_operational_impact %>%
  left_join(
    test_risk_output,
    by = "test_row_id"
  ) %>%
  mutate(
    intervention_priority = factor(
      case_when(
        risk_review_flag &
          operational_impact_tier == "Very High" ~
          "1 - Immediate Mitigation",

        risk_review_flag &
          operational_impact_tier == "High" ~
          "2 - Priority Review",

        risk_review_flag &
          operational_impact_tier %in% c("Moderate", "Low") ~
          "3 - Standard Risk Review",

        !risk_review_flag &
          operational_impact_tier == "Very High" ~
          "4 - Impact Monitoring",

        TRUE ~
          "5 - Routine Monitoring"
      ),
      levels = c(
        "1 - Immediate Mitigation",
        "2 - Priority Review",
        "3 - Standard Risk Review",
        "4 - Impact Monitoring",
        "5 - Routine Monitoring"
      ),
      ordered = TRUE
    ),

    recommended_action = case_when(
      intervention_priority == "1 - Immediate Mitigation" ~
        paste(
          "Contact supplier, confirm recovery plan,",
          "and assess alternate sourcing or inventory protection."
        ),

      intervention_priority == "2 - Priority Review" ~
        paste(
          "Review supplier commitment and verify mitigation options",
          "before the next planning cycle."
        ),

      intervention_priority == "3 - Standard Risk Review" ~
        paste(
          "Confirm delivery status and monitor against the promised date."
        ),

      intervention_priority == "4 - Impact Monitoring" ~
        paste(
          "Maintain contingency awareness because operational impact is",
          "very high despite risk falling outside the active review capacity."
        ),

      TRUE ~ "Continue routine monitoring."
    ),

    active_risk_watchlist_flag = risk_review_flag,

    risk_impact_display_score = round(
      predicted_late_probability * operational_impact_score,
      digits = 2
    )
  ) %>%
  arrange(
    intervention_priority,
    desc(predicted_late_probability),
    desc(operational_impact_score),
    scoring_record_id
  ) %>%
  mutate(
    watchlist_display_rank = row_number()
  ) %>%
  relocate(
    watchlist_display_rank,
    .before = scoring_record_id
  )

final_test_watchlist <- final_test_portfolio %>%
  filter(active_risk_watchlist_flag) %>%
  arrange(
    intervention_priority,
    desc(predicted_late_probability),
    desc(operational_impact_score),
    scoring_record_id
  ) %>%
  mutate(
    active_watchlist_rank = row_number()
  ) %>%
  relocate(
    active_watchlist_rank,
    .before = watchlist_display_rank
  )

# -------------------------------------------------------------------------
# Summarize final test watchlist performance
# -------------------------------------------------------------------------

final_test_priority_summary <- final_test_portfolio %>%
  group_by(intervention_priority) %>%
  summarize(
    orders = n(),
    share_of_orders = n() / nrow(final_test_portfolio),
    average_predicted_late_probability = mean(
      predicted_late_probability
    ),
    average_operational_impact_score = mean(
      operational_impact_score
    ),
    actual_late_orders = sum(late_delivery_target),
    actual_late_rate = mean(late_delivery_target),
    .groups = "drop"
  )

final_test_active_watchlist_summary <- final_test_watchlist %>%
  group_by(
    intervention_priority,
    operational_impact_tier
  ) %>%
  summarize(
    orders = n(),
    actual_late_orders = sum(late_delivery_target),
    actual_late_rate = mean(late_delivery_target),
    average_predicted_late_probability = mean(
      predicted_late_probability
    ),
    average_operational_impact_score = mean(
      operational_impact_score
    ),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# Validate final outputs
# -------------------------------------------------------------------------

stopifnot(
  nrow(final_test_portfolio) == test_records,
  nrow(final_test_watchlist) == orders_to_review,
  !anyDuplicated(final_test_portfolio$scoring_record_id),
  !anyDuplicated(final_test_watchlist$scoring_record_id),
  !anyNA(final_test_portfolio$predicted_late_probability),
  !anyNA(final_test_portfolio$operational_impact_score),
  !anyNA(final_test_portfolio$intervention_priority),
  all(final_test_watchlist$risk_review_flag),
  sum(final_test_portfolio$risk_review_flag) == orders_to_review
)

# -------------------------------------------------------------------------
# Save final evaluation model and test results
# -------------------------------------------------------------------------

dir.create(
  "models",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  file.path("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  final_evaluation_fit,
  file = file.path(
    "models",
    "final_logistic_test_evaluation_model.rds"
  ),
  compress = "xz"
)

write_csv(
  final_test_probability_metrics,
  file.path(
    "data", "processed", "final_test_probability_metrics.csv"
  ),
  na = ""
)

write_csv(
  test_review_capacity_summary,
  file.path(
    "data", "processed", "final_test_review_capacity.csv"
  ),
  na = ""
)

write_csv(
  test_risk_decile_summary,
  file.path(
    "data", "processed", "final_test_risk_deciles.csv"
  ),
  na = ""
)

write_csv(
  final_test_portfolio,
  file.path(
    "data", "processed", "final_test_supplier_risk_portfolio.csv"
  ),
  na = ""
)

write_csv(
  final_test_watchlist,
  file.path(
    "data", "processed", "final_test_supplier_risk_watchlist.csv"
  ),
  na = ""
)

write_csv(
  final_test_priority_summary,
  file.path(
    "data", "processed", "final_test_priority_summary.csv"
  ),
  na = ""
)

write_csv(
  final_test_active_watchlist_summary,
  file.path(
    "data", "processed", "final_test_active_watchlist_summary.csv"
  ),
  na = ""
)

write_csv(
  locked_policy,
  file.path(
    "data", "processed", "final_test_locked_policy.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print final test results
# -------------------------------------------------------------------------

elapsed_seconds <- as.numeric(
  difftime(
    Sys.time(),
    script_start_time,
    units = "secs"
  )
)

cat("\nFINAL ONE-TIME LOGISTIC TEST EVALUATION\n")
cat(
  "\nDevelopment refit records:",
  format(nrow(development_model_data), big.mark = ","),
  "\n"
)
cat(
  "Final test records:",
  format(test_records, big.mark = ","),
  "\n"
)
cat(
  "Final test late-delivery rate:",
  scales::percent(test_late_rate, accuracy = 0.1),
  "\n"
)

cat("\nFINAL TEST PROBABILITY METRICS\n")
print(
  final_test_probability_metrics,
  n = Inf
)

cat("\nLOCKED TOP-20% TEST REVIEW PERFORMANCE\n")
print(
  test_review_capacity_summary,
  width = Inf
)

cat("\nFINAL TEST RISK DECILES\n")
print(
  test_risk_decile_summary,
  n = Inf,
  width = Inf
)

cat("\nFINAL TEST INTERVENTION-PRIORITY DISTRIBUTION\n")
print(
  final_test_priority_summary,
  n = Inf,
  width = Inf
)

cat("\nFINAL TEST ACTIVE WATCHLIST SUMMARY\n")
print(
  final_test_active_watchlist_summary,
  n = Inf,
  width = Inf
)

cat("\nHIGHEST-PRIORITY FINAL TEST WATCHLIST ORDERS\n")
print(
  final_test_watchlist %>%
    select(
      active_watchlist_rank,
      scoring_record_id,
      supplier_name,
      material_name,
      promised_delivery_date,
      predicted_late_probability,
      risk_tier,
      operational_impact_score,
      operational_impact_tier,
      intervention_priority,
      recommended_action
    ) %>%
    slice_head(n = 20),
  n = Inf,
  width = Inf
)

cat(
  "\nTotal elapsed time:",
  round(elapsed_seconds, digits = 1),
  "seconds\n"
)

message(
  "Final one-time logistic test evaluation completed successfully."
)
message(
  "The logistic workflow was refit on training plus validation."
)
message(
  "The locked top-20% review and watchlist policies were applied unchanged."
)
message(
  paste(
    "These test results must now be documented without additional model or",
    "policy tuning."
  )
)
message(
  paste(
    "The next modeling step is a separate deployment refit on training plus",
    "validation plus test for future scoring and Power BI outputs."
  )
)
