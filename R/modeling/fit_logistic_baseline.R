# Fit Order-Level Logistic Regression Baseline
#
# Fits an interpretable logistic regression model that predicts whether a
# purchase-order line will arrive more than 7 calendar days after its
# promised delivery date.
#
# Preprocessing and model estimation use only the chronological training
# period. The validation period is used for model assessment and threshold
# analysis. The final test dataset is intentionally not loaded or evaluated.

library(tidymodels)
library(dplyr)
library(readr)
library(tidyr)

set.seed(423)

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

training_file <- file.path(
  "data",
  "processed",
  "training_model_data.csv"
)

validation_file <- file.path(
  "data",
  "processed",
  "validation_model_data.csv"
)

required_files <- c(
  training_file,
  validation_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required model datasets are missing: ",
    paste(missing_files, collapse = ", "),
    ". Run R/modeling/prepare_order_level_model_data.R first."
  )
}

# -------------------------------------------------------------------------
# Load training and validation data
# -------------------------------------------------------------------------

training_model_data <- read_csv(
  training_file,
  show_col_types = FALSE
)

validation_model_data <- read_csv(
  validation_file,
  show_col_types = FALSE
)

history_indicator_fields <- c(
  "has_90d_relationship_history",
  "has_180d_relationship_history",
  "has_365d_relationship_history",
  "has_180d_supplier_history"
)

# CSV files do not retain R factor classes. Restore the expected field types.
restore_field_types <- function(data) {
  missing_history_indicators <- setdiff(
    history_indicator_fields,
    names(data)
  )

  if (length(missing_history_indicators) > 0L) {
    stop(
      "Required history indicators are missing: ",
      paste(missing_history_indicators, collapse = ", ")
    )
  }

  data %>%
    mutate(
      late_delivery_target = factor(
        late_delivery_target,
        levels = c(
          "on_time",
          "late"
        )
      ),
      across(
        all_of(history_indicator_fields),
        ~ factor(
          .x,
          levels = c(
            "no_history",
            "has_history"
          )
        )
      )
    )
}

training_model_data <- restore_field_types(
  training_model_data
)

validation_model_data <- restore_field_types(
  validation_model_data
)

# -------------------------------------------------------------------------
# Validate model inputs
# -------------------------------------------------------------------------

stopifnot(
  nrow(training_model_data) > 0L,
  nrow(validation_model_data) > 0L,
  identical(
    names(training_model_data),
    names(validation_model_data)
  ),
  identical(
    levels(training_model_data$late_delivery_target),
    c("on_time", "late")
  ),
  identical(
    levels(validation_model_data$late_delivery_target),
    c("on_time", "late")
  ),
  !anyNA(training_model_data$late_delivery_target),
  !anyNA(validation_model_data$late_delivery_target),
  all(
    vapply(
      training_model_data[history_indicator_fields],
      is.factor,
      logical(1)
    )
  ),
  all(
    vapply(
      validation_model_data[history_indicator_fields],
      is.factor,
      logical(1)
    )
  )
)

# -------------------------------------------------------------------------
# Define training-only preprocessing
# -------------------------------------------------------------------------
#
# Missing historical values generally mean that no qualifying prior delivery
# history existed when the purchase order was created.
#
# Missing-value indicators preserve that information. Numeric missing values
# are then imputed using medians learned only from the training data.

logistic_recipe <- recipe(
  late_delivery_target ~ .,
  data = training_model_data
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

# -------------------------------------------------------------------------
# Define and fit the logistic regression baseline
# -------------------------------------------------------------------------

logistic_model <- logistic_reg(
  mode = "classification"
) %>%
  set_engine("glm")

logistic_workflow <- workflow() %>%
  add_recipe(logistic_recipe) %>%
  add_model(logistic_model)

logistic_fit <- fit(
  logistic_workflow,
  data = training_model_data
)

# -------------------------------------------------------------------------
# Generate validation probabilities
# -------------------------------------------------------------------------

validation_probabilities <- predict(
  logistic_fit,
  new_data = validation_model_data,
  type = "prob"
)

validation_predictions <- bind_cols(
  validation_model_data %>%
    select(late_delivery_target),
  validation_probabilities
)

stopifnot(
  nrow(validation_predictions) == nrow(validation_model_data),
  !anyNA(validation_predictions$.pred_late),
  all(
    validation_predictions$.pred_late >= 0 &
      validation_predictions$.pred_late <= 1
  ),
  all(
    validation_predictions$.pred_on_time >= 0 &
      validation_predictions$.pred_on_time <= 1
  ),
  all(
    abs(
      validation_predictions$.pred_late +
        validation_predictions$.pred_on_time - 1
    ) < 0.000001
  )
)

# -------------------------------------------------------------------------
# Probability-based validation metrics
# -------------------------------------------------------------------------

probability_metrics <- metric_set(
  roc_auc,
  pr_auc,
  mn_log_loss,
  brier_class
)

validation_probability_metrics <- probability_metrics(
  validation_predictions,
  truth = late_delivery_target,
  .pred_late,
  event_level = "second"
)

# -------------------------------------------------------------------------
# Evaluate the default 0.50 threshold
# -------------------------------------------------------------------------

validation_predictions <- validation_predictions %>%
  mutate(
    predicted_class_050 = factor(
      if_else(
        .pred_late >= 0.50,
        "late",
        "on_time"
      ),
      levels = c(
        "on_time",
        "late"
      )
    )
  )

classification_metrics <- metric_set(
  accuracy,
  bal_accuracy,
  sens,
  spec,
  precision,
  f_meas
)

validation_classification_metrics_050 <- classification_metrics(
  validation_predictions,
  truth = late_delivery_target,
  estimate = predicted_class_050,
  event_level = "second"
)

validation_confusion_matrix_050 <- conf_mat(
  validation_predictions,
  truth = late_delivery_target,
  estimate = predicted_class_050
)

# -------------------------------------------------------------------------
# Compare alternative validation thresholds
# -------------------------------------------------------------------------

threshold_grid <- seq(
  from = 0.05,
  to = 0.50,
  by = 0.025
)

evaluate_threshold <- function(threshold, prediction_data) {
  threshold_predictions <- prediction_data %>%
    mutate(
      predicted_class = factor(
        if_else(
          .pred_late >= threshold,
          "late",
          "on_time"
        ),
        levels = c(
          "on_time",
          "late"
        )
      )
    )

  confusion_counts <- threshold_predictions %>%
    summarize(
      true_positive = sum(
        predicted_class == "late" &
          late_delivery_target == "late"
      ),
      false_positive = sum(
        predicted_class == "late" &
          late_delivery_target == "on_time"
      ),
      true_negative = sum(
        predicted_class == "on_time" &
          late_delivery_target == "on_time"
      ),
      false_negative = sum(
        predicted_class == "on_time" &
          late_delivery_target == "late"
      ),
      alerts = sum(predicted_class == "late"),
      alert_rate = mean(predicted_class == "late")
    )

  threshold_metrics <- tibble(
    threshold = threshold,
    accuracy = accuracy_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class
    ),
    balanced_accuracy = bal_accuracy_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class,
      event_level = "second"
    ),
    recall = sens_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class,
      event_level = "second"
    ),
    specificity = spec_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class,
      event_level = "second"
    ),
    precision = precision_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class,
      event_level = "second"
    ),
    f1_score = f_meas_vec(
      truth = threshold_predictions$late_delivery_target,
      estimate = threshold_predictions$predicted_class,
      event_level = "second"
    )
  )

  bind_cols(
    threshold_metrics,
    confusion_counts
  )
}

validation_threshold_summary <- bind_rows(
  lapply(
    threshold_grid,
    evaluate_threshold,
    prediction_data = validation_predictions
  )
) %>%
  mutate(
    youden_index = recall + specificity - 1
  ) %>%
  arrange(threshold)

best_balanced_threshold <- validation_threshold_summary %>%
  filter(
    balanced_accuracy == max(
      balanced_accuracy,
      na.rm = TRUE
    )
  ) %>%
  slice(1)

best_f1_threshold <- validation_threshold_summary %>%
  filter(
    f1_score == max(
      f1_score,
      na.rm = TRUE
    )
  ) %>%
  slice(1)

# -------------------------------------------------------------------------
# Print validation results
# -------------------------------------------------------------------------

cat("\nORDER-LEVEL LOGISTIC REGRESSION BASELINE\n")
cat(
  "\nTraining predictors:",
  ncol(training_model_data) - 1L,
  "\n"
)
cat(
  "Training records:",
  format(nrow(training_model_data), big.mark = ","),
  "\n"
)
cat(
  "Validation records:",
  format(nrow(validation_model_data), big.mark = ","),
  "\n"
)
cat(
  "Training late-delivery rate:",
  scales::percent(
    mean(training_model_data$late_delivery_target == "late"),
    accuracy = 0.1
  ),
  "\n"
)
cat(
  "Validation late-delivery rate:",
  scales::percent(
    mean(validation_model_data$late_delivery_target == "late"),
    accuracy = 0.1
  ),
  "\n"
)

cat("\nPROBABILITY METRICS\n")
print(validation_probability_metrics, n = Inf)

cat("\nCLASSIFICATION METRICS AT 0.50\n")
print(validation_classification_metrics_050, n = Inf)

cat("\nCONFUSION MATRIX AT 0.50\n")
print(validation_confusion_matrix_050)

cat("\nPREDICTED LATE-PROBABILITY SUMMARY\n")
print(summary(validation_predictions$.pred_late))

cat("\nVALIDATION THRESHOLD COMPARISON\n")
print(validation_threshold_summary, n = Inf, width = Inf)

cat("\nTHRESHOLD WITH HIGHEST BALANCED ACCURACY\n")
print(best_balanced_threshold, width = Inf)

cat("\nTHRESHOLD WITH HIGHEST F1 SCORE\n")
print(best_f1_threshold, width = Inf)

# -------------------------------------------------------------------------
# Save local model artifacts and validation results
# -------------------------------------------------------------------------

dir.create(
  "models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  logistic_fit,
  file = file.path(
    "models",
    "order_level_logistic_baseline.rds"
  ),
  compress = "xz"
)

write_csv(
  validation_predictions,
  file.path(
    "data",
    "processed",
    "logistic_validation_predictions.csv"
  ),
  na = ""
)

write_csv(
  validation_probability_metrics,
  file.path(
    "data",
    "processed",
    "logistic_validation_probability_metrics.csv"
  ),
  na = ""
)

write_csv(
  validation_classification_metrics_050,
  file.path(
    "data",
    "processed",
    "logistic_validation_classification_metrics_050.csv"
  ),
  na = ""
)

write_csv(
  validation_threshold_summary,
  file.path(
    "data",
    "processed",
    "logistic_validation_threshold_summary.csv"
  ),
  na = ""
)

message(
  "Order-level logistic regression baseline completed successfully."
)
message(
  "The supplier-history indicator was restored as a categorical predictor."
)
message(
  "The final test dataset was not loaded or evaluated."
)
message(
  paste(
    "The validation threshold comparison is exploratory and does not yet",
    "lock the final operating threshold."
  )
)
