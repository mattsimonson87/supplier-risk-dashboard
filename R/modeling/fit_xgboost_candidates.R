# Fit Order-Level XGBoost Candidates
#
# Fits a controlled set of XGBoost classification models that predict
# whether a purchase-order line will arrive more than 7 calendar days after
# its promised delivery date.
#
# Models are fitted using only the chronological training period.
# Candidate selection and exploratory threshold analysis use only the
# validation period.
#
# The final test dataset is intentionally not loaded or evaluated.

library(tidymodels)
library(dplyr)
library(readr)
library(xgboost)

set.seed(425)

script_start_time <- Sys.time()

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

logistic_metrics_file <- file.path(
  "data",
  "processed",
  "logistic_validation_probability_metrics.csv"
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
    paste(
      missing_files,
      collapse = ", "
    ),
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

# -------------------------------------------------------------------------
# Restore field types lost during CSV export
# -------------------------------------------------------------------------

history_indicator_fields <- c(
  "has_90d_relationship_history",
  "has_180d_relationship_history",
  "has_365d_relationship_history",
  "has_180d_supplier_history"
)

restore_field_types <- function(data) {
  missing_history_indicators <- setdiff(
    history_indicator_fields,
    names(data)
  )

  if (length(missing_history_indicators) > 0L) {
    stop(
      "Required history indicators are missing: ",
      paste(
        missing_history_indicators,
        collapse = ", "
      )
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
        all_of(
          history_indicator_fields
        ),
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
    levels(
      training_model_data$late_delivery_target
    ),
    c(
      "on_time",
      "late"
    )
  ),

  identical(
    levels(
      validation_model_data$late_delivery_target
    ),
    c(
      "on_time",
      "late"
    )
  ),

  !anyNA(
    training_model_data$late_delivery_target
  ),

  !anyNA(
    validation_model_data$late_delivery_target
  ),

  all(
    vapply(
      training_model_data[
        history_indicator_fields
      ],
      is.factor,
      logical(1)
    )
  ),

  all(
    vapply(
      validation_model_data[
        history_indicator_fields
      ],
      is.factor,
      logical(1)
    )
  ),

  all(
    vapply(
      training_model_data[
        history_indicator_fields
      ],
      function(field) {
        identical(
          levels(field),
          c(
            "no_history",
            "has_history"
          )
        )
      },
      logical(1)
    )
  ),

  all(
    vapply(
      validation_model_data[
        history_indicator_fields
      ],
      function(field) {
        identical(
          levels(field),
          c(
            "no_history",
            "has_history"
          )
        )
      },
      logical(1)
    )
  )
)

# -------------------------------------------------------------------------
# Define training-only preprocessing
# -------------------------------------------------------------------------
#
# XGBoost does not require normalized numeric predictors.
#
# Missingness indicators are created before median imputation so the model
# can distinguish insufficient historical information from an ordinary
# median value.
#
# Nominal predictors are converted to numeric dummy variables.

xgboost_recipe <- recipe(
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
    one_hot = TRUE
  ) %>%
  step_zv(
    all_predictors()
  )

# -------------------------------------------------------------------------
# Define candidate configurations
# -------------------------------------------------------------------------
#
# This is a controlled candidate comparison rather than an exhaustive
# hyperparameter search.

candidate_grid <- tidyr::crossing(
  trees = c(
    300L,
    600L
  ),

  tree_depth = c(
    2L,
    3L,
    4L
  ),

  learn_rate = c(
    0.03,
    0.07
  )
) %>%
  mutate(
    candidate_id = sprintf(
      "XGB-%02d",
      row_number()
    ),

    min_n = 10L,

    loss_reduction = 0.01,

    sample_size = 0.80,

    mtry = 0.80
  ) %>%
  select(
    candidate_id,
    everything()
  )

message(
  "XGBoost candidate models to fit: ",
  nrow(candidate_grid),
  "."
)

# -------------------------------------------------------------------------
# Define validation metrics
# -------------------------------------------------------------------------

probability_metrics <- metric_set(
  roc_auc,
  pr_auc,
  mn_log_loss,
  brier_class
)

# -------------------------------------------------------------------------
# Fit and evaluate one candidate
# -------------------------------------------------------------------------

fit_xgboost_candidate <- function(
  candidate_row,
  training_data,
  validation_data
) {
  candidate_id <-
    candidate_row$candidate_id[[1]]

  message(
    "Fitting ",
    candidate_id,
    "..."
  )

  candidate_model <- boost_tree(
    mode = "classification",

    trees =
      candidate_row$trees[[1]],

    tree_depth =
      candidate_row$tree_depth[[1]],

    learn_rate =
      candidate_row$learn_rate[[1]],

    min_n =
      candidate_row$min_n[[1]],

    loss_reduction =
      candidate_row$loss_reduction[[1]],

    sample_size =
      candidate_row$sample_size[[1]],

    mtry =
      candidate_row$mtry[[1]]
  ) %>%
    set_engine(
      "xgboost",

      objective =
        "binary:logistic",

      eval_metric =
        "logloss",

      nthread =
        1,

      counts =
        FALSE,

      verbosity =
        0
    )

  candidate_workflow <- workflow() %>%
    add_recipe(
      xgboost_recipe
    ) %>%
    add_model(
      candidate_model
    )

  candidate_start_time <- Sys.time()

  candidate_fit <- fit(
    candidate_workflow,
    data = training_data
  )

  candidate_probabilities <- predict(
    candidate_fit,
    new_data = validation_data,
    type = "prob"
  )

  candidate_predictions <- bind_cols(
    validation_data %>%
      select(
        late_delivery_target
      ),

    candidate_probabilities
  )

  stopifnot(
    nrow(candidate_predictions) ==
      nrow(validation_data),

    !anyNA(
      candidate_predictions$.pred_late
    ),

    all(
      candidate_predictions$.pred_late >= 0 &
        candidate_predictions$.pred_late <= 1
    ),

    all(
      candidate_predictions$.pred_on_time >= 0 &
        candidate_predictions$.pred_on_time <= 1
    ),

    all(
      abs(
        candidate_predictions$.pred_late +
          candidate_predictions$.pred_on_time -
          1
      ) < 0.000001
    )
  )

  candidate_metrics <- probability_metrics(
    candidate_predictions,
    truth = late_delivery_target,
    .pred_late,
    event_level = "second"
  ) %>%
    select(
      .metric,
      .estimate
    ) %>%
    tidyr::pivot_wider(
      names_from = .metric,
      values_from = .estimate
    )

  candidate_end_time <- Sys.time()

  candidate_elapsed_seconds <- as.numeric(
    difftime(
      candidate_end_time,
      candidate_start_time,
      units = "secs"
    )
  )

  candidate_summary <- bind_cols(
    candidate_row,
    candidate_metrics
  ) %>%
    mutate(
      elapsed_seconds =
        candidate_elapsed_seconds
    )

  list(
    candidate_id =
      candidate_id,

    fit =
      candidate_fit,

    predictions =
      candidate_predictions,

    summary =
      candidate_summary
  )
}

# -------------------------------------------------------------------------
# Fit all candidate models
# -------------------------------------------------------------------------

candidate_results <- vector(
  mode = "list",
  length = nrow(candidate_grid)
)

names(candidate_results) <-
  candidate_grid$candidate_id

for (
  candidate_index in
  seq_len(
    nrow(candidate_grid)
  )
) {
  candidate_row <- candidate_grid[
    candidate_index,
  ]

  candidate_id <-
    candidate_row$candidate_id[[1]]

  candidate_results[[candidate_id]] <-
    fit_xgboost_candidate(
      candidate_row =
        candidate_row,

      training_data =
        training_model_data,

      validation_data =
        validation_model_data
    )
}

# Confirm that every candidate completed.
stopifnot(
  length(candidate_results) ==
    nrow(candidate_grid),

  all(
    names(candidate_results) ==
      candidate_grid$candidate_id
  ),

  all(
    vapply(
      candidate_results,
      function(result) {
        !is.null(result)
      },
      logical(1)
    )
  )
)

# -------------------------------------------------------------------------
# Compare candidate performance
# -------------------------------------------------------------------------

candidate_comparison <- bind_rows(
  lapply(
    candidate_results,
    function(result) {
      result$summary
    }
  )
) %>%
  arrange(
    desc(pr_auc),
    desc(roc_auc),
    mn_log_loss,
    brier_class
  ) %>%
  mutate(
    validation_rank =
      row_number()
  ) %>%
  relocate(
    validation_rank
  )

# -------------------------------------------------------------------------
# Select the leading validation candidate
# -------------------------------------------------------------------------
#
# PR AUC is the primary selection measure because late delivery is the
# less-common outcome.
#
# ROC AUC, log loss, and Brier score are retained as secondary checks.

best_candidate_summary <- candidate_comparison %>%
  slice(
    1
  )

best_candidate_id <-
  best_candidate_summary$candidate_id[[1]]

stopifnot(
  length(best_candidate_id) == 1L,

  best_candidate_id %in%
    names(candidate_results)
)

best_candidate_result <-
  candidate_results[[best_candidate_id]]

best_xgboost_fit <-
  best_candidate_result$fit

best_validation_predictions <-
  best_candidate_result$predictions

stopifnot(
  nrow(best_candidate_summary) == 1L,

  nrow(best_validation_predictions) ==
    nrow(validation_model_data)
)

# -------------------------------------------------------------------------
# Compare selected XGBoost with logistic regression
# -------------------------------------------------------------------------

xgboost_best_metrics <- best_candidate_summary %>%
  transmute(
    model =
      "XGBoost",

    roc_auc =
      roc_auc,

    pr_auc =
      pr_auc,

    mn_log_loss =
      mn_log_loss,

    brier_class =
      brier_class
  )

if (file.exists(logistic_metrics_file)) {
  logistic_metrics_long <- read_csv(
    logistic_metrics_file,
    show_col_types = FALSE
  )

  required_logistic_metrics <- c(
    "roc_auc",
    "pr_auc",
    "mn_log_loss",
    "brier_class"
  )

  stopifnot(
    all(
      required_logistic_metrics %in%
        logistic_metrics_long$.metric
    )
  )

  logistic_metrics <- logistic_metrics_long %>%
    select(
      .metric,
      .estimate
    ) %>%
    tidyr::pivot_wider(
      names_from = .metric,
      values_from = .estimate
    ) %>%
    mutate(
      model =
        "Logistic regression"
    ) %>%
    select(
      model,
      roc_auc,
      pr_auc,
      mn_log_loss,
      brier_class
    )

  model_comparison <- bind_rows(
    logistic_metrics,
    xgboost_best_metrics
  )
} else {
  warning(
    "Logistic validation metrics were not found. ",
    "XGBoost will be reported without the baseline comparison."
  )

  model_comparison <-
    xgboost_best_metrics
}

# Add differences relative to logistic regression when both models exist.
if (
  all(
    c(
      "Logistic regression",
      "XGBoost"
    ) %in%
      model_comparison$model
  )
) {
  logistic_row <- model_comparison %>%
    filter(
      model == "Logistic regression"
    )

  xgboost_row <- model_comparison %>%
    filter(
      model == "XGBoost"
    )

  model_improvement <- tibble(
    roc_auc_difference =
      xgboost_row$roc_auc -
        logistic_row$roc_auc,

    pr_auc_difference =
      xgboost_row$pr_auc -
        logistic_row$pr_auc,

    log_loss_difference =
      xgboost_row$mn_log_loss -
        logistic_row$mn_log_loss,

    brier_difference =
      xgboost_row$brier_class -
        logistic_row$brier_class
  )
} else {
  model_improvement <- tibble(
    roc_auc_difference =
      NA_real_,

    pr_auc_difference =
      NA_real_,

    log_loss_difference =
      NA_real_,

    brier_difference =
      NA_real_
  )
}

# -------------------------------------------------------------------------
# Exploratory threshold comparison
# -------------------------------------------------------------------------
#
# Thresholds are evaluated only on validation data.
#
# No final operating threshold is selected in this script.

threshold_grid <- seq(
  from = 0.05,
  to = 0.50,
  by = 0.025
)

evaluate_threshold <- function(
  threshold,
  prediction_data
) {
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

      alerts = sum(
        predicted_class == "late"
      ),

      alert_rate = mean(
        predicted_class == "late"
      )
    )

  threshold_metrics <- tibble(
    threshold =
      threshold,

    accuracy = accuracy_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class
    ),

    balanced_accuracy = bal_accuracy_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class,

      event_level =
        "second"
    ),

    recall = sens_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class,

      event_level =
        "second"
    ),

    specificity = spec_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class,

      event_level =
        "second"
    ),

    precision = precision_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class,

      event_level =
        "second"
    ),

    f1_score = f_meas_vec(
      truth =
        threshold_predictions$late_delivery_target,

      estimate =
        threshold_predictions$predicted_class,

      event_level =
        "second"
    )
  )

  bind_cols(
    threshold_metrics,
    confusion_counts
  )
}

xgboost_threshold_summary <- bind_rows(
  lapply(
    threshold_grid,
    evaluate_threshold,
    prediction_data =
      best_validation_predictions
  )
) %>%
  mutate(
    youden_index =
      recall +
      specificity -
      1
  ) %>%
  arrange(
    threshold
  )

best_balanced_threshold <- xgboost_threshold_summary %>%
  filter(
    balanced_accuracy ==
      max(
        balanced_accuracy,
        na.rm = TRUE
      )
  ) %>%
  slice(
    1
  )

best_f1_threshold <- xgboost_threshold_summary %>%
  filter(
    f1_score ==
      max(
        f1_score,
        na.rm = TRUE
      )
  ) %>%
  slice(
    1
  )

# -------------------------------------------------------------------------
# Print results
# -------------------------------------------------------------------------

cat(
  "\nORDER-LEVEL XGBOOST CANDIDATE COMPARISON\n"
)

cat(
  "\nTraining records:",
  format(
    nrow(training_model_data),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Validation records:",
  format(
    nrow(validation_model_data),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Candidate models:",
  nrow(candidate_grid),
  "\n"
)

cat(
  "\nALL XGBOOST CANDIDATES\n"
)

print(
  candidate_comparison,
  n = Inf,
  width = Inf
)

cat(
  "\nSELECTED XGBOOST CANDIDATE\n"
)

print(
  best_candidate_summary,
  width = Inf
)

cat(
  "\nLOGISTIC REGRESSION AND XGBOOST COMPARISON\n"
)

print(
  model_comparison,
  n = Inf,
  width = Inf
)

cat(
  "\nXGBOOST DIFFERENCE RELATIVE TO LOGISTIC REGRESSION\n"
)

print(
  model_improvement,
  width = Inf
)

cat(
  "\nSELECTED XGBOOST PROBABILITY SUMMARY\n"
)

print(
  summary(
    best_validation_predictions$.pred_late
  )
)

cat(
  "\nSELECTED XGBOOST THRESHOLD COMPARISON\n"
)

print(
  xgboost_threshold_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nXGBOOST THRESHOLD WITH HIGHEST BALANCED ACCURACY\n"
)

print(
  best_balanced_threshold,
  width = Inf
)

cat(
  "\nXGBOOST THRESHOLD WITH HIGHEST F1 SCORE\n"
)

print(
  best_f1_threshold,
  width = Inf
)

# -------------------------------------------------------------------------
# Save local model artifacts and validation results
# -------------------------------------------------------------------------

dir.create(
  "models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  best_xgboost_fit,
  file = file.path(
    "models",
    "order_level_xgboost_validation_model.rds"
  ),
  compress = "xz"
)

write_csv(
  candidate_comparison,
  file.path(
    "data",
    "processed",
    "xgboost_candidate_comparison.csv"
  ),
  na = ""
)

write_csv(
  best_validation_predictions,
  file.path(
    "data",
    "processed",
    "xgboost_validation_predictions.csv"
  ),
  na = ""
)

write_csv(
  model_comparison,
  file.path(
    "data",
    "processed",
    "predictive_model_validation_comparison.csv"
  ),
  na = ""
)

write_csv(
  model_improvement,
  file.path(
    "data",
    "processed",
    "xgboost_improvement_over_logistic.csv"
  ),
  na = ""
)

write_csv(
  xgboost_threshold_summary,
  file.path(
    "data",
    "processed",
    "xgboost_validation_threshold_summary.csv"
  ),
  na = ""
)

script_end_time <- Sys.time()

elapsed_seconds <- as.numeric(
  difftime(
    script_end_time,
    script_start_time,
    units = "secs"
  )
)

cat(
  "\nTotal elapsed time:",
  round(
    elapsed_seconds,
    digits = 1
  ),
  "seconds\n"
)

message(
  "XGBoost candidate comparison completed successfully."
)

message(
  "The final test dataset was not loaded or evaluated."
)

message(
  "The selected XGBoost model is based on validation PR AUC."
)

message(
  "The final operating threshold has not yet been locked."
)