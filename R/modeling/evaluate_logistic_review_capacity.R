# Evaluate Logistic Review Capacity
#
# Translates validation-period logistic regression predictions into a
# capacity-based supplier-risk review policy.
#
# The analysis answers the operational question:
#
#   If the supplier-risk team can review only the highest-risk share of new
#   purchase-order lines, how many materially late deliveries would the
#   model identify?
#
# This script uses existing validation predictions only. It does not refit
# the model and does not load or evaluate the final test dataset.

library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# Input file
# -------------------------------------------------------------------------

validation_predictions_file <- file.path(
  "data",
  "processed",
  "logistic_validation_predictions.csv"
)

if (!file.exists(validation_predictions_file)) {
  stop(
    "logistic_validation_predictions.csv was not found. ",
    "Run R/modeling/fit_logistic_baseline.R first."
  )
}

# -------------------------------------------------------------------------
# Load and validate prediction data
# -------------------------------------------------------------------------

validation_predictions <- read_csv(
  validation_predictions_file,
  show_col_types = FALSE
) %>%
  mutate(
    late_delivery_target = factor(
      late_delivery_target,
      levels = c(
        "on_time",
        "late"
      )
    )
  )

required_fields <- c(
  "late_delivery_target",
  ".pred_late",
  ".pred_on_time"
)

missing_fields <- setdiff(
  required_fields,
  names(validation_predictions)
)

if (length(missing_fields) > 0L) {
  stop(
    "Required validation prediction fields are missing: ",
    paste(missing_fields, collapse = ", ")
  )
}

stopifnot(
  nrow(validation_predictions) > 0L,
  !anyNA(validation_predictions$late_delivery_target),
  !anyNA(validation_predictions$.pred_late),
  all(
    validation_predictions$.pred_late >= 0 &
      validation_predictions$.pred_late <= 1
  ),
  all(
    abs(
      validation_predictions$.pred_late +
        validation_predictions$.pred_on_time -
        1
    ) < 0.000001
  )
)

# -------------------------------------------------------------------------
# Rank validation orders by predicted late-delivery risk
# -------------------------------------------------------------------------
#
# Ties are resolved by original row order so the ranked output is
# reproducible. The row order comes from the chronological validation file.

ranked_validation_predictions <- validation_predictions %>%
  mutate(
    validation_row_id = row_number(),
    actual_late = as.integer(
      late_delivery_target == "late"
    )
  ) %>%
  arrange(
    desc(.pred_late),
    validation_row_id
  ) %>%
  mutate(
    risk_rank = row_number(),
    cumulative_orders_reviewed = row_number(),
    cumulative_late_orders_found = cumsum(actual_late),
    cumulative_review_rate =
      cumulative_orders_reviewed /
      n(),
    cumulative_recall =
      cumulative_late_orders_found /
      sum(actual_late),
    cumulative_precision =
      cumulative_late_orders_found /
      cumulative_orders_reviewed
  )

validation_records <- nrow(
  ranked_validation_predictions
)

validation_late_orders <- sum(
  ranked_validation_predictions$actual_late
)

validation_late_rate <- mean(
  ranked_validation_predictions$actual_late
)

stopifnot(
  validation_late_orders > 0L,
  validation_late_rate > 0,
  validation_late_rate < 1
)

# -------------------------------------------------------------------------
# Evaluate fixed review capacities
# -------------------------------------------------------------------------

review_capacity_rates <- c(
  0.05,
  0.10,
  0.15,
  0.20,
  0.25,
  0.30
)

evaluate_review_capacity <- function(
  review_rate,
  ranked_predictions
) {
  orders_to_review <- ceiling(
    nrow(ranked_predictions) * review_rate
  )

  reviewed_orders <- ranked_predictions %>%
    slice_head(
      n = orders_to_review
    )

  late_orders_found <- sum(
    reviewed_orders$actual_late
  )

  precision_at_capacity <-
    late_orders_found /
    orders_to_review

  recall_at_capacity <-
    late_orders_found /
    validation_late_orders

  tibble(
    requested_review_rate = review_rate,
    orders_reviewed = orders_to_review,
    actual_review_rate =
      orders_to_review /
      validation_records,
    validation_orders = validation_records,
    late_orders_found = late_orders_found,
    total_late_orders = validation_late_orders,
    precision = precision_at_capacity,
    recall = recall_at_capacity,
    baseline_late_rate = validation_late_rate,
    lift_over_baseline =
      precision_at_capacity /
      validation_late_rate,
    minimum_predicted_probability_reviewed = min(
      reviewed_orders$.pred_late
    ),
    maximum_predicted_probability_not_reviewed = if (
      orders_to_review < validation_records
    ) {
      max(
        ranked_predictions$.pred_late[
          ranked_predictions$risk_rank > orders_to_review
        ]
      )
    } else {
      NA_real_
    }
  )
}

review_capacity_summary <- bind_rows(
  lapply(
    review_capacity_rates,
    evaluate_review_capacity,
    ranked_predictions = ranked_validation_predictions
  )
)

# -------------------------------------------------------------------------
# Create validation risk-decile summary
# -------------------------------------------------------------------------
#
# Decile 1 contains the highest-risk orders. Decile 10 contains the
# lowest-risk orders.

risk_decile_summary <- ranked_validation_predictions %>%
  mutate(
    risk_decile = ntile(
      risk_rank,
      10
    )
  ) %>%
  group_by(
    risk_decile
  ) %>%
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
    baseline_late_rate = validation_late_rate,
    lift_over_baseline =
      actual_late_rate /
      baseline_late_rate,
    share_of_all_late_orders =
      late_orders /
      validation_late_orders
  )

# -------------------------------------------------------------------------
# Create cumulative-gains output
# -------------------------------------------------------------------------
#
# One row is retained for each additional percentage point of review
# capacity. This table can feed a cumulative-gains visual in Power BI.

cumulative_gain_points <- seq(
  from = 0.01,
  to = 1.00,
  by = 0.01
)

cumulative_gains_summary <- bind_rows(
  lapply(
    cumulative_gain_points,
    function(review_rate) {
      orders_to_review <- ceiling(
        validation_records * review_rate
      )

      selected_row <- ranked_validation_predictions %>%
        slice(
          orders_to_review
        )

      tibble(
        requested_review_rate = review_rate,
        orders_reviewed = orders_to_review,
        actual_review_rate =
          orders_to_review /
          validation_records,
        late_orders_found =
          selected_row$cumulative_late_orders_found,
        cumulative_recall =
          selected_row$cumulative_recall,
        cumulative_precision =
          selected_row$cumulative_precision,
        baseline_expected_late_orders =
          orders_to_review * validation_late_rate,
        perfect_model_recall = min(
          orders_to_review /
            validation_late_orders,
          1
        )
      )
    }
  )
)

# -------------------------------------------------------------------------
# Identify a discussion candidate, not a locked operating policy
# -------------------------------------------------------------------------
#
# Twenty percent is retained as a discussion candidate because it represents
# a clear capacity constraint and is commonly understandable to business
# stakeholders. This script does not permanently lock that policy.

review_capacity_discussion_candidate <- review_capacity_summary %>%
  filter(
    requested_review_rate == 0.20
  )

# -------------------------------------------------------------------------
# Validate outputs
# -------------------------------------------------------------------------

stopifnot(
  nrow(review_capacity_summary) == length(review_capacity_rates),
  nrow(risk_decile_summary) == 10L,
  nrow(cumulative_gains_summary) == 100L,
  all(review_capacity_summary$orders_reviewed > 0L),
  all(review_capacity_summary$late_orders_found >= 0L),
  all(review_capacity_summary$precision >= 0),
  all(review_capacity_summary$precision <= 1),
  all(review_capacity_summary$recall >= 0),
  all(review_capacity_summary$recall <= 1),
  all(review_capacity_summary$lift_over_baseline >= 0),
  nrow(review_capacity_discussion_candidate) == 1L
)

# -------------------------------------------------------------------------
# Export validation decision-support outputs
# -------------------------------------------------------------------------

dir.create(
  file.path(
    "data",
    "processed"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  review_capacity_summary,
  file.path(
    "data",
    "processed",
    "logistic_validation_review_capacity.csv"
  ),
  na = ""
)

write_csv(
  risk_decile_summary,
  file.path(
    "data",
    "processed",
    "logistic_validation_risk_deciles.csv"
  ),
  na = ""
)

write_csv(
  cumulative_gains_summary,
  file.path(
    "data",
    "processed",
    "logistic_validation_cumulative_gains.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print results
# -------------------------------------------------------------------------

cat("\nLOGISTIC VALIDATION REVIEW-CAPACITY ANALYSIS\n")
cat(
  "\nValidation orders:",
  format(validation_records, big.mark = ","),
  "\n"
)
cat(
  "Validation late orders:",
  format(validation_late_orders, big.mark = ","),
  "\n"
)
cat(
  "Validation late-delivery rate:",
  scales::percent(validation_late_rate, accuracy = 0.1),
  "\n"
)

cat("\nFIXED REVIEW-CAPACITY RESULTS\n")
print(
  review_capacity_summary,
  n = Inf,
  width = Inf
)

cat("\nRISK-DECILE RESULTS\n")
print(
  risk_decile_summary,
  n = Inf,
  width = Inf
)

cat("\nTWENTY-PERCENT REVIEW-CAPACITY DISCUSSION CANDIDATE\n")
print(
  review_capacity_discussion_candidate,
  width = Inf
)

message(
  "Logistic validation review-capacity analysis completed successfully."
)
message(
  "The final test dataset was not loaded or evaluated."
)
message(
  paste(
    "No review-capacity policy was permanently locked by this script;",
    "the 20% result is presented as a discussion candidate."
  )
)
