# Build Validation Supplier-Risk Watchlist
#
# Combines validation-period logistic risk predictions with the locked
# operational-impact rubric.
#
# Locked decision policies:
#
#   - Predictive model: logistic regression
#   - Review capacity: highest-risk 20% of validation orders
#   - Operational-impact score: fixed 0-to-100 point rubric
#   - Impact tiers: Low, Moderate, High, and Very High
#
# Risk and impact remain separate dimensions. The watchlist does not replace
# them with a newly optimized model or tune policy using validation outcomes.
# Actual outcomes are retained only for retrospective validation reporting.
#
# The final test dataset is not loaded or evaluated.

library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# Locked policy parameters
# -------------------------------------------------------------------------

locked_review_capacity <- 0.20

locked_policy <- tibble(
  policy_name = c(
    "Selected predictive model",
    "Review-capacity rule",
    "Operational-impact score range",
    "Impact-tier boundaries"
  ),
  policy_value = c(
    "Logistic regression",
    "Top 20% by predicted late-delivery probability",
    "0 to 100 points",
    "Low <40; Moderate 40-<60; High 60-<80; Very High 80-100"
  )
)

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

validation_data_file <- file.path(
  "data",
  "processed",
  "validation_data.csv"
)

validation_predictions_file <- file.path(
  "data",
  "processed",
  "logistic_validation_predictions.csv"
)

validation_impact_file <- file.path(
  "data",
  "processed",
  "validation_operational_impact_scores.csv"
)

required_files <- c(
  validation_data_file,
  validation_predictions_file,
  validation_impact_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required validation files are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

# -------------------------------------------------------------------------
# Load validation data, predictions, and impact scores
# -------------------------------------------------------------------------

validation_data <- read_csv(
  validation_data_file,
  show_col_types = FALSE
) %>%
  mutate(
    validation_row_id = row_number(),
    scoring_date = as.Date(scoring_date),
    order_date = as.Date(order_date),
    promised_delivery_date = as.Date(promised_delivery_date),
    actual_delivery_date = as.Date(actual_delivery_date),
    late_delivery_target = as.integer(late_delivery_target)
  )

validation_predictions <- read_csv(
  validation_predictions_file,
  show_col_types = FALSE
) %>%
  mutate(
    validation_row_id = row_number(),
    prediction_late_delivery_target = as.integer(
      late_delivery_target == "late"
    )
  )

validation_impact <- read_csv(
  validation_impact_file,
  show_col_types = FALSE
) %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    promised_delivery_date = as.Date(promised_delivery_date),
    impact_late_delivery_target = as.integer(late_delivery_target)
  )

# -------------------------------------------------------------------------
# Validate input alignment
# -------------------------------------------------------------------------
#
# The prediction file contains one row per validation record but does not
# contain business identifiers. Its row order is inherited from the same
# validation model dataset. Alignment is therefore verified against the
# validation target before attaching predictions to scoring_record_id.

required_prediction_fields <- c(
  "validation_row_id",
  "prediction_late_delivery_target",
  ".pred_late",
  ".pred_on_time"
)

required_impact_fields <- c(
  "scoring_record_id",
  "operational_impact_score",
  "operational_impact_tier",
  "impact_late_delivery_target"
)

missing_prediction_fields <- setdiff(
  required_prediction_fields,
  names(validation_predictions)
)

missing_impact_fields <- setdiff(
  required_impact_fields,
  names(validation_impact)
)

if (length(missing_prediction_fields) > 0L) {
  stop(
    "Required prediction fields are missing: ",
    paste(missing_prediction_fields, collapse = ", ")
  )
}

if (length(missing_impact_fields) > 0L) {
  stop(
    "Required impact fields are missing: ",
    paste(missing_impact_fields, collapse = ", ")
  )
}

stopifnot(
  nrow(validation_data) > 0L,
  nrow(validation_predictions) == nrow(validation_data),
  nrow(validation_impact) == nrow(validation_data),
  !anyDuplicated(validation_data$scoring_record_id),
  !anyDuplicated(validation_impact$scoring_record_id),
  !anyNA(validation_predictions$.pred_late),
  all(
    validation_predictions$.pred_late >= 0 &
      validation_predictions$.pred_late <= 1
  ),
  all(
    validation_predictions$prediction_late_delivery_target ==
      validation_data$late_delivery_target
  )
)

impact_alignment_check <- validation_data %>%
  select(
    scoring_record_id,
    validation_target = late_delivery_target
  ) %>%
  left_join(
    validation_impact %>%
      select(
        scoring_record_id,
        impact_late_delivery_target
      ),
    by = "scoring_record_id"
  )

stopifnot(
  !anyNA(impact_alignment_check$impact_late_delivery_target),
  all(
    impact_alignment_check$validation_target ==
      impact_alignment_check$impact_late_delivery_target
  )
)

# -------------------------------------------------------------------------
# Attach predicted probabilities to business identifiers
# -------------------------------------------------------------------------

validation_risk <- validation_data %>%
  select(
    validation_row_id,
    scoring_record_id,
    late_delivery_target
  ) %>%
  left_join(
    validation_predictions %>%
      select(
        validation_row_id,
        predicted_late_probability = .pred_late,
        predicted_on_time_probability = .pred_on_time
      ),
    by = "validation_row_id"
  )

stopifnot(
  !anyNA(validation_risk$predicted_late_probability)
)

# -------------------------------------------------------------------------
# Apply the locked top-20% risk-review policy
# -------------------------------------------------------------------------

orders_to_review <- ceiling(
  nrow(validation_risk) * locked_review_capacity
)

validation_risk <- validation_risk %>%
  arrange(
    desc(predicted_late_probability),
    validation_row_id
  ) %>%
  mutate(
    late_risk_rank = row_number(),
    late_risk_percentile = percent_rank(predicted_late_probability),
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
      levels = c(
        "Routine",
        "Elevated",
        "High",
        "Very High"
      ),
      ordered = TRUE
    )
  ) %>%
  arrange(validation_row_id)

locked_probability_boundary <- min(
  validation_risk$predicted_late_probability[
    validation_risk$risk_review_flag
  ]
)

# -------------------------------------------------------------------------
# Join risk, impact, and reporting fields
# -------------------------------------------------------------------------

validation_watchlist <- validation_impact %>%
  select(
    -late_delivery_target,
    -any_of("impact_late_delivery_target")
  ) %>%
  left_join(
    validation_risk %>%
      select(
        scoring_record_id,
        predicted_late_probability,
        predicted_on_time_probability,
        late_risk_rank,
        late_risk_percentile,
        risk_review_flag,
        risk_review_status,
        risk_tier,
        late_delivery_target
      ),
    by = "scoring_record_id"
  )

# -------------------------------------------------------------------------
# Assign intervention-priority tiers and recommended actions
# -------------------------------------------------------------------------
#
# The top-20% review population defines the active risk-review workload.
# Impact determines how orders inside that workload should be triaged.
#
# Orders outside the top-20% review population are not added to the active
# risk watchlist solely because of impact. Very-high-impact orders remain
# visible as contingency-monitoring items in Power BI.

validation_watchlist <- validation_watchlist %>%
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

      TRUE ~
        "Continue routine monitoring."
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

# -------------------------------------------------------------------------
# Create active top-20% risk watchlist
# -------------------------------------------------------------------------

active_validation_watchlist <- validation_watchlist %>%
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
# Create retrospective validation summaries
# -------------------------------------------------------------------------
#
# Actual outcomes are used only to describe validation-period policy
# performance. They do not determine risk, impact, priority, or rank.

watchlist_priority_summary <- validation_watchlist %>%
  group_by(intervention_priority) %>%
  summarize(
    orders = n(),
    share_of_orders = n() / nrow(validation_watchlist),
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

active_watchlist_summary <- active_validation_watchlist %>%
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

validation_watchlist_kpis <- tibble(
  metric = c(
    "Validation orders",
    "Locked review capacity",
    "Active watchlist orders",
    "Active watchlist share",
    "Validation late orders",
    "Late orders captured",
    "Watchlist recall",
    "Watchlist precision",
    "Validation baseline late rate",
    "Watchlist lift",
    "Validation probability boundary"
  ),
  value = c(
    nrow(validation_watchlist),
    locked_review_capacity,
    nrow(active_validation_watchlist),
    nrow(active_validation_watchlist) / nrow(validation_watchlist),
    sum(validation_watchlist$late_delivery_target),
    sum(active_validation_watchlist$late_delivery_target),
    sum(active_validation_watchlist$late_delivery_target) /
      sum(validation_watchlist$late_delivery_target),
    mean(active_validation_watchlist$late_delivery_target),
    mean(validation_watchlist$late_delivery_target),
    mean(active_validation_watchlist$late_delivery_target) /
      mean(validation_watchlist$late_delivery_target),
    locked_probability_boundary
  )
)

# -------------------------------------------------------------------------
# Validate watchlist outputs
# -------------------------------------------------------------------------

stopifnot(
  nrow(validation_watchlist) == nrow(validation_data),
  nrow(active_validation_watchlist) == orders_to_review,
  !anyDuplicated(validation_watchlist$scoring_record_id),
  !anyDuplicated(active_validation_watchlist$scoring_record_id),
  !anyNA(validation_watchlist$predicted_late_probability),
  !anyNA(validation_watchlist$operational_impact_score),
  !anyNA(validation_watchlist$intervention_priority),
  all(active_validation_watchlist$risk_review_flag),
  all(
    validation_watchlist$risk_review_flag ==
      (
        validation_watchlist$late_risk_rank <=
          orders_to_review
      )
  ),
  all(
    validation_watchlist$risk_impact_display_score >= 0
  ),
  sum(validation_watchlist$risk_review_flag) == orders_to_review
)

# -------------------------------------------------------------------------
# Export watchlist and policy outputs
# -------------------------------------------------------------------------

dir.create(
  file.path("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  validation_watchlist,
  file.path(
    "data",
    "processed",
    "validation_supplier_risk_portfolio.csv"
  ),
  na = ""
)

write_csv(
  active_validation_watchlist,
  file.path(
    "data",
    "processed",
    "validation_supplier_risk_watchlist.csv"
  ),
  na = ""
)

write_csv(
  watchlist_priority_summary,
  file.path(
    "data",
    "processed",
    "validation_watchlist_priority_summary.csv"
  ),
  na = ""
)

write_csv(
  active_watchlist_summary,
  file.path(
    "data",
    "processed",
    "validation_active_watchlist_summary.csv"
  ),
  na = ""
)

write_csv(
  validation_watchlist_kpis,
  file.path(
    "data",
    "processed",
    "validation_watchlist_kpis.csv"
  ),
  na = ""
)

write_csv(
  locked_policy,
  file.path(
    "data",
    "processed",
    "locked_supplier_risk_policy.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print results
# -------------------------------------------------------------------------

cat("\nVALIDATION SUPPLIER-RISK WATCHLIST\n")
cat(
  "\nValidation orders:",
  format(nrow(validation_watchlist), big.mark = ","),
  "\n"
)
cat(
  "Locked review capacity:",
  scales::percent(locked_review_capacity, accuracy = 1),
  "\n"
)
cat(
  "Active watchlist orders:",
  format(nrow(active_validation_watchlist), big.mark = ","),
  "\n"
)
cat(
  "Validation probability boundary:",
  scales::percent(locked_probability_boundary, accuracy = 0.1),
  "\n"
)

cat("\nWATCHLIST KPI SUMMARY\n")
print(
  validation_watchlist_kpis,
  n = Inf,
  width = Inf
)

cat("\nINTERVENTION-PRIORITY DISTRIBUTION\n")
print(
  watchlist_priority_summary,
  n = Inf,
  width = Inf
)

cat("\nACTIVE WATCHLIST SUMMARY\n")
print(
  active_watchlist_summary,
  n = Inf,
  width = Inf
)

cat("\nHIGHEST-PRIORITY ACTIVE WATCHLIST ORDERS\n")
print(
  active_validation_watchlist %>%
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

message(
  "Validation supplier-risk watchlist completed successfully."
)
message(
  "The locked top-20% review-capacity policy was applied."
)
message(
  "Risk probability and operational impact remain separate dimensions."
)
message(
  "The final test dataset was not loaded or evaluated."
)
