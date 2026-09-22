# Profile Purchase-Order Scoring Dataset
#
# Reviews missingness, target prevalence, historical coverage, planned lead
# times, and point-in-time validity before model development.
#
# Every purchase-order line is scored on its order date.
#
# This script does not modify the analytical dataset.

library(dplyr)
library(readr)
library(tidyr)

scoring_file <- file.path(
  "data",
  "processed",
  "purchase_order_scoring_dataset.csv"
)

if (!file.exists(scoring_file)) {
  stop(
    "purchase_order_scoring_dataset.csv was not found. ",
    "Run R/feature_engineering/build_purchase_order_scoring_dataset.R first."
  )
}

# -------------------------------------------------------------------------
# Load scoring data
# -------------------------------------------------------------------------

scoring_data <- read_csv(
  scoring_file,
  show_col_types = FALSE
) %>%
  mutate(
    scoring_date = as.Date(
      scoring_date
    ),
    order_date = as.Date(
      order_date
    ),
    promised_delivery_date = as.Date(
      promised_delivery_date
    ),
    actual_delivery_date = as.Date(
      actual_delivery_date
    ),
    late_delivery_target = as.integer(
      late_delivery_target
    )
  )

# -------------------------------------------------------------------------
# Overall dataset summary
# -------------------------------------------------------------------------

overall_summary <- scoring_data %>%
  summarize(
    scoring_records = n(),

    purchase_order_lines = n_distinct(
      purchase_order_line_id
    ),

    suppliers = n_distinct(
      supplier_id
    ),

    materials = n_distinct(
      material_id
    ),

    supplier_material_relationships = n_distinct(
      supplier_material_id
    ),

    first_scoring_date = min(
      scoring_date
    ),

    last_scoring_date = max(
      scoring_date
    ),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    )
  )

# -------------------------------------------------------------------------
# Missingness profile
# -------------------------------------------------------------------------

missingness_summary <- scoring_data %>%
  summarize(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "field",
    values_to = "missing_count"
  ) %>%
  mutate(
    missing_percent =
      missing_count /
      nrow(scoring_data)
  ) %>%
  arrange(
    desc(missing_percent),
    field
  )

fields_with_missing_values <- missingness_summary %>%
  filter(
    missing_count > 0L
  )

# -------------------------------------------------------------------------
# Target rate by order month
# -------------------------------------------------------------------------
#
# The scoring date equals the order date, so this represents the target rate
# for purchase orders created during each month.

monthly_target_summary <- scoring_data %>%
  mutate(
    order_month = as.Date(
      format(
        order_date,
        "%Y-%m-01"
      )
    )
  ) %>%
  group_by(
    order_month
  ) %>%
  summarize(
    scoring_records = n(),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    ),

    .groups = "drop"
  ) %>%
  arrange(
    order_month
  )

# -------------------------------------------------------------------------
# Target rate by promised-delivery calendar month
# -------------------------------------------------------------------------
#
# The synthetic purchase-order generator intentionally introduced seasonal
# differences based on the promised delivery month.

promised_month_target_summary <- scoring_data %>%
  mutate(
    promised_delivery_month = as.integer(
      format(
        promised_delivery_date,
        "%m"
      )
    ),

    promised_delivery_month_name = factor(
      month.name[
        promised_delivery_month
      ],
      levels = month.name,
      ordered = TRUE
    )
  ) %>%
  group_by(
    promised_delivery_month,
    promised_delivery_month_name
  ) %>%
  summarize(
    scoring_records = n(),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    ),

    .groups = "drop"
  ) %>%
  arrange(
    promised_delivery_month
  )

# -------------------------------------------------------------------------
# Historical coverage
# -------------------------------------------------------------------------

historical_coverage_summary <- scoring_data %>%
  summarize(
    no_90d_history = sum(
      completed_order_count_90d == 0L
    ),

    no_180d_history = sum(
      completed_order_count_180d == 0L
    ),

    no_365d_history = sum(
      completed_order_count_365d == 0L
    ),

    percent_without_90d_history = mean(
      completed_order_count_90d == 0L
    ),

    percent_without_180d_history = mean(
      completed_order_count_180d == 0L
    ),

    percent_without_365d_history = mean(
      completed_order_count_365d == 0L
    ),

    median_completed_orders_90d = median(
      completed_order_count_90d
    ),

    median_completed_orders_180d = median(
      completed_order_count_180d
    ),

    median_completed_orders_365d = median(
      completed_order_count_365d
    ),

    maximum_completed_orders_180d = max(
      completed_order_count_180d
    )
  )

# -------------------------------------------------------------------------
# Historical missingness validation
# -------------------------------------------------------------------------
#
# Historical rates should be missing only when no qualifying delivery history
# exists. Delivery variability requires at least two historical observations.

historical_missingness_validation <- scoring_data %>%
  summarize(
    missing_on_time_90d_with_history = sum(
      is.na(on_time_rate_90d) &
        completed_order_count_90d > 0L
    ),

    missing_on_time_180d_with_history = sum(
      is.na(on_time_rate_180d) &
        completed_order_count_180d > 0L
    ),

    missing_on_time_365d_with_history = sum(
      is.na(on_time_rate_365d) &
        completed_order_count_365d > 0L
    ),

    missing_late_quantity_90d_with_history = sum(
      is.na(late_quantity_rate_90d) &
        completed_order_count_90d > 0L
    ),

    missing_late_quantity_180d_with_history = sum(
      is.na(late_quantity_rate_180d) &
        completed_order_count_180d > 0L
    ),

    missing_late_quantity_365d_with_history = sum(
      is.na(late_quantity_rate_365d) &
        completed_order_count_365d > 0L
    ),

    missing_variability_with_two_or_more_orders = sum(
      is.na(delivery_variability_180d) &
        completed_order_count_180d >= 2L
    ),

    nonmissing_variability_with_fewer_than_two_orders = sum(
      !is.na(delivery_variability_180d) &
        completed_order_count_180d < 2L
    )
  )

# -------------------------------------------------------------------------
# Planned lead-time profile
# -------------------------------------------------------------------------

planned_lead_time_summary <- scoring_data %>%
  summarize(
    minimum_planned_lead_time = min(
      planned_lead_time_days
    ),

    first_quartile_planned_lead_time = as.numeric(
      quantile(
        planned_lead_time_days,
        probs = 0.25
      )
    ),

    median_planned_lead_time = median(
      planned_lead_time_days
    ),

    mean_planned_lead_time = mean(
      planned_lead_time_days
    ),

    third_quartile_planned_lead_time = as.numeric(
      quantile(
        planned_lead_time_days,
        probs = 0.75
      )
    ),

    maximum_planned_lead_time = max(
      planned_lead_time_days
    ),

    records_scored_on_order_date = sum(
      scoring_date == order_date
    )
  )

planned_lead_time_distribution <- scoring_data %>%
  count(
    planned_lead_time_days,
    name = "scoring_records"
  ) %>%
  mutate(
    percentage =
      scoring_records /
      sum(scoring_records)
  ) %>%
  arrange(
    planned_lead_time_days
  )

# -------------------------------------------------------------------------
# Cold-start analysis
# -------------------------------------------------------------------------
#
# A cold-start record has no completed delivery history for that specific
# supplier-material relationship as of the order date.

cold_start_summary <- scoring_data %>%
  mutate(
    relationship_history_status = case_when(
      completed_order_count_365d == 0L ~
        "No prior completed order",

      completed_order_count_180d < 3L ~
        "Limited history",

      TRUE ~
        "Established history"
    )
  ) %>%
  group_by(
    relationship_history_status
  ) %>%
  summarize(
    scoring_records = n(),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(scoring_records)
  )

# -------------------------------------------------------------------------
# Target rates by selected descriptive dimensions
# -------------------------------------------------------------------------
#
# These summaries are descriptive. They do not establish that supplier tier
# or material category causes late delivery.

supplier_tier_target_summary <- scoring_data %>%
  group_by(
    supplier_tier
  ) %>%
  summarize(
    scoring_records = n(),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(target_rate)
  )

material_category_target_summary <- scoring_data %>%
  group_by(
    material_category
  ) %>%
  summarize(
    scoring_records = n(),

    positive_targets = sum(
      late_delivery_target
    ),

    target_rate = mean(
      late_delivery_target
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(target_rate)
  )

# -------------------------------------------------------------------------
# Point-in-time and target validation
# -------------------------------------------------------------------------

dataset_validation <- scoring_data %>%
  summarize(
    duplicated_scoring_records = sum(
      duplicated(
        scoring_record_id
      )
    ),

    duplicated_purchase_order_lines = sum(
      duplicated(
        purchase_order_line_id
      )
    ),

    invalid_target_rows = sum(
      late_delivery_target !=
        as.integer(
          actual_late_days > 7L
        )
    ),

    scoring_date_not_equal_to_order_date = sum(
      scoring_date != order_date
    ),

    invalid_planned_lead_time = sum(
      planned_lead_time_days !=
        as.integer(
          promised_delivery_date -
            order_date
        )
    ),

    nonpositive_planned_lead_time = sum(
      planned_lead_time_days <= 0L
    ),

    invalid_order_size_ratio = sum(
      order_size_ratio <= 0
    ),

    invalid_open_order_workload = sum(
      supplier_open_order_count < 1L
    ),

    future_completed_delivery_reference = sum(
      days_since_last_completed_delivery < 0,
      na.rm = TRUE
    )
  )

# -------------------------------------------------------------------------
# Print profile
# -------------------------------------------------------------------------

cat(
  "\nOVERALL DATASET SUMMARY\n"
)

print(
  overall_summary,
  width = Inf
)

cat(
  "\nDATASET VALIDATION\n"
)

print(
  dataset_validation,
  width = Inf
)

cat(
  "\nFIELDS WITH MISSING VALUES\n"
)

print(
  fields_with_missing_values,
  n = Inf,
  width = Inf
)

cat(
  "\nHISTORICAL COVERAGE\n"
)

print(
  historical_coverage_summary,
  width = Inf
)

cat(
  "\nHISTORICAL MISSINGNESS VALIDATION\n"
)

print(
  historical_missingness_validation,
  width = Inf
)

cat(
  "\nPLANNED LEAD-TIME SUMMARY\n"
)

print(
  planned_lead_time_summary,
  width = Inf
)

cat(
  "\nPLANNED LEAD-TIME DISTRIBUTION\n"
)

print(
  planned_lead_time_distribution,
  n = Inf,
  width = Inf
)

cat(
  "\nRELATIONSHIP HISTORY STATUS\n"
)

print(
  cold_start_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nTARGET RATE BY ORDER MONTH\n"
)

print(
  monthly_target_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nTARGET RATE BY PROMISED-DELIVERY MONTH\n"
)

print(
  promised_month_target_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nTARGET RATE BY SUPPLIER TIER\n"
)

print(
  supplier_tier_target_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nTARGET RATE BY MATERIAL CATEGORY\n"
)

print(
  material_category_target_summary,
  n = Inf,
  width = Inf
)

# -------------------------------------------------------------------------
# Required checks
# -------------------------------------------------------------------------

stopifnot(
  dataset_validation$
    duplicated_scoring_records == 0L,

  dataset_validation$
    duplicated_purchase_order_lines == 0L,

  dataset_validation$
    invalid_target_rows == 0L,

  dataset_validation$
    scoring_date_not_equal_to_order_date == 0L,

  dataset_validation$
    invalid_planned_lead_time == 0L,

  dataset_validation$
    nonpositive_planned_lead_time == 0L,

  dataset_validation$
    invalid_order_size_ratio == 0L,

  dataset_validation$
    invalid_open_order_workload == 0L,

  dataset_validation$
    future_completed_delivery_reference == 0L,

  historical_missingness_validation$
    missing_on_time_90d_with_history == 0L,

  historical_missingness_validation$
    missing_on_time_180d_with_history == 0L,

  historical_missingness_validation$
    missing_on_time_365d_with_history == 0L,

  historical_missingness_validation$
    missing_late_quantity_90d_with_history == 0L,

  historical_missingness_validation$
    missing_late_quantity_180d_with_history == 0L,

  historical_missingness_validation$
    missing_late_quantity_365d_with_history == 0L,

  historical_missingness_validation$
    missing_variability_with_two_or_more_orders == 0L,

  historical_missingness_validation$
    nonmissing_variability_with_fewer_than_two_orders == 0L,

  overall_summary$scoring_records ==
    overall_summary$purchase_order_lines,

  overall_summary$target_rate > 0.05,

  overall_summary$target_rate < 0.40
)

message(
  "Purchase-order scoring dataset profile completed successfully."
)
