# Prepare Order-Level Model Data
#
# Selects predictors for the purchase-order late-delivery probability model.
#
# The probability model uses current-order characteristics, supplier-material
# delivery history, supplier-wide delivery history, supplier workload, and
# promised-delivery seasonality.
#
# Identifiers, future outcomes, and operational-impact fields are excluded.
# Missing-value treatment and categorical encoding will be learned later
# using only the chronological training dataset.

library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

split_files <- c(
  training = file.path(
    "data",
    "processed",
    "training_data.csv"
  ),
  validation = file.path(
    "data",
    "processed",
    "validation_data.csv"
  ),
  test = file.path(
    "data",
    "processed",
    "test_data.csv"
  )
)

missing_files <- split_files[
  !file.exists(split_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required chronological split files are missing: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

# -------------------------------------------------------------------------
# Load chronological splits
# -------------------------------------------------------------------------

training_data <- read_csv(
  split_files[["training"]],
  show_col_types = FALSE
)

validation_data <- read_csv(
  split_files[["validation"]],
  show_col_types = FALSE
)

test_data <- read_csv(
  split_files[["test"]],
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Define model fields
# -------------------------------------------------------------------------

outcome_field <- "late_delivery_target"

# These predictors describe information plausibly related to whether the
# supplier will deliver the order materially late.
predictor_fields <- c(
  # Current purchase-order characteristics
  "planned_lead_time_days",
  "lead_time_pressure_days",
  "order_size_ratio",

  # Promised-delivery seasonality
  "promised_delivery_month_sin",
  "promised_delivery_month_cos",

  # Amount of supplier-material history
  "completed_order_count_90d",
  "completed_order_count_180d",
  "completed_order_count_365d",

  # Historical supplier-material purchase quantities
  "historical_quantity_90d",
  "historical_quantity_180d",
  "historical_quantity_365d",

  # Historical supplier-material delivery reliability
  "on_time_rate_90d",
  "on_time_rate_180d",
  "on_time_rate_365d",

  # Historical supplier-material quantity-weighted lateness
  "late_quantity_rate_90d",
  "late_quantity_rate_180d",
  "late_quantity_rate_365d",

  # Historical supplier-material delay severity and consistency
  "average_late_days_90d",
  "average_late_days_180d",
  "delivery_variability_180d",
  "maximum_late_days_180d",
  "recent_late_order_count_90d",
  "days_since_last_completed_delivery",

  # Focused supplier-wide delivery history
  #
  # These fields pool completed deliveries across all materials purchased
  # from the supplier. The 180-day window provides a stable recent view,
  # while the 90-day late-order count captures short-term deterioration.
  "supplier_completed_order_count_180d",
  "supplier_on_time_rate_180d",
  "supplier_late_quantity_rate_180d",
  "supplier_average_late_days_180d",
  "supplier_delivery_variability_180d",
  "supplier_recent_late_order_count_90d",
  "supplier_days_since_last_completed_delivery",

  # Supplier workload visible when the order is created
  "supplier_open_order_count",
  "supplier_open_order_quantity",
  "supplier_recent_order_count_90d",
  "supplier_recent_order_quantity_90d",
  "workload_pressure_90d",
  "supplier_average_open_order_size_ratio",

  # Cold-start and limited-history indicators
  "has_90d_relationship_history",
  "has_180d_relationship_history",
  "has_365d_relationship_history",
  "has_180d_supplier_history"
)

# Fields retained in the full scoring dataset for reporting but excluded from
# model training.
identifier_fields <- c(
  "scoring_record_id",
  "purchase_order_line_id",
  "purchase_order_id",
  "supplier_material_id",
  "supplier_id",
  "supplier_name",
  "material_id",
  "material_name"
)

# Fields known at order creation but reserved for the separate operational
# impact calculation or reporting layer.
impact_or_reporting_fields <- c(
  "supplier_region",
  "supplier_tier",
  "material_category",
  "material_criticality",
  "minimum_order_quantity",
  "standard_order_quantity",
  "unit_price",
  "order_value",
  "sourcing_allocation",
  "preferred_supplier_flag",
  "supplier_priority_rank",
  "approved_supplier_count",
  "supplier_open_order_value"
)

# Fields observed only after delivery and prohibited as predictors.
future_outcome_fields <- c(
  "actual_delivery_date",
  "actual_late_days"
)

# Dates used for splitting, reporting, or feature construction. They will not
# be given directly to the models.
date_and_administrative_fields <- c(
  "scoring_date",
  "order_date",
  "promised_delivery_date",
  "scoring_month",
  "scoring_month_sin",
  "scoring_month_cos",
  "promised_delivery_month"
)

history_indicator_fields <- c(
  "has_90d_relationship_history",
  "has_180d_relationship_history",
  "has_365d_relationship_history",
  "has_180d_supplier_history"
)

# -------------------------------------------------------------------------
# Prepare one chronological split
# -------------------------------------------------------------------------

prepare_model_split <- function(data) {
  missing_predictors <- setdiff(
    predictor_fields,
    names(data)
  )

  if (length(missing_predictors) > 0L) {
    stop(
      "Required predictor fields are missing: ",
      paste(
        missing_predictors,
        collapse = ", "
      )
    )
  }

  if (!outcome_field %in% names(data)) {
    stop(
      "The model outcome field is missing: ",
      outcome_field
    )
  }

  data %>%
    mutate(
      late_delivery_target = factor(
        late_delivery_target,
        levels = c(
          0,
          1
        ),
        labels = c(
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
            FALSE,
            TRUE
          ),
          labels = c(
            "no_history",
            "has_history"
          )
        )
      )
    ) %>%
    select(
      all_of(
        predictor_fields
      ),
      all_of(
        outcome_field
      )
    )
}

training_model_data <- prepare_model_split(
  training_data
)

validation_model_data <- prepare_model_split(
  validation_data
)

test_model_data <- prepare_model_split(
  test_data
)

# -------------------------------------------------------------------------
# Validate model datasets
# -------------------------------------------------------------------------

stopifnot(
  identical(
    names(training_model_data),
    names(validation_model_data)
  ),

  identical(
    names(training_model_data),
    names(test_model_data)
  ),

  nrow(training_model_data) ==
    nrow(training_data),

  nrow(validation_model_data) ==
    nrow(validation_data),

  nrow(test_model_data) ==
    nrow(test_data),

  all(
    predictor_fields %in%
      names(training_model_data)
  ),

  all(
    history_indicator_fields %in%
      names(training_model_data)
  )
)

prohibited_fields <- c(
  identifier_fields,
  impact_or_reporting_fields,
  future_outcome_fields,
  date_and_administrative_fields,
  "latent_reliability",
  "latent_relationship_effect",
  "latent_late_probability"
)

stopifnot(
  !any(
    prohibited_fields %in%
      names(training_model_data)
  ),

  !any(
    prohibited_fields %in%
      names(validation_model_data)
  ),

  !any(
    prohibited_fields %in%
      names(test_model_data)
  ),

  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(
        training_model_data$
          late_delivery_target
      )
  ),

  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(
        validation_model_data$
          late_delivery_target
      )
  ),

  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(
        test_model_data$
          late_delivery_target
      )
  )
)

# -------------------------------------------------------------------------
# Export model-ready chronological datasets
# -------------------------------------------------------------------------

write_csv(
  training_model_data,
  file.path(
    "data",
    "processed",
    "training_model_data.csv"
  ),
  na = ""
)

write_csv(
  validation_model_data,
  file.path(
    "data",
    "processed",
    "validation_model_data.csv"
  ),
  na = ""
)

write_csv(
  test_model_data,
  file.path(
    "data",
    "processed",
    "test_model_data.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print summary
# -------------------------------------------------------------------------

model_data_summary <- bind_rows(
  training_model_data %>%
    count(
      late_delivery_target,
      name = "records"
    ) %>%
    mutate(
      split = "Training"
    ),

  validation_model_data %>%
    count(
      late_delivery_target,
      name = "records"
    ) %>%
    mutate(
      split = "Validation"
    ),

  test_model_data %>%
    count(
      late_delivery_target,
      name = "records"
    ) %>%
    mutate(
      split = "Test"
    )
) %>%
  group_by(
    split
  ) %>%
  mutate(
    percentage =
      records /
      sum(records)
  ) %>%
  ungroup() %>%
  select(
    split,
    late_delivery_target,
    records,
    percentage
  )

cat(
  "\nORDER-LEVEL MODEL DATA SUMMARY\n"
)

print(
  model_data_summary,
  n = Inf
)

cat(
  "\nPredictor count:",
  length(
    predictor_fields
  ),
  "\n"
)

cat(
  "Training records:",
  nrow(
    training_model_data
  ),
  "\n"
)

cat(
  "Validation records:",
  nrow(
    validation_model_data
  ),
  "\n"
)

cat(
  "Test records:",
  nrow(
    test_model_data
  ),
  "\n"
)

cat(
  "\nProbability-model predictors:\n"
)

print(
  predictor_fields
)

cat(
  "\nFields reserved for operational impact or reporting:\n"
)

print(
  impact_or_reporting_fields
)

message(
  "Created order-level probability-model datasets."
)

message(
  "Focused supplier-level delivery-history predictors were added."
)

message(
  "Operational-impact fields were excluded from the probability model."
)

message(
  "The final test model dataset must remain untouched until evaluation."
)
