# Prepare Model Data
#
# Selects model predictors, creates calendar features, and explicitly removes
# identifiers, future outcomes, and leakage-prone fields.
#
# Missing-value treatment and categorical encoding will be learned from the
# training data later through a tidymodels recipe.

library(dplyr)
library(readr)

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
    "Required model split files are missing: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

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

# These fields are generated from future delivery outcomes and must never be
# included as model predictors.
target_only_fields <- c(
  "late_quantity_next_30d",
  "late_quantity_rate_target"
)

# These fields identify records or entities. They remain available in the
# original scoring dataset for reporting, but are excluded from model
# training so the model does not memorize individual suppliers, materials,
# or relationships.
identifier_fields <- c(
  "scoring_record_id",
  "supplier_material_id",
  "supplier_id",
  "supplier_name",
  "material_id",
  "material_name"
)

# The prediction end date is determined directly from the scoring date and
# does not add independent predictive information.
administrative_fields <- c(
  "prediction_end_date"
)

# Define the observable fields that are permitted as model predictors.
#
# These values are either known on the scoring date or calculated solely
# from delivery history completed on or before the scoring date.
predictor_fields <- c(
  # Calendar features
  "scoring_month",
  "scoring_month_sin",
  "scoring_month_cos",

  # Supplier and material context
  "supplier_region",
  "supplier_tier",
  "material_category",
  "material_criticality",

  # Supplier-material relationship characteristics
  "quoted_lead_time_days",
  "minimum_order_quantity",
  "standard_order_quantity",
  "sourcing_allocation",
  "preferred_supplier_flag",
  "supplier_priority_rank",
  "unit_price",
  "approved_supplier_count",

  # Orders known to be due during the prediction window
  "orders_due_next_30d",
  "quantity_due_next_30d",
  "value_due_next_30d",
  "average_due_order_size_ratio",

  # Historical delivery volume
  "completed_order_count_90d",
  "completed_order_count_180d",
  "completed_order_count_365d",
  "ordered_quantity_90d",
  "ordered_quantity_180d",
  "ordered_quantity_365d",

  # Historical delivery performance
  "on_time_rate_90d",
  "on_time_rate_180d",
  "on_time_rate_365d",
  "late_quantity_rate_90d",
  "late_quantity_rate_180d",
  "late_quantity_rate_365d",
  "average_late_days_90d",
  "average_late_days_180d",
  "delivery_variability_180d",
  "maximum_late_days_180d",
  "recent_late_order_count_90d",
  "days_since_last_completed_delivery",

  # Current open-order exposure
  "open_order_count",
  "open_order_quantity",
  "open_order_value",
  "average_open_order_size_ratio"
)

outcome_field <- "late_delivery_target"

prepare_split <- function(data) {
  data %>%
    mutate(
      scoring_date = as.Date(scoring_date),

      # Month is observable at prediction time and allows the models to
      # capture seasonal patterns.
      scoring_month = as.integer(
        format(
          scoring_date,
          "%m"
        )
      ),

      # Cyclical encoding preserves the closeness of December and January.
      scoring_month_sin = sin(
        2 * pi * scoring_month / 12
      ),
      scoring_month_cos = cos(
        2 * pi * scoring_month / 12
      ),

      # Classification outcome represented as a factor for tidymodels.
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

      preferred_supplier_flag = factor(
        preferred_supplier_flag,
        levels = c(
          FALSE,
          TRUE
        ),
        labels = c(
          "not_preferred",
          "preferred"
        )
      )
    ) %>%
    select(
      all_of(predictor_fields),
      all_of(outcome_field)
    )
}

training_model_data <- prepare_split(
  training_data
)

validation_model_data <- prepare_split(
  validation_data
)

test_model_data <- prepare_split(
  test_data
)

# Confirm that all three datasets have identical fields and field order.
stopifnot(
  identical(
    names(training_model_data),
    names(validation_model_data)
  ),
  identical(
    names(training_model_data),
    names(test_model_data)
  )
)

# Confirm that prohibited fields were excluded.
prohibited_fields <- c(
  target_only_fields,
  identifier_fields,
  administrative_fields,
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
  )
)

# Confirm that each split contains both outcome classes.
stopifnot(
  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(training_model_data$late_delivery_target)
  ),
  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(validation_model_data$late_delivery_target)
  ),
  all(
    c(
      "on_time",
      "late"
    ) %in%
      unique(test_model_data$late_delivery_target)
  )
)

# Confirm that the test data remain chronologically later than validation,
# which remains later than training.
stopifnot(
  max(
    as.Date(training_data$scoring_date)
  ) <
    min(
      as.Date(validation_data$scoring_date)
    ),
  max(
    as.Date(validation_data$scoring_date)
  ) <
    min(
      as.Date(test_data$scoring_date)
    )
)

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

cat("\nMODEL DATA SUMMARY\n")

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
  group_by(split) %>%
  mutate(
    percentage = records / sum(records)
  ) %>%
  ungroup() %>%
  select(
    split,
    late_delivery_target,
    records,
    percentage
  )

print(
  model_data_summary,
  n = Inf
)

cat(
  "\nPredictor count:",
  length(predictor_fields),
  "\n"
)

cat(
  "Training records:",
  nrow(training_model_data),
  "\n"
)

cat(
  "Validation records:",
  nrow(validation_model_data),
  "\n"
)

cat(
  "Test records:",
  nrow(test_model_data),
  "\n"
)

message(
  "Created model-ready chronological datasets."
)

message(
  "The test model dataset must remain untouched until final evaluation."
)