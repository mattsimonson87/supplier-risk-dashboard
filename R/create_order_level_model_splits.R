# Create Order-Level Chronological Model Splits
#
# Divides the purchase-order scoring dataset into chronological training,
# validation, and test periods based on the purchase-order scoring date.
#
# The scoring date equals the purchase-order date.
#
# Orders from 2023 are excluded from model fitting and evaluation but served
# as historical warm-up data when the scoring features were calculated.
#
# The final test period must remain untouched until all preprocessing,
# model-selection, tuning, and threshold decisions are complete.

library(dplyr)
library(readr)

scoring_file <- file.path(
  "data",
  "processed",
  "purchase_order_scoring_dataset.csv"
)

if (!file.exists(scoring_file)) {
  stop(
    "purchase_order_scoring_dataset.csv was not found. ",
    "Run R/build_purchase_order_scoring_dataset.R first."
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
  ) %>%
  arrange(
    scoring_date,
    purchase_order_line_id
  )

stopifnot(
  nrow(scoring_data) > 0L,

  all(
    scoring_data$scoring_date ==
      scoring_data$order_date
  ),

  !anyDuplicated(
    scoring_data$scoring_record_id
  ),

  !anyDuplicated(
    scoring_data$purchase_order_line_id
  )
)

# -------------------------------------------------------------------------
# Define chronological periods
# -------------------------------------------------------------------------

warmup_start_date <- as.Date(
  "2023-01-01"
)

warmup_end_date <- as.Date(
  "2023-12-31"
)

training_start_date <- as.Date(
  "2024-01-01"
)

training_end_date <- as.Date(
  "2025-10-31"
)

validation_start_date <- as.Date(
  "2025-11-01"
)

validation_end_date <- as.Date(
  "2026-02-28"
)

test_start_date <- as.Date(
  "2026-03-01"
)

test_end_date <- as.Date(
  "2026-08-31"
)

# -------------------------------------------------------------------------
# Create chronological partitions
# -------------------------------------------------------------------------

warmup_data <- scoring_data %>%
  filter(
    scoring_date >= warmup_start_date,
    scoring_date <= warmup_end_date
  )

training_data <- scoring_data %>%
  filter(
    scoring_date >= training_start_date,
    scoring_date <= training_end_date
  )

validation_data <- scoring_data %>%
  filter(
    scoring_date >= validation_start_date,
    scoring_date <= validation_end_date
  )

test_data <- scoring_data %>%
  filter(
    scoring_date >= test_start_date,
    scoring_date <= test_end_date
  )

# -------------------------------------------------------------------------
# Summarize each period
# -------------------------------------------------------------------------

summarize_period <- function(
  data,
  period_name,
  modeling_role
) {
  data %>%
    summarize(
      period = period_name,

      role = modeling_role,

      scoring_records = n(),

      scoring_months = n_distinct(
        format(
          scoring_date,
          "%Y-%m"
        )
      ),

      first_scoring_date = min(
        scoring_date
      ),

      last_scoring_date = max(
        scoring_date
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

      positive_targets = sum(
        late_delivery_target
      ),

      target_rate = mean(
        late_delivery_target
      ),

      records_without_180d_history = sum(
        completed_order_count_180d == 0L
      ),

      percent_without_180d_history = mean(
        completed_order_count_180d == 0L
      )
    )
}

split_summary <- bind_rows(
  summarize_period(
    warmup_data,
    "Warm-up",
    "Historical context only"
  ),

  summarize_period(
    training_data,
    "Training",
    "Fit models and preprocessing"
  ),

  summarize_period(
    validation_data,
    "Validation",
    "Select model and threshold"
  ),

  summarize_period(
    test_data,
    "Test",
    "Final locked evaluation"
  )
)

# -------------------------------------------------------------------------
# Validate chronological separation
# -------------------------------------------------------------------------

all_partition_ids <- c(
  warmup_data$scoring_record_id,
  training_data$scoring_record_id,
  validation_data$scoring_record_id,
  test_data$scoring_record_id
)

stopifnot(
  nrow(warmup_data) > 0L,
  nrow(training_data) > 0L,
  nrow(validation_data) > 0L,
  nrow(test_data) > 0L,

  nrow(warmup_data) +
    nrow(training_data) +
    nrow(validation_data) +
    nrow(test_data) ==
    nrow(scoring_data),

  length(all_partition_ids) ==
    length(
      unique(
        all_partition_ids
      )
    ),

  max(warmup_data$scoring_date) <
    min(training_data$scoring_date),

  max(training_data$scoring_date) <
    min(validation_data$scoring_date),

  max(validation_data$scoring_date) <
    min(test_data$scoring_date),

  all(
    training_data$late_delivery_target %in%
      c(
        0L,
        1L
      )
  ),

  all(
    validation_data$late_delivery_target %in%
      c(
        0L,
        1L
      )
  ),

  all(
    test_data$late_delivery_target %in%
      c(
        0L,
        1L
      )
  ),

  sum(
    training_data$late_delivery_target
  ) > 0L,

  sum(
    validation_data$late_delivery_target
  ) > 0L,

  sum(
    test_data$late_delivery_target
  ) > 0L
)

# The training period should contain substantially fewer cold-start records
# than the 2023 warm-up period.
stopifnot(
  mean(
    training_data$completed_order_count_180d == 0L
  ) <
    mean(
      warmup_data$completed_order_count_180d == 0L
    )
)

# -------------------------------------------------------------------------
# Export model partitions
# -------------------------------------------------------------------------
#
# Generated files under data/processed are ignored by Git. Only this
# reproducible splitting script will be committed.

write_csv(
  warmup_data,
  file.path(
    "data",
    "processed",
    "warmup_data.csv"
  ),
  na = ""
)

write_csv(
  training_data,
  file.path(
    "data",
    "processed",
    "training_data.csv"
  ),
  na = ""
)

write_csv(
  validation_data,
  file.path(
    "data",
    "processed",
    "validation_data.csv"
  ),
  na = ""
)

write_csv(
  test_data,
  file.path(
    "data",
    "processed",
    "test_data.csv"
  ),
  na = ""
)

write_csv(
  split_summary,
  file.path(
    "data",
    "processed",
    "order_level_split_summary.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print summary
# -------------------------------------------------------------------------

cat(
  "\nORDER-LEVEL CHRONOLOGICAL SPLITS\n"
)

print(
  split_summary,
  n = Inf,
  width = Inf
)

message(
  "Created chronological warm-up, training, validation, and test datasets."
)

message(
  "Warm-up records are not used to fit or evaluate the models."
)

message(
  "The final test dataset must remain untouched until the model is locked."
)