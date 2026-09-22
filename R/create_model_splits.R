# Create Chronological Model Splits
#
# Divides the monthly supplier-material scoring dataset into chronological
# training, validation, and test periods.
#
# The test dataset must remain untouched until all model selection, tuning,
# preprocessing decisions, and classification-threshold decisions are final.

library(dplyr)
library(readr)

scoring_file <- file.path(
  "data",
  "processed",
  "monthly_scoring_dataset.csv"
)

if (!file.exists(scoring_file)) {
  stop(
    "monthly_scoring_dataset.csv was not found. ",
    "Run R/build_monthly_scoring_dataset.R first."
  )
}

scoring_data <- read_csv(
  scoring_file,
  show_col_types = FALSE
) %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    prediction_end_date = as.Date(prediction_end_date)
  )

# Define chronological period boundaries.
train_end_date <- as.Date("2025-10-31")

validation_start_date <- as.Date("2025-11-30")
validation_end_date <- as.Date("2026-02-28")

test_start_date <- as.Date("2026-03-31")
test_end_date <- as.Date("2026-07-31")

# Create the chronological splits.
training_data <- scoring_data %>%
  filter(
    scoring_date <= train_end_date
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

# Create a summary of each split.
summarize_split <- function(
  data,
  split_name
) {
  data %>%
    summarize(
      split = split_name,
      scoring_records = n(),
      scoring_months = n_distinct(scoring_date),
      first_scoring_date = min(scoring_date),
      last_scoring_date = max(scoring_date),
      suppliers = n_distinct(supplier_id),
      materials = n_distinct(material_id),
      supplier_material_relationships =
        n_distinct(supplier_material_id),
      positive_targets =
        sum(late_delivery_target),
      target_rate =
        mean(late_delivery_target)
    )
}

split_summary <- bind_rows(
  summarize_split(
    training_data,
    "Training"
  ),
  summarize_split(
    validation_data,
    "Validation"
  ),
  summarize_split(
    test_data,
    "Test"
  )
)

# Confirm that every source record appears in exactly one split.
split_record_ids <- c(
  training_data$scoring_record_id,
  validation_data$scoring_record_id,
  test_data$scoring_record_id
)

stopifnot(
  nrow(training_data) > 0,
  nrow(validation_data) > 0,
  nrow(test_data) > 0,
  nrow(training_data) +
    nrow(validation_data) +
    nrow(test_data) ==
    nrow(scoring_data),
  length(split_record_ids) ==
    length(unique(split_record_ids)),
  max(training_data$scoring_date) <
    min(validation_data$scoring_date),
  max(validation_data$scoring_date) <
    min(test_data$scoring_date),
  all(
    training_data$late_delivery_target %in%
      c(0L, 1L)
  ),
  all(
    validation_data$late_delivery_target %in%
      c(0L, 1L)
  ),
  all(
    test_data$late_delivery_target %in%
      c(0L, 1L)
  ),
  sum(training_data$late_delivery_target) > 0,
  sum(validation_data$late_delivery_target) > 0,
  sum(test_data$late_delivery_target) > 0
)

# Export the splits locally.
#
# All files under data/processed are ignored by Git. The reproducible script,
# rather than the generated datasets, will be committed.
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
    "split_summary.csv"
  ),
  na = ""
)

cat("\nCHRONOLOGICAL MODEL SPLITS\n")
print(
  split_summary,
  n = Inf,
  width = Inf
)

message(
  "Created chronological training, validation, and test datasets."
)

message(
  "The test dataset must remain untouched until the final model is selected."
)