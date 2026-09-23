# Build Validation Operational-Impact Scores
#
# Creates a transparent operational-impact score for validation-period
# purchase-order lines.
#
# The late-delivery probability model answers:
#
#   How likely is the order to arrive materially late?
#
# This impact score answers:
#
#   If the order arrives materially late, how consequential could it be?
#
# The score intentionally excludes predicted late-delivery probability.
# Risk and impact will be combined later in a separate watchlist script.
#
# Version 1 uses fields currently available at order creation:
#
#   - Material criticality
#   - Number of approved suppliers
#   - Sourcing allocation
#   - Order value relative to the training-period distribution
#
# The scoring rubric is fixed and transparent. Order-value thresholds are
# learned from the chronological training period only and then applied to the
# validation period.
#
# The final test dataset is not loaded or evaluated.

library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

training_file <- file.path(
  "data",
  "processed",
  "training_data.csv"
)

validation_file <- file.path(
  "data",
  "processed",
  "validation_data.csv"
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
    "Required chronological split files are missing: ",
    paste(missing_files, collapse = ", "),
    ". Run R/modeling/create_order_level_model_splits.R first."
  )
}

# -------------------------------------------------------------------------
# Load training and validation data
# -------------------------------------------------------------------------

training_data <- read_csv(
  training_file,
  show_col_types = FALSE
)

validation_data <- read_csv(
  validation_file,
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Define and validate required fields
# -------------------------------------------------------------------------

required_impact_fields <- c(
  "scoring_record_id",
  "purchase_order_line_id",
  "purchase_order_id",
  "supplier_material_id",
  "supplier_id",
  "supplier_name",
  "supplier_region",
  "supplier_tier",
  "material_id",
  "material_name",
  "material_category",
  "material_criticality",
  "scoring_date",
  "promised_delivery_date",
  "ordered_quantity",
  "unit_price",
  "order_value",
  "sourcing_allocation",
  "approved_supplier_count",
  "late_delivery_target"
)

missing_training_fields <- setdiff(
  required_impact_fields,
  names(training_data)
)

missing_validation_fields <- setdiff(
  required_impact_fields,
  names(validation_data)
)

if (length(missing_training_fields) > 0L) {
  stop(
    "Required training impact fields are missing: ",
    paste(missing_training_fields, collapse = ", ")
  )
}

if (length(missing_validation_fields) > 0L) {
  stop(
    "Required validation impact fields are missing: ",
    paste(missing_validation_fields, collapse = ", ")
  )
}

training_data <- training_data %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    promised_delivery_date = as.Date(promised_delivery_date)
  )

validation_data <- validation_data %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    promised_delivery_date = as.Date(promised_delivery_date)
  )

stopifnot(
  nrow(training_data) > 0L,
  nrow(validation_data) > 0L,
  !anyDuplicated(training_data$scoring_record_id),
  !anyDuplicated(validation_data$scoring_record_id),
  !anyNA(training_data$order_value),
  !anyNA(validation_data$order_value),
  all(training_data$order_value > 0),
  all(validation_data$order_value > 0),
  !anyNA(validation_data$sourcing_allocation),
  all(validation_data$sourcing_allocation >= 0),
  all(validation_data$sourcing_allocation <= 1),
  !anyNA(validation_data$approved_supplier_count),
  all(validation_data$approved_supplier_count >= 1)
)

# -------------------------------------------------------------------------
# Normalize and validate material criticality
# -------------------------------------------------------------------------
#
# The script recognizes common labels while retaining the original value for
# reporting. Any unrecognized label stops the script rather than silently
# assigning an impact score.

normalize_material_criticality <- function(values) {
  normalized_values <- tolower(
    trimws(
      as.character(values)
    )
  )

  case_when(
    normalized_values %in% c(
      "low",
      "noncritical",
      "non-critical"
    ) ~ "Low",

    normalized_values %in% c(
      "medium",
      "moderate"
    ) ~ "Medium",

    normalized_values %in% c(
      "high",
      "critical",
      "very high",
      "very_high"
    ) ~ "High",

    TRUE ~ NA_character_
  )
}

training_data <- training_data %>%
  mutate(
    material_criticality_normalized =
      normalize_material_criticality(material_criticality)
  )

validation_data <- validation_data %>%
  mutate(
    material_criticality_normalized =
      normalize_material_criticality(material_criticality)
  )

if (anyNA(training_data$material_criticality_normalized)) {
  unknown_values <- training_data %>%
    filter(is.na(material_criticality_normalized)) %>%
    distinct(material_criticality) %>%
    pull(material_criticality)

  stop(
    "Unrecognized training material-criticality values: ",
    paste(unknown_values, collapse = ", ")
  )
}

if (anyNA(validation_data$material_criticality_normalized)) {
  unknown_values <- validation_data %>%
    filter(is.na(material_criticality_normalized)) %>%
    distinct(material_criticality) %>%
    pull(material_criticality)

  stop(
    "Unrecognized validation material-criticality values: ",
    paste(unknown_values, collapse = ", ")
  )
}

# -------------------------------------------------------------------------
# Learn order-value thresholds from the training period
# -------------------------------------------------------------------------
#
# Quartiles are calculated from training data only. The thresholds are then
# frozen and applied to validation orders.

order_value_thresholds <- quantile(
  training_data$order_value,
  probs = c(
    0.25,
    0.50,
    0.75
  ),
  na.rm = TRUE,
  names = FALSE,
  type = 7
)

order_value_q25 <- as.numeric(order_value_thresholds[[1]])
order_value_q50 <- as.numeric(order_value_thresholds[[2]])
order_value_q75 <- as.numeric(order_value_thresholds[[3]])

stopifnot(
  is.finite(order_value_q25),
  is.finite(order_value_q50),
  is.finite(order_value_q75),
  order_value_q25 <= order_value_q50,
  order_value_q50 <= order_value_q75
)

impact_scoring_parameters <- tibble(
  parameter = c(
    "order_value_training_q25",
    "order_value_training_q50",
    "order_value_training_q75",
    "criticality_weight",
    "approved_supplier_weight",
    "sourcing_allocation_weight",
    "order_value_weight"
  ),
  value = c(
    order_value_q25,
    order_value_q50,
    order_value_q75,
    35,
    25,
    20,
    20
  ),
  description = c(
    "Training-period 25th percentile used for order-value scoring",
    "Training-period median used for order-value scoring",
    "Training-period 75th percentile used for order-value scoring",
    "Maximum points assigned to material criticality",
    "Maximum points assigned to limited approved-supplier availability",
    "Maximum points assigned to sourcing concentration",
    "Maximum points assigned to order value"
  )
)

# -------------------------------------------------------------------------
# Define the fixed operational-impact rubric
# -------------------------------------------------------------------------
#
# Total possible score: 100 points
#
# Fixed impact tiers:
#   Low       = below 40
#   Moderate  = 40 to below 60
#   High      = 60 to below 80
#   Very High = 80 to 100
#
# Material criticality: 35 points
#   Low    = 0.0
#   Medium = 17.5
#   High   = 35.0
#
# Approved-supplier availability: 25 points
#   1 approved supplier  = 25.0
#   2 approved suppliers = 15.0
#   3 approved suppliers = 7.5
#   4 or more            = 0.0
#
# Sourcing allocation: 20 points
#   Points equal allocation share multiplied by 20.
#
# Order value: 20 points
#   At or below training Q1 = 0
#   Q1 to median            = 7
#   Median to Q3            = 14
#   Above Q3                = 20

impact_rubric <- tibble(
  component = c(
    "Material criticality",
    "Material criticality",
    "Material criticality",
    "Approved suppliers",
    "Approved suppliers",
    "Approved suppliers",
    "Approved suppliers",
    "Sourcing allocation",
    "Order value",
    "Order value",
    "Order value",
    "Order value"
  ),
  rule = c(
    "Low",
    "Medium",
    "High",
    "1 approved supplier",
    "2 approved suppliers",
    "3 approved suppliers",
    "4 or more approved suppliers",
    "Sourcing allocation multiplied by 20",
    "At or below training-period Q1",
    "Above Q1 through training-period median",
    "Above median through training-period Q3",
    "Above training-period Q3"
  ),
  points = c(
    0,
    17.5,
    35,
    25,
    15,
    7.5,
    0,
    NA_real_,
    0,
    7,
    14,
    20
  ),
  maximum_component_points = c(
    rep(35, 3),
    rep(25, 4),
    20,
    rep(20, 4)
  )
)

# -------------------------------------------------------------------------
# Score validation-period operational impact
# -------------------------------------------------------------------------

validation_operational_impact <- validation_data %>%
  mutate(
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
      pmin(
        pmax(sourcing_allocation, 0),
        1
      ) * 20,

    order_value_band = case_when(
      order_value <= order_value_q25 ~ "At or below training Q1",
      order_value <= order_value_q50 ~ "Above Q1 through training median",
      order_value <= order_value_q75 ~ "Above median through training Q3",
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
      levels = c(
        "Low",
        "Moderate",
        "High",
        "Very High"
      ),
      ordered = TRUE
    )
  ) %>%
  select(
    scoring_record_id,
    scoring_date,
    purchase_order_line_id,
    purchase_order_id,
    supplier_material_id,
    supplier_id,
    supplier_name,
    supplier_region,
    supplier_tier,
    material_id,
    material_name,
    material_category,
    material_criticality,
    material_criticality_normalized,
    promised_delivery_date,
    ordered_quantity,
    unit_price,
    order_value,
    approved_supplier_count,
    sourcing_allocation,
    material_criticality_points,
    approved_supplier_points,
    sourcing_allocation_points,
    order_value_band,
    order_value_points,
    operational_impact_score,
    operational_impact_tier,
    late_delivery_target
  )

# -------------------------------------------------------------------------
# Validate impact scores
# -------------------------------------------------------------------------

stopifnot(
  nrow(validation_operational_impact) == nrow(validation_data),
  !anyDuplicated(validation_operational_impact$scoring_record_id),
  !anyNA(validation_operational_impact$operational_impact_score),
  !anyNA(validation_operational_impact$operational_impact_tier),
  all(
    validation_operational_impact$material_criticality_points >= 0 &
      validation_operational_impact$material_criticality_points <= 35
  ),
  all(
    validation_operational_impact$approved_supplier_points >= 0 &
      validation_operational_impact$approved_supplier_points <= 25
  ),
  all(
    validation_operational_impact$sourcing_allocation_points >= 0 &
      validation_operational_impact$sourcing_allocation_points <= 20
  ),
  all(
    validation_operational_impact$order_value_points >= 0 &
      validation_operational_impact$order_value_points <= 20
  ),
  all(
    validation_operational_impact$operational_impact_score >= 0 &
      validation_operational_impact$operational_impact_score <= 100
  )
)

# -------------------------------------------------------------------------
# Summarize validation impact distribution
# -------------------------------------------------------------------------

impact_tier_summary <- validation_operational_impact %>%
  count(
    operational_impact_tier,
    name = "orders",
    .drop = FALSE
  ) %>%
  mutate(
    share_of_orders =
      orders /
      sum(orders)
  )

impact_component_summary <- validation_operational_impact %>%
  summarize(
    average_material_criticality_points = mean(
      material_criticality_points
    ),
    average_approved_supplier_points = mean(
      approved_supplier_points
    ),
    average_sourcing_allocation_points = mean(
      sourcing_allocation_points
    ),
    average_order_value_points = mean(
      order_value_points
    ),
    average_operational_impact_score = mean(
      operational_impact_score
    ),
    median_operational_impact_score = median(
      operational_impact_score
    ),
    minimum_operational_impact_score = min(
      operational_impact_score
    ),
    maximum_operational_impact_score = max(
      operational_impact_score
    )
  )

# -------------------------------------------------------------------------
# Export validation impact outputs
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
  validation_operational_impact,
  file.path(
    "data",
    "processed",
    "validation_operational_impact_scores.csv"
  ),
  na = ""
)

write_csv(
  impact_scoring_parameters,
  file.path(
    "data",
    "processed",
    "operational_impact_scoring_parameters.csv"
  ),
  na = ""
)

write_csv(
  impact_rubric,
  file.path(
    "data",
    "processed",
    "operational_impact_rubric.csv"
  ),
  na = ""
)

write_csv(
  impact_tier_summary,
  file.path(
    "data",
    "processed",
    "validation_operational_impact_tier_summary.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print results
# -------------------------------------------------------------------------

cat("\nVALIDATION OPERATIONAL-IMPACT SCORING\n")
cat(
  "\nValidation orders scored:",
  format(nrow(validation_operational_impact), big.mark = ","),
  "\n"
)
cat(
  "Training order-value Q1:",
  scales::dollar(order_value_q25),
  "\n"
)
cat(
  "Training order-value median:",
  scales::dollar(order_value_q50),
  "\n"
)
cat(
  "Training order-value Q3:",
  scales::dollar(order_value_q75),
  "\n"
)

cat("\nIMPACT-TIER DISTRIBUTION\n")
print(
  impact_tier_summary,
  n = Inf,
  width = Inf
)

cat("\nIMPACT-COMPONENT SUMMARY\n")
print(
  impact_component_summary,
  width = Inf
)

cat("\nHIGHEST-IMPACT VALIDATION ORDERS\n")
print(
  validation_operational_impact %>%
    arrange(
      desc(operational_impact_score),
      desc(order_value),
      scoring_record_id
    ) %>%
    select(
      scoring_record_id,
      supplier_name,
      material_name,
      material_criticality_normalized,
      approved_supplier_count,
      sourcing_allocation,
      order_value,
      operational_impact_score,
      operational_impact_tier
    ) %>%
    slice_head(n = 15),
  n = Inf,
  width = Inf
)

message(
  "Validation operational-impact scoring completed successfully."
)
message(
  "Late-delivery probability was not used in the impact score."
)
message(
  "The final test dataset was not loaded or evaluated."
)
message(
  paste(
    "The impact rubric uses fixed 40, 60, and 80-point tier boundaries",
    "and must be reviewed and locked before final test evaluation."
  )
)
