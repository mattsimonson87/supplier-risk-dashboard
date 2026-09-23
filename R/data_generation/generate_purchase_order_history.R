# Generate Purchase-Order and Delivery History
#
# Creates fictional purchase-order lines and delivery outcomes for the
# Supplier Risk Intelligence project.
#
# Delivery risk is generated from a combination of:
#
#   - Persistent hidden supplier reliability
#   - Persistent hidden supplier-material effects
#   - Order size relative to normal
#   - Supplier workload visible when the order is created
#   - Compressed promised lead time
#   - Promised-delivery seasonality
#   - Random outcome variation
#
# Hidden simulation parameters are used only to generate synthetic outcomes.
# They are not exported, provided to predictive models, or displayed in the
# application.
#
# No employer, client, or prospective-employer data is used.

# -------------------------------------------------------------------------
# Regenerate prerequisite data
# -------------------------------------------------------------------------

source(
  "R/data_generation/generate_supplier_master.R"
)

source(
  "R/data_generation/generate_material_master.R"
)

source(
  "R/data_generation/generate_supplier_material_relationships.R"
)

# Each prerequisite generator has its own reproducibility seed.
# Set the purchase-order seed after sourcing those scripts.
set.seed(422)

# -------------------------------------------------------------------------
# Validate prerequisite objects
# -------------------------------------------------------------------------

required_objects <- c(
  "supplier_master",
  "supplier_latent_parameters",
  "material_master",
  "supplier_material_relationships",
  "relationship_latent_parameters"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1)
  )
]

if (length(missing_objects) > 0L) {
  stop(
    "Required generator objects are missing: ",
    paste(
      missing_objects,
      collapse = ", "
    )
  )
}

history_start_date <- as.Date(
  "2023-01-01"
)

history_end_date <- as.Date(
  "2026-08-31"
)

# -------------------------------------------------------------------------
# Prepare active supplier-material relationships
# -------------------------------------------------------------------------

active_relationships <-
  supplier_material_relationships[
    supplier_material_relationships$
      relationship_status == "Active",
  ]

active_materials <-
  material_master[
    material_master$active_flag,
  ]

# Add observable material demand.
relationship_inputs <- merge(
  active_relationships,

  active_materials[
    c(
      "material_id",
      "average_daily_demand"
    )
  ],

  by = "material_id",
  all.x = TRUE,
  sort = FALSE
)

# Add hidden supplier reliability.
relationship_inputs <- merge(
  relationship_inputs,
  supplier_latent_parameters,
  by = "supplier_id",
  all.x = TRUE,
  sort = FALSE
)

# Add hidden supplier-material effects.
relationship_inputs <- merge(
  relationship_inputs,
  relationship_latent_parameters,
  by = "supplier_material_id",
  all.x = TRUE,
  sort = FALSE
)

stopifnot(
  !anyNA(
    relationship_inputs$average_daily_demand
  ),

  !anyNA(
    relationship_inputs$latent_reliability
  ),

  !anyNA(
    relationship_inputs$
      latent_relationship_effect
  )
)

# -------------------------------------------------------------------------
# Generate purchase orders for one supplier-material relationship
# -------------------------------------------------------------------------

generate_relationship_orders <- function(
  relationship_row
) {
  relationship_start <- as.Date(
    relationship_row$
      relationship_start_date
  )

  first_possible_order_date <- max(
    history_start_date,
    relationship_start
  )

  if (
    first_possible_order_date >
      history_end_date
  ) {
    return(NULL)
  }

  allocated_daily_demand <-
    relationship_row$
      average_daily_demand *
    relationship_row$
      sourcing_allocation

  # Approximate the number of days represented by one typical order.
  expected_order_interval <- round(
    relationship_row$
      standard_order_quantity /
      max(
        allocated_daily_demand,
        0.1
      )
  )

  # Keep ordering frequency within a practical range.
  expected_order_interval <- max(
    7L,
    min(
      expected_order_interval,
      90L
    )
  )

  order_dates <- as.Date(
    character()
  )

  next_order_date <-
    first_possible_order_date +
    sample(
      0:expected_order_interval,
      size = 1
    )

  while (
    next_order_date <= history_end_date
  ) {
    order_dates <- c(
      order_dates,
      next_order_date
    )

    interval_variation <- round(
      rnorm(
        n = 1,
        mean = expected_order_interval,
        sd = max(
          2,
          expected_order_interval *
            0.20
        )
      )
    )

    interval_variation <- max(
      5L,
      interval_variation
    )

    next_order_date <-
      next_order_date +
      interval_variation
  }

  if (length(order_dates) == 0L) {
    return(NULL)
  }

  order_count <- length(
    order_dates
  )

  # Generate order quantities around the relationship's typical quantity.
  order_size_multiplier <- pmax(
    0.60,

    rlnorm(
      n = order_count,
      meanlog = 0,
      sdlog = 0.25
    )
  )

  ordered_quantity <- round(
    relationship_row$
      standard_order_quantity *
      order_size_multiplier
  )

  ordered_quantity <- pmax(
    ordered_quantity,
    relationship_row$
      minimum_order_quantity
  )

  # Create variation between the normal quoted lead time and the lead time
  # promised for an individual order.
  #
  # Negative adjustments represent compressed or expedited commitments.
  # Positive adjustments give the supplier additional time.
  lead_time_adjustment <- sample(
    c(
      -15L,
      -10L,
      -5L,
      0L,
      5L,
      10L
    ),

    size = order_count,
    replace = TRUE,

    prob = c(
      0.05,
      0.10,
      0.20,
      0.40,
      0.15,
      0.10
    )
  )

  planned_lead_time_days <- pmax(
    5L,

    relationship_row$
      quoted_lead_time_days +
      lead_time_adjustment
  )

  promised_delivery_date <-
    order_dates +
    planned_lead_time_days

  data.frame(
    supplier_material_id =
      relationship_row$
        supplier_material_id,

    supplier_id =
      relationship_row$
        supplier_id,

    material_id =
      relationship_row$
        material_id,

    order_date =
      order_dates,

    promised_delivery_date =
      promised_delivery_date,

    planned_lead_time_days =
      as.integer(
        planned_lead_time_days
      ),

    ordered_quantity =
      as.integer(
        ordered_quantity
      ),

    standard_order_quantity =
      relationship_row$
        standard_order_quantity,

    quoted_lead_time_days =
      relationship_row$
        quoted_lead_time_days,

    unit_price =
      relationship_row$
        unit_price,

    latent_reliability =
      relationship_row$
        latent_reliability,

    latent_relationship_effect =
      relationship_row$
        latent_relationship_effect,

    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Generate all purchase-order rows
# -------------------------------------------------------------------------

purchase_order_rows <- lapply(
  seq_len(
    nrow(
      relationship_inputs
    )
  ),

  function(row_index) {
    generate_relationship_orders(
      relationship_inputs[
        row_index,
      ]
    )
  }
)

purchase_order_rows <- purchase_order_rows[
  !vapply(
    purchase_order_rows,
    is.null,
    logical(1)
  )
]

purchase_order_history <- do.call(
  rbind,
  purchase_order_rows
)

row.names(
  purchase_order_history
) <- NULL

stopifnot(
  nrow(purchase_order_history) > 0L,

  !anyNA(
    purchase_order_history$order_date
  ),

  !anyNA(
    purchase_order_history$
      promised_delivery_date
  ),

  all(
    purchase_order_history$
      planned_lead_time_days >
      0L
  )
)

# -------------------------------------------------------------------------
# Calculate supplier workload visible when each order is created
# -------------------------------------------------------------------------
#
# Workload uses only purchase orders placed during the 90 days ending on the
# current order date.
#
# Orders placed later in the same month are not included.
#
# Orders placed on the same date are treated as visible because the proof of
# concept assumes end-of-day scoring.

purchase_order_history <-
  purchase_order_history[
    order(
      purchase_order_history$
        supplier_id,

      purchase_order_history$
        order_date,

      purchase_order_history$
        supplier_material_id
    ),
  ]

row.names(
  purchase_order_history
) <- NULL

purchase_order_history$
  supplier_recent_order_count_90d <- 0L

purchase_order_history$
  supplier_recent_order_quantity_90d <- 0

supplier_order_indexes <- split(
  seq_len(
    nrow(
      purchase_order_history
    )
  ),

  purchase_order_history$supplier_id
)

for (
  supplier_index in
  supplier_order_indexes
) {
  supplier_dates <-
    purchase_order_history$
      order_date[
        supplier_index
      ]

  supplier_quantities <-
    purchase_order_history$
      ordered_quantity[
        supplier_index
      ]

  unique_supplier_dates <- sort(
    unique(
      supplier_dates
    )
  )

  for (
    current_date in
    unique_supplier_dates
  ) {
    visible_order_mask <-
      supplier_dates <= current_date &
      supplier_dates >
        current_date - 90L

    current_date_mask <-
      supplier_dates == current_date

    current_date_indexes <-
      supplier_index[
        current_date_mask
      ]

    purchase_order_history$
      supplier_recent_order_count_90d[
        current_date_indexes
      ] <- sum(
        visible_order_mask
      )

    purchase_order_history$
      supplier_recent_order_quantity_90d[
        current_date_indexes
      ] <- sum(
        supplier_quantities[
          visible_order_mask
        ]
      )
  }
}

stopifnot(
  all(
    purchase_order_history$
      supplier_recent_order_count_90d >=
      1L
  ),

  all(
    purchase_order_history$
      supplier_recent_order_quantity_90d >
      0
  )
)

# Convert recent volume into workload pressure.
#
# Risk begins increasing after 8,000 units have been ordered from the
# supplier during the preceding 90 days.
purchase_order_history$
  workload_pressure <- pmax(
    purchase_order_history$
      supplier_recent_order_quantity_90d /
      8000 -
      1,

    0
  )

# Cap the generator-only pressure measure to prevent a small number of
# extremely large suppliers from dominating the simulated outcome.
purchase_order_history$
  workload_pressure_capped <- pmin(
    purchase_order_history$
      workload_pressure,

    2
  )

# -------------------------------------------------------------------------
# Calculate observable current-order pressure
# -------------------------------------------------------------------------

purchase_order_history$
  order_size_ratio <-
  purchase_order_history$
    ordered_quantity /
  purchase_order_history$
    standard_order_quantity

# Only above-normal order size adds pressure.
purchase_order_history$
  order_size_pressure <- pmax(
    purchase_order_history$
      order_size_ratio -
      1,

    0
  )

# A positive value means the promised lead time is shorter than the normal
# quoted lead time.
purchase_order_history$
  lead_time_pressure_days <- pmax(
    purchase_order_history$
      quoted_lead_time_days -
      purchase_order_history$
        planned_lead_time_days,

    0
  )

# -------------------------------------------------------------------------
# Calculate smooth promised-delivery seasonality
# -------------------------------------------------------------------------
#
# Seasonal pressure is greatest around January and lowest around July.
#
# The smooth cyclical pattern aligns with the sine and cosine month features
# later supplied to the predictive models.

promised_month <- as.integer(
  format(
    purchase_order_history$
      promised_delivery_date,

    "%m"
  )
)

purchase_order_history$
  seasonal_pressure <-
  (
    cos(
      2 *
        pi *
        (
          promised_month -
            1
        ) /
        12
    ) +
      1
  ) /
  2

stopifnot(
  all(
    purchase_order_history$
      seasonal_pressure >=
      0
  ),

  all(
    purchase_order_history$
      seasonal_pressure <=
      1
  )
)

# -------------------------------------------------------------------------
# Create the systematic late-delivery risk
# -------------------------------------------------------------------------

# Lower supplier reliability produces greater hidden supplier risk.
supplier_risk_effect <- qlogis(
  1 -
    pmin(
      pmax(
        purchase_order_history$
          latent_reliability,

        0.01
      ),

      0.99
    )
)

# Center hidden supplier risk so the calibrated intercept controls the
# overall target rate.
supplier_risk_effect <-
  supplier_risk_effect -
  mean(
    supplier_risk_effect
  )

# Build a systematic risk score without an intercept.
#
# Observable order pressure receives more weight than in generator version 1
# so the final business data contain meaningful, recoverable predictive
# signal.
systematic_late_risk <-
  1.10 *
    supplier_risk_effect +
  0.75 *
    purchase_order_history$
      latent_relationship_effect +
  1.20 *
    purchase_order_history$
      order_size_pressure +
  0.70 *
    purchase_order_history$
      workload_pressure_capped +
  0.07 *
    purchase_order_history$
      lead_time_pressure_days +
  0.65 *
    purchase_order_history$
      seasonal_pressure

stopifnot(
  !anyNA(
    systematic_late_risk
  ),

  all(
    is.finite(
      systematic_late_risk
    )
  )
)

# -------------------------------------------------------------------------
# Calibrate the probability intercept
# -------------------------------------------------------------------------
#
# Strengthening systematic relationships should increase separation between
# lower-risk and higher-risk orders without increasing overall prevalence.
#
# The intercept is selected so the average latent probability remains near
# 12 percent.

target_late_rate <- 0.12

calibration_function <- function(
  intercept
) {
  mean(
    plogis(
      intercept +
        systematic_late_risk
    )
  ) -
    target_late_rate
}

calibrated_intercept <- uniroot(
  calibration_function,

  interval = c(
    -10,
    5
  )
)$root

late_log_odds <-
  calibrated_intercept +
  systematic_late_risk

purchase_order_history$
  latent_late_probability <- plogis(
    late_log_odds
  )

# Prevent effectively certain outcomes while preserving meaningful risk
# differentiation.
purchase_order_history$
  latent_late_probability <- pmin(
    pmax(
      purchase_order_history$
        latent_late_probability,

      0.01
    ),

    0.80
  )

stopifnot(
  all(
    purchase_order_history$
      latent_late_probability >=
      0
  ),

  all(
    purchase_order_history$
      latent_late_probability <=
      1
  )
)

# -------------------------------------------------------------------------
# Generate late-delivery outcomes
# -------------------------------------------------------------------------

generated_late_event <- rbinom(
  n = nrow(
    purchase_order_history
  ),

  size = 1,

  prob =
    purchase_order_history$
      latent_late_probability
)

# Non-event orders may arrive early or no more than seven days late.
#
# Event orders arrive between eight and thirty days late.
purchase_order_history$
  late_days <- ifelse(
    generated_late_event == 1L,

    sample(
      8:30,

      size = nrow(
        purchase_order_history
      ),

      replace = TRUE,

      prob = rev(
        seq_len(23)
      )
    ),

    sample(
      -5:7,

      size = nrow(
        purchase_order_history
      ),

      replace = TRUE,

      prob = c(
        0.03,
        0.04,
        0.05,
        0.07,
        0.09,
        0.16,
        0.16,
        0.14,
        0.10,
        0.07,
        0.04,
        0.03,
        0.02
      )
    )
  )

purchase_order_history$
  late_days <- as.integer(
    purchase_order_history$
      late_days
  )

purchase_order_history$
  actual_delivery_date <-
  purchase_order_history$
    promised_delivery_date +
  purchase_order_history$
    late_days

purchase_order_history$
  late_delivery_flag <- as.integer(
    purchase_order_history$
      late_days >
      7L
  )

purchase_order_history$
  order_value <- round(
    purchase_order_history$
      ordered_quantity *
      purchase_order_history$
        unit_price,

    digits = 2
  )

# -------------------------------------------------------------------------
# Restore chronological order and create identifiers
# -------------------------------------------------------------------------

purchase_order_history <-
  purchase_order_history[
    order(
      purchase_order_history$
        order_date,

      purchase_order_history$
        supplier_id,

      purchase_order_history$
        material_id
    ),
  ]

row.names(
  purchase_order_history
) <- NULL

purchase_order_history$
  purchase_order_line_id <- sprintf(
    "POL-%06d",

    seq_len(
      nrow(
        purchase_order_history
      )
    )
  )

# In version one of the business process, each purchase order has one line.
purchase_order_history$
  purchase_order_id <- sprintf(
    "PO-%06d",

    seq_len(
      nrow(
        purchase_order_history
      )
    )
  )

# -------------------------------------------------------------------------
# Create observable export
# -------------------------------------------------------------------------
#
# Hidden supplier reliability, supplier-material effects, generated
# probabilities, and generator-only pressure fields are intentionally
# excluded.

purchase_order_export <-
  purchase_order_history[
    c(
      "purchase_order_line_id",
      "purchase_order_id",
      "supplier_material_id",
      "supplier_id",
      "material_id",
      "order_date",
      "promised_delivery_date",
      "actual_delivery_date",
      "ordered_quantity",
      "unit_price",
      "order_value",
      "late_days",
      "late_delivery_flag"
    )
  ]

# -------------------------------------------------------------------------
# Validate observable purchase-order history
# -------------------------------------------------------------------------

stopifnot(
  nrow(purchase_order_export) > 0L,

  !anyDuplicated(
    purchase_order_export$
      purchase_order_line_id
  ),

  !anyDuplicated(
    purchase_order_export$
      purchase_order_id
  ),

  !anyNA(
    purchase_order_export$
      promised_delivery_date
  ),

  !anyNA(
    purchase_order_export$
      actual_delivery_date
  ),

  all(
    purchase_order_export$
      ordered_quantity >
      0
  ),

  all(
    purchase_order_export$
      unit_price >
      0
  ),

  all(
    purchase_order_export$
      order_value >
      0
  ),

  all(
    purchase_order_export$
      promised_delivery_date >=
      purchase_order_export$
        order_date
  ),

  all(
    purchase_order_export$
      actual_delivery_date ==
      purchase_order_export$
        promised_delivery_date +
      purchase_order_export$
        late_days
  ),

  all(
    purchase_order_export$
      late_delivery_flag ==
      as.integer(
        purchase_order_export$
          late_days >
          7L
      )
  )
)

# -------------------------------------------------------------------------
# Export observable data
# -------------------------------------------------------------------------

dir.create(
  file.path(
    "data",
    "raw"
  ),

  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  purchase_order_export,

  file = file.path(
    "data",
    "raw",
    "purchase_order_history.csv"
  ),

  row.names = FALSE,
  na = ""
)

# -------------------------------------------------------------------------
# Generator diagnostics
# -------------------------------------------------------------------------

late_delivery_rate <- mean(
  purchase_order_export$
    late_delivery_flag
)

mean_latent_probability <- mean(
  purchase_order_history$
    latent_late_probability
)

latent_probability_percentiles <- quantile(
  purchase_order_history$
    latent_late_probability,

  probs = c(
    0,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    1
  )
)

oracle_data <- data.frame(
  late_delivery_target = factor(
    purchase_order_history$
      late_delivery_flag,

    levels = c(
      0,
      1
    ),

    labels = c(
      "on_time",
      "late"
    )
  ),

  latent_late_probability =
    purchase_order_history$
      latent_late_probability
)

oracle_roc_auc <- yardstick::roc_auc(
  oracle_data,

  truth =
    late_delivery_target,

  latent_late_probability,

  event_level =
    "second"
)$.estimate[[1]]

oracle_pr_auc <- yardstick::pr_auc(
  oracle_data,

  truth =
    late_delivery_target,

  latent_late_probability,

  event_level =
    "second"
)$.estimate[[1]]

oracle_deciles <- purchase_order_history |>
  dplyr::mutate(
    oracle_risk_decile =
      dplyr::ntile(
        latent_late_probability,
        10
      )
  ) |>
  dplyr::group_by(
    oracle_risk_decile
  ) |>
  dplyr::summarize(
    orders =
      dplyr::n(),

    average_generated_probability =
      mean(
        latent_late_probability
      ),

    actual_late_rate =
      mean(
        late_delivery_flag
      ),

    .groups =
      "drop"
  )

# -------------------------------------------------------------------------
# Print generator results
# -------------------------------------------------------------------------

message(
  "Created purchase_order_history.csv with ",
  format(
    nrow(
      purchase_order_export
    ),
    big.mark = ","
  ),
  " fictional purchase-order lines."
)

message(
  "Generated late-delivery rate: ",
  scales::percent(
    late_delivery_rate,
    accuracy = 0.1
  )
)

message(
  "Mean latent late probability: ",
  scales::percent(
    mean_latent_probability,
    accuracy = 0.1
  )
)

message(
  "Calibrated probability target: ",
  scales::percent(
    target_late_rate,
    accuracy = 0.1
  )
)

message(
  "Calibrated intercept: ",
  round(
    calibrated_intercept,
    digits = 3
  )
)

message(
  "Oracle ROC AUC: ",
  round(
    oracle_roc_auc,
    digits = 3
  )
)

message(
  "Oracle PR AUC: ",
  round(
    oracle_pr_auc,
    digits = 3
  )
)

message(
  "Hidden simulation fields were not exported."
)

cat(
  "\nLATENT PROBABILITY PERCENTILES\n"
)

print(
  latent_probability_percentiles
)

cat(
  "\nORACLE RISK DECILES\n"
)

print(
  oracle_deciles,
  n = Inf,
  width = 120
)

cat(
  "\nPURCHASE-ORDER EXPORT PREVIEW\n"
)

print(
  head(
    purchase_order_export,
    12
  )
)