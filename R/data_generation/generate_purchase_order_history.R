# Generate Purchase-Order and Delivery History
#
# Creates fictional purchase-order lines and delivery outcomes for the
# Supplier Risk Intelligence project.
#
# No employer, client, or prospective-employer data is used.

# Regenerate prerequisite datasets and hidden simulation parameters.
source("R/data_generation/generate_supplier_master.R")
source("R/data_generation/generate_material_master.R")
source("R/data_generation/generate_supplier_material_relationships.R")

# Set the purchase-order seed after the prerequisite scripts because each
# prerequisite generator sets its own reproducibility seed.
set.seed(422)

# Confirm that required objects exist.
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
    paste(missing_objects, collapse = ", ")
  )
}

history_start_date <- as.Date("2023-01-01")
history_end_date <- as.Date("2026-08-31")

active_relationships <- supplier_material_relationships[
  supplier_material_relationships$relationship_status == "Active",
]

active_materials <- material_master[
  material_master$active_flag,
]

# Add the material's average daily demand.
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

# Add hidden supplier-level reliability.
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
  !anyNA(relationship_inputs$average_daily_demand),
  !anyNA(relationship_inputs$latent_reliability),
  !anyNA(relationship_inputs$latent_relationship_effect)
)

# Generate order dates for one supplier-material relationship.
generate_relationship_orders <- function(relationship_row) {
  relationship_start <- as.Date(
    relationship_row$relationship_start_date
  )

  first_possible_order_date <- max(
    history_start_date,
    relationship_start
  )

  if (first_possible_order_date > history_end_date) {
    return(NULL)
  }

  allocated_daily_demand <-
    relationship_row$average_daily_demand *
    relationship_row$sourcing_allocation

  # Approximate the number of days represented by one standard order.
  expected_order_interval <- round(
    relationship_row$standard_order_quantity /
      max(allocated_daily_demand, 0.1)
  )

  # Keep order frequency within a reasonable range.
  expected_order_interval <- max(
    7L,
    min(
      expected_order_interval,
      90L
    )
  )

  order_dates <- as.Date(character())

  next_order_date <- first_possible_order_date +
    sample(
      0:expected_order_interval,
      size = 1
    )

  while (next_order_date <= history_end_date) {
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
          expected_order_interval * 0.20
        )
      )
    )

    interval_variation <- max(
      5L,
      interval_variation
    )

    next_order_date <- next_order_date +
      interval_variation
  }

  if (length(order_dates) == 0L) {
    return(NULL)
  }

  order_count <- length(order_dates)

  order_size_multiplier <- pmax(
    0.60,
    rlnorm(
      order_count,
      meanlog = 0,
      sdlog = 0.25
    )
  )

  ordered_quantity <- round(
    relationship_row$standard_order_quantity *
      order_size_multiplier
  )

  ordered_quantity <- pmax(
    ordered_quantity,
    relationship_row$minimum_order_quantity
  )

  promised_delivery_date <- order_dates +
    relationship_row$quoted_lead_time_days

  data.frame(
    supplier_material_id =
      relationship_row$supplier_material_id,
    supplier_id =
      relationship_row$supplier_id,
    material_id =
      relationship_row$material_id,
    order_date = order_dates,
    promised_delivery_date =
      promised_delivery_date,
    ordered_quantity =
      as.integer(ordered_quantity),
    standard_order_quantity =
      relationship_row$standard_order_quantity,
    quoted_lead_time_days =
      relationship_row$quoted_lead_time_days,
    unit_price =
      relationship_row$unit_price,
    latent_reliability =
      relationship_row$latent_reliability,
    latent_relationship_effect =
      relationship_row$latent_relationship_effect,
    stringsAsFactors = FALSE
  )
}

purchase_order_rows <- lapply(
  seq_len(nrow(relationship_inputs)),
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

row.names(purchase_order_history) <- NULL

# Create a supplier-month identifier for observable order-volume pressure.
purchase_order_history$order_month <- as.Date(
  format(
    purchase_order_history$order_date,
    "%Y-%m-01"
  )
)

supplier_month_volume <- aggregate(
  ordered_quantity ~ supplier_id + order_month,
  data = purchase_order_history,
  FUN = sum
)

names(supplier_month_volume)[
  names(supplier_month_volume) == "ordered_quantity"
] <- "supplier_month_order_volume"

supplier_average_volume <- aggregate(
  supplier_month_order_volume ~ supplier_id,
  data = supplier_month_volume,
  FUN = mean
)

names(supplier_average_volume)[
  names(supplier_average_volume) ==
    "supplier_month_order_volume"
] <- "supplier_average_monthly_volume"

supplier_month_volume <- merge(
  supplier_month_volume,
  supplier_average_volume,
  by = "supplier_id",
  all.x = TRUE,
  sort = FALSE
)

supplier_month_volume$volume_pressure_ratio <-
  supplier_month_volume$supplier_month_order_volume /
  supplier_month_volume$supplier_average_monthly_volume

purchase_order_history <- merge(
  purchase_order_history,
  supplier_month_volume[
    c(
      "supplier_id",
      "order_month",
      "supplier_month_order_volume",
      "volume_pressure_ratio"
    )
  ],
  by = c(
    "supplier_id",
    "order_month"
  ),
  all.x = TRUE,
  sort = FALSE
)

# Measure each order relative to its normal relationship-level order size.
purchase_order_history$order_size_ratio <-
  purchase_order_history$ordered_quantity /
  purchase_order_history$standard_order_quantity

# Create a modest seasonal effect.
#
# Deliveries promised during November, December, January, or February receive
# slightly greater synthetic delay pressure.
promised_month <- as.integer(
  format(
    purchase_order_history$promised_delivery_date,
    "%m"
  )
)

purchase_order_history$seasonal_pressure <- ifelse(
  promised_month %in% c(
    11L,
    12L,
    1L,
    2L
  ),
  1,
  0
)

# Convert latent reliability into a hidden supplier risk effect.
#
# Lower reliability produces a larger positive lateness effect.
supplier_risk_effect <- qlogis(
  1 - pmin(
    pmax(
      purchase_order_history$latent_reliability,
      0.01
    ),
    0.99
  )
)

# Create the latent probability of a materially late delivery.
late_log_odds <-
  -0.50 +
  supplier_risk_effect +
  purchase_order_history$latent_relationship_effect +
  0.65 *
    pmax(
      purchase_order_history$order_size_ratio - 1,
      0
    ) +
  0.55 *
    pmax(
      purchase_order_history$volume_pressure_ratio - 1,
      0
    ) +
  0.35 *
    purchase_order_history$seasonal_pressure

purchase_order_history$latent_late_probability <- plogis(
  late_log_odds
)

purchase_order_history$latent_late_probability <- pmin(
  pmax(
    purchase_order_history$latent_late_probability,
    0.02
  ),
  0.75
)

# Generate whether each order experiences a material delay.
generated_late_event <- rbinom(
  n = nrow(purchase_order_history),
  size = 1,
  prob = purchase_order_history$latent_late_probability
)

# Generate delivery timing.
#
# Non-event orders may arrive early or up to seven days late.
# Event orders arrive between eight and thirty days late.
purchase_order_history$late_days <- ifelse(
  generated_late_event == 1L,
  sample(
    8:30,
    size = nrow(purchase_order_history),
    replace = TRUE,
    prob = rev(
      seq_len(23)
    )
  ),
  sample(
    -5:7,
    size = nrow(purchase_order_history),
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

purchase_order_history$late_days <- as.integer(
  purchase_order_history$late_days
)

purchase_order_history$actual_delivery_date <-
  purchase_order_history$promised_delivery_date +
  purchase_order_history$late_days

purchase_order_history$late_delivery_flag <- as.integer(
  purchase_order_history$late_days > 7L
)

purchase_order_history$order_value <- round(
  purchase_order_history$ordered_quantity *
    purchase_order_history$unit_price,
  digits = 2
)

# Restore chronological order before creating identifiers.
purchase_order_history <- purchase_order_history[
  order(
    purchase_order_history$order_date,
    purchase_order_history$supplier_id,
    purchase_order_history$material_id
  ),
]

row.names(purchase_order_history) <- NULL

purchase_order_history$purchase_order_line_id <- sprintf(
  "POL-%06d",
  seq_len(nrow(purchase_order_history))
)

# For version one, each synthetic purchase order contains one line.
purchase_order_history$purchase_order_id <- sprintf(
  "PO-%06d",
  seq_len(nrow(purchase_order_history))
)

# Create the observable export.
#
# Hidden simulation parameters are intentionally excluded.
purchase_order_export <- purchase_order_history[
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

# Validation checks
stopifnot(
  nrow(purchase_order_export) > 0,
  !anyDuplicated(
    purchase_order_export$purchase_order_line_id
  ),
  !anyDuplicated(
    purchase_order_export$purchase_order_id
  ),
  !anyNA(
    purchase_order_export$promised_delivery_date
  ),
  !anyNA(
    purchase_order_export$actual_delivery_date
  ),
  all(
    purchase_order_export$ordered_quantity > 0
  ),
  all(
    purchase_order_export$unit_price > 0
  ),
  all(
    purchase_order_export$order_value > 0
  ),
  all(
    purchase_order_export$promised_delivery_date >=
      purchase_order_export$order_date
  ),
  all(
    purchase_order_export$actual_delivery_date ==
      purchase_order_export$promised_delivery_date +
        purchase_order_export$late_days
  ),
  all(
    purchase_order_export$late_delivery_flag ==
      as.integer(
        purchase_order_export$late_days > 7L
      )
  )
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

late_delivery_rate <- mean(
  purchase_order_export$late_delivery_flag
)

message(
  "Created purchase_order_history.csv with ",
  format(
    nrow(purchase_order_export),
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
  "Hidden simulation fields were not exported."
)

print(
  head(
    purchase_order_export,
    12
  )
)
