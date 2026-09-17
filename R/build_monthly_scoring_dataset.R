# Build Monthly Supplier-Material Scoring Dataset
#
# Creates one row per eligible supplier-material relationship and monthly
# scoring date.
#
# Predictor features use only information available on or before the scoring
# date. The target uses deliveries promised during the following 30 days.
#
# No employer, client, or prospective-employer data is used.

purchase_order_file <- file.path(
  "data",
  "raw",
  "purchase_order_history.csv"
)

relationship_file <- file.path(
  "data",
  "raw",
  "supplier_material_relationships.csv"
)

supplier_file <- file.path(
  "data",
  "raw",
  "supplier_master.csv"
)

material_file <- file.path(
  "data",
  "raw",
  "material_master.csv"
)

required_files <- c(
  purchase_order_file,
  relationship_file,
  supplier_file,
  material_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required input files are missing: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}

purchase_orders <- read.csv(
  purchase_order_file,
  stringsAsFactors = FALSE
)

supplier_material_relationships <- read.csv(
  relationship_file,
  stringsAsFactors = FALSE
)

supplier_master <- read.csv(
  supplier_file,
  stringsAsFactors = FALSE
)

material_master <- read.csv(
  material_file,
  stringsAsFactors = FALSE
)

# Convert date fields imported from CSV.
purchase_orders$order_date <- as.Date(
  purchase_orders$order_date
)

purchase_orders$promised_delivery_date <- as.Date(
  purchase_orders$promised_delivery_date
)

purchase_orders$actual_delivery_date <- as.Date(
  purchase_orders$actual_delivery_date
)

supplier_material_relationships$relationship_start_date <- as.Date(
  supplier_material_relationships$relationship_start_date
)

supplier_material_relationships$relationship_end_date <- as.Date(
  supplier_material_relationships$relationship_end_date
)

# Convert logical fields imported from CSV.
supplier_material_relationships$preferred_supplier_flag <- as.logical(
  supplier_material_relationships$preferred_supplier_flag
)

supplier_master$active_flag <- as.logical(
  supplier_master$active_flag
)

material_master$active_flag <- as.logical(
  material_master$active_flag
)

# Define the monthly scoring period.
#
# January 2024 allows at least one year of historical purchase-order data.
# July 2026 is the final scoring month with a complete forward-looking
# 30-day prediction window in the generated order history.
scoring_dates <- seq(
  from = as.Date("2024-01-31"),
  to = as.Date("2026-07-31"),
  by = "month"
)

# Date sequences beginning on the 29th, 30th, or 31st can skip or roll into
# unintended calendar dates. Rebuild the scoring dates explicitly as the
# final day of each month.
month_starts <- seq(
  from = as.Date("2024-01-01"),
  to = as.Date("2026-07-01"),
  by = "month"
)

scoring_dates <- as.Date(
  vapply(
    month_starts,
    function(month_start) {
      next_month <- seq(
        from = as.Date(month_start),
        by = "month",
        length.out = 2
      )[2]

      as.character(
        next_month - 1
      )
    },
    character(1)
  )
)

# Safely calculate a quantity-weighted average.
weighted_average_safe <- function(
  values,
  weights
) {
  valid_rows <- !is.na(values) &
    !is.na(weights) &
    weights > 0

  if (!any(valid_rows)) {
    return(NA_real_)
  }

  weighted.mean(
    values[valid_rows],
    weights[valid_rows]
  )
}

# Safely calculate standard deviation.
standard_deviation_safe <- function(values) {
  valid_values <- values[
    !is.na(values)
  ]

  if (length(valid_values) < 2L) {
    return(NA_real_)
  }

  sd(valid_values)
}

# Calculate historical performance features for one relationship and date.
calculate_historical_features <- function(
  relationship_orders,
  scoring_date
) {
  # Only deliveries completed on or before the scoring date may contribute
  # to historical performance features.
  completed_history <- relationship_orders[
    relationship_orders$actual_delivery_date <= scoring_date,
  ]

  history_90_start <- scoring_date - 90
  history_180_start <- scoring_date - 180
  history_365_start <- scoring_date - 365

  history_90 <- completed_history[
    completed_history$actual_delivery_date >
      history_90_start,
  ]

  history_180 <- completed_history[
    completed_history$actual_delivery_date >
      history_180_start,
  ]

  history_365 <- completed_history[
    completed_history$actual_delivery_date >
      history_365_start,
  ]

  late_quantity_rate <- function(history_data) {
    if (
      nrow(history_data) == 0L ||
        sum(history_data$ordered_quantity) == 0
    ) {
      return(NA_real_)
    }

    sum(
      history_data$ordered_quantity *
        history_data$late_delivery_flag
    ) /
      sum(history_data$ordered_quantity)
  }

  on_time_rate <- function(history_data) {
    if (nrow(history_data) == 0L) {
      return(NA_real_)
    }

    mean(
      history_data$late_delivery_flag == 0L
    )
  }

  average_late_days <- function(history_data) {
    if (nrow(history_data) == 0L) {
      return(NA_real_)
    }

    mean(
      history_data$late_days
    )
  }

  data.frame(
    completed_order_count_90d =
      nrow(history_90),
    completed_order_count_180d =
      nrow(history_180),
    completed_order_count_365d =
      nrow(history_365),
    ordered_quantity_90d =
      sum(history_90$ordered_quantity),
    ordered_quantity_180d =
      sum(history_180$ordered_quantity),
    ordered_quantity_365d =
      sum(history_365$ordered_quantity),
    on_time_rate_90d =
      on_time_rate(history_90),
    on_time_rate_180d =
      on_time_rate(history_180),
    on_time_rate_365d =
      on_time_rate(history_365),
    late_quantity_rate_90d =
      late_quantity_rate(history_90),
    late_quantity_rate_180d =
      late_quantity_rate(history_180),
    late_quantity_rate_365d =
      late_quantity_rate(history_365),
    average_late_days_90d =
      average_late_days(history_90),
    average_late_days_180d =
      average_late_days(history_180),
    delivery_variability_180d =
      standard_deviation_safe(
        history_180$late_days
      ),
    maximum_late_days_180d = if (
      nrow(history_180) == 0L
    ) {
      NA_real_
    } else {
      max(history_180$late_days)
    },
    recent_late_order_count_90d =
      sum(history_90$late_delivery_flag),
    days_since_last_completed_delivery = if (
      nrow(completed_history) == 0L
    ) {
      NA_real_
    } else {
      as.numeric(
        scoring_date -
          max(
            completed_history$actual_delivery_date
          )
      )
    },
    stringsAsFactors = FALSE
  )
}

# Calculate current open-order features.
calculate_open_order_features <- function(
  relationship_orders,
  scoring_date
) {
  open_orders <- relationship_orders[
    relationship_orders$order_date <= scoring_date &
      relationship_orders$actual_delivery_date > scoring_date,
  ]

  if (nrow(open_orders) == 0L) {
    return(
      data.frame(
        open_order_count = 0L,
        open_order_quantity = 0,
        open_order_value = 0,
        average_open_order_size_ratio =
          NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  data.frame(
    open_order_count =
      nrow(open_orders),
    open_order_quantity =
      sum(open_orders$ordered_quantity),
    open_order_value =
      sum(open_orders$order_value),
    average_open_order_size_ratio =
      mean(
        open_orders$ordered_quantity
      ),
    stringsAsFactors = FALSE
  )
}

# Build one scoring record.
build_scoring_record <- function(
  relationship_row,
  relationship_orders,
  scoring_date
) {
  prediction_end_date <- scoring_date + 30

  # A relationship is eligible when at least one order was placed by the
  # scoring date and has a promised delivery date in the next 30 days.
  eligible_orders <- relationship_orders[
    relationship_orders$order_date <= scoring_date &
      relationship_orders$actual_delivery_date > scoring_date &
      relationship_orders$promised_delivery_date > scoring_date &
      relationship_orders$promised_delivery_date <=
        prediction_end_date,
  ]

  if (nrow(eligible_orders) == 0L) {
    return(NULL)
  }

  total_quantity_due <- sum(
    eligible_orders$ordered_quantity
  )

  late_quantity_due <- sum(
    eligible_orders$ordered_quantity *
      eligible_orders$late_delivery_flag
  )

  late_quantity_rate_target <-
    late_quantity_due /
    total_quantity_due

  late_delivery_target <- as.integer(
    late_quantity_rate_target > 0.20
  )

  historical_features <- calculate_historical_features(
    relationship_orders = relationship_orders,
    scoring_date = scoring_date
  )

  open_order_features <- calculate_open_order_features(
    relationship_orders = relationship_orders,
    scoring_date = scoring_date
  )

  # Correct the open-order size measure using the relationship's standard
  # order quantity.
  if (
    open_order_features$open_order_count > 0L
  ) {
    open_orders <- relationship_orders[
      relationship_orders$order_date <= scoring_date &
        relationship_orders$actual_delivery_date >
          scoring_date,
    ]

    open_order_features$average_open_order_size_ratio <-
      mean(
        open_orders$ordered_quantity /
          relationship_row$standard_order_quantity
      )
  }

  scoring_record <- data.frame(
    scoring_date =
      scoring_date,
    prediction_end_date =
      prediction_end_date,
    supplier_material_id =
      relationship_row$supplier_material_id,
    supplier_id =
      relationship_row$supplier_id,
    material_id =
      relationship_row$material_id,
    quoted_lead_time_days =
      relationship_row$quoted_lead_time_days,
    minimum_order_quantity =
      relationship_row$minimum_order_quantity,
    standard_order_quantity =
      relationship_row$standard_order_quantity,
    sourcing_allocation =
      relationship_row$sourcing_allocation,
    preferred_supplier_flag =
      relationship_row$preferred_supplier_flag,
    supplier_priority_rank =
      relationship_row$supplier_priority_rank,
    unit_price =
      relationship_row$unit_price,
    orders_due_next_30d =
      nrow(eligible_orders),
    quantity_due_next_30d =
      total_quantity_due,
    value_due_next_30d =
      sum(eligible_orders$order_value),
    average_due_order_size_ratio =
      mean(
        eligible_orders$ordered_quantity /
          relationship_row$standard_order_quantity
      ),
    late_quantity_next_30d =
      late_quantity_due,
    late_quantity_rate_target =
      late_quantity_rate_target,
    late_delivery_target =
      late_delivery_target,
    stringsAsFactors = FALSE
  )

  cbind(
    scoring_record,
    historical_features,
    open_order_features
  )
}

scoring_rows <- list()
scoring_row_counter <- 1L

for (
  scoring_date_index in seq_along(scoring_dates)
) {
  scoring_date <- scoring_dates[
    scoring_date_index
  ]

  for (
    relationship_index in seq_len(
      nrow(supplier_material_relationships)
    )
  ) {
    relationship_row <-
      supplier_material_relationships[
        relationship_index,
      ]

    relationship_orders <- purchase_orders[
      purchase_orders$supplier_material_id ==
        relationship_row$supplier_material_id,
    ]

    if (nrow(relationship_orders) == 0L) {
      next
    }

    scoring_record <- build_scoring_record(
      relationship_row =
        relationship_row,
      relationship_orders =
        relationship_orders,
      scoring_date =
        scoring_date
    )

    if (!is.null(scoring_record)) {
      scoring_rows[[scoring_row_counter]] <-
        scoring_record

      scoring_row_counter <-
        scoring_row_counter + 1L
    }
  }
}

if (length(scoring_rows) == 0L) {
  stop(
    "No eligible monthly scoring records were created."
  )
}

monthly_scoring_dataset <- do.call(
  rbind,
  scoring_rows
)

row.names(monthly_scoring_dataset) <- NULL

# Add descriptive supplier fields.
monthly_scoring_dataset <- merge(
  monthly_scoring_dataset,
  supplier_master[
    c(
      "supplier_id",
      "supplier_name",
      "supplier_region",
      "supplier_tier"
    )
  ],
  by = "supplier_id",
  all.x = TRUE,
  sort = FALSE
)

# Add descriptive material fields.
monthly_scoring_dataset <- merge(
  monthly_scoring_dataset,
  material_master[
    c(
      "material_id",
      "material_name",
      "material_category",
      "material_criticality",
      "average_daily_demand",
      "demand_variability",
      "safety_stock_days",
      "approved_supplier_count"
    )
  ],
  by = "material_id",
  all.x = TRUE,
  sort = FALSE
)

# Restore chronological and relationship order.
monthly_scoring_dataset <- monthly_scoring_dataset[
  order(
    monthly_scoring_dataset$scoring_date,
    monthly_scoring_dataset$supplier_material_id
  ),
]

row.names(monthly_scoring_dataset) <- NULL

monthly_scoring_dataset$scoring_record_id <- sprintf(
  "SCR-%06d",
  seq_len(nrow(monthly_scoring_dataset))
)

# Arrange fields so identifiers and descriptive fields appear first.
monthly_scoring_dataset <- monthly_scoring_dataset[
  c(
    "scoring_record_id",
    "scoring_date",
    "prediction_end_date",
    "supplier_material_id",
    "supplier_id",
    "supplier_name",
    "supplier_region",
    "supplier_tier",
    "material_id",
    "material_name",
    "material_category",
    "material_criticality",
    "quoted_lead_time_days",
    "minimum_order_quantity",
    "standard_order_quantity",
    "sourcing_allocation",
    "preferred_supplier_flag",
    "supplier_priority_rank",
    "unit_price",
    "average_daily_demand",
    "demand_variability",
    "safety_stock_days",
    "approved_supplier_count",
    "orders_due_next_30d",
    "quantity_due_next_30d",
    "value_due_next_30d",
    "average_due_order_size_ratio",
    "completed_order_count_90d",
    "completed_order_count_180d",
    "completed_order_count_365d",
    "ordered_quantity_90d",
    "ordered_quantity_180d",
    "ordered_quantity_365d",
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
    "open_order_count",
    "open_order_quantity",
    "open_order_value",
    "average_open_order_size_ratio",
    "late_quantity_next_30d",
    "late_quantity_rate_target",
    "late_delivery_target"
  )
]

# Validate identifiers and required fields.
stopifnot(
  nrow(monthly_scoring_dataset) > 0,
  !anyDuplicated(
    monthly_scoring_dataset$scoring_record_id
  ),
  !anyDuplicated(
    monthly_scoring_dataset[
      c(
        "scoring_date",
        "supplier_material_id"
      )
    ]
  ),
  !anyNA(
    monthly_scoring_dataset$scoring_date
  ),
  !anyNA(
    monthly_scoring_dataset$supplier_material_id
  ),
  !anyNA(
    monthly_scoring_dataset$late_delivery_target
  ),
  all(
    monthly_scoring_dataset$orders_due_next_30d >= 1L
  ),
  all(
    monthly_scoring_dataset$quantity_due_next_30d > 0
  ),
  all(
    monthly_scoring_dataset$late_quantity_next_30d >= 0
  ),
  all(
    monthly_scoring_dataset$late_quantity_next_30d <=
      monthly_scoring_dataset$quantity_due_next_30d
  ),
  all(
    monthly_scoring_dataset$late_quantity_rate_target >= 0 &
      monthly_scoring_dataset$late_quantity_rate_target <= 1
  ),
  all(
    monthly_scoring_dataset$late_delivery_target ==
      as.integer(
        monthly_scoring_dataset$late_quantity_rate_target >
          0.20
      )
  )
)

# Confirm that no hidden generator variables are present.
forbidden_fields <- c(
  "latent_reliability",
  "latent_relationship_effect",
  "latent_late_probability"
)

stopifnot(
  !any(
    forbidden_fields %in%
      names(monthly_scoring_dataset)
  )
)

dir.create(
  file.path(
    "data",
    "processed"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  monthly_scoring_dataset,
  file = file.path(
    "data",
    "processed",
    "monthly_scoring_dataset.csv"
  ),
  row.names = FALSE,
  na = ""
)

target_rate <- mean(
  monthly_scoring_dataset$late_delivery_target
)

message(
  "Created monthly_scoring_dataset.csv with ",
  format(
    nrow(monthly_scoring_dataset),
    big.mark = ","
  ),
  " supplier-material scoring records."
)

message(
  "Scoring dates: ",
  min(monthly_scoring_dataset$scoring_date),
  " through ",
  max(monthly_scoring_dataset$scoring_date),
  "."
)

message(
  "Supplier-material target rate: ",
  scales::percent(
    target_rate,
    accuracy = 0.1
  )
)

message(
  "Hidden simulation fields were not included."
)

print(
  head(
    monthly_scoring_dataset[
      c(
        "scoring_date",
        "supplier_material_id",
        "supplier_name",
        "material_name",
        "orders_due_next_30d",
        "quantity_due_next_30d",
        "on_time_rate_180d",
        "late_quantity_rate_180d",
        "late_quantity_rate_target",
        "late_delivery_target"
      )
    ],
    12
  )
)