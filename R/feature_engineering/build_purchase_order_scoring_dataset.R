# Build Purchase-Order Scoring Dataset
#
# Creates one analytical record per purchase-order line at the order date.
# Historical features use only deliveries completed on or before that date.
# Supplier workload uses only purchase orders visible on that date.
# No employer, client, or prospective-employer data is used.

library(dplyr)
library(readr)

script_start_time <- Sys.time()

# -------------------------------------------------------------------------
# Input files
# -------------------------------------------------------------------------

purchase_order_file <- file.path("data", "raw", "purchase_order_history.csv")
relationship_file <- file.path("data", "raw", "supplier_material_relationships.csv")
supplier_file <- file.path("data", "raw", "supplier_master.csv")
material_file <- file.path("data", "raw", "material_master.csv")

required_files <- c(
  purchase_order_file,
  relationship_file,
  supplier_file,
  material_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Required source files are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

# -------------------------------------------------------------------------
# Load source data
# -------------------------------------------------------------------------

purchase_orders <- read_csv(
  purchase_order_file,
  show_col_types = FALSE
) %>%
  mutate(
    order_date = as.Date(order_date),
    promised_delivery_date = as.Date(promised_delivery_date),
    actual_delivery_date = as.Date(actual_delivery_date),
    late_delivery_flag = as.integer(late_delivery_flag)
  )

relationships <- read_csv(
  relationship_file,
  show_col_types = FALSE
) %>%
  mutate(
    relationship_start_date = as.Date(relationship_start_date),
    relationship_end_date = as.Date(relationship_end_date),
    preferred_supplier_flag = as.logical(preferred_supplier_flag)
  )

suppliers <- read_csv(
  supplier_file,
  show_col_types = FALSE
) %>%
  mutate(active_flag = as.logical(active_flag))

materials <- read_csv(
  material_file,
  show_col_types = FALSE
) %>%
  mutate(active_flag = as.logical(active_flag))

# -------------------------------------------------------------------------
# Validate source data
# -------------------------------------------------------------------------

stopifnot(
  nrow(purchase_orders) > 0L,
  !anyDuplicated(purchase_orders$purchase_order_line_id),
  !anyDuplicated(purchase_orders$purchase_order_id),
  !anyDuplicated(relationships$supplier_material_id),
  !anyDuplicated(suppliers$supplier_id),
  !anyDuplicated(materials$material_id),
  all(
    purchase_orders$actual_delivery_date ==
      purchase_orders$promised_delivery_date + purchase_orders$late_days
  ),
  all(
    purchase_orders$late_delivery_flag ==
      as.integer(purchase_orders$late_days > 7L)
  )
)

# -------------------------------------------------------------------------
# Add supplier, material, and relationship context
# -------------------------------------------------------------------------

order_data <- purchase_orders %>%
  left_join(
    relationships %>%
      select(
        supplier_material_id,
        relationship_start_date,
        relationship_end_date,
        relationship_status,
        quoted_lead_time_days,
        minimum_order_quantity,
        standard_order_quantity,
        sourcing_allocation,
        preferred_supplier_flag,
        supplier_priority_rank
      ),
    by = "supplier_material_id"
  ) %>%
  left_join(
    suppliers %>%
      select(
        supplier_id,
        supplier_name,
        supplier_region,
        supplier_tier
      ),
    by = "supplier_id"
  ) %>%
  left_join(
    materials %>%
      select(
        material_id,
        material_name,
        material_category,
        material_criticality,
        approved_supplier_count
      ),
    by = "material_id"
  ) %>%
  mutate(
    scoring_date = order_date,
    planned_lead_time_days = as.integer(
      promised_delivery_date - order_date
    ),
    lead_time_pressure_days = pmax(
      quoted_lead_time_days - planned_lead_time_days,
      0L
    ),
    order_size_ratio = ordered_quantity / standard_order_quantity
  ) %>%
  arrange(scoring_date, purchase_order_line_id)

stopifnot(
  !anyNA(order_data$quoted_lead_time_days),
  !anyNA(order_data$standard_order_quantity),
  !anyNA(order_data$supplier_name),
  !anyNA(order_data$material_name),
  all(order_data$scoring_date == order_data$order_date),
  all(order_data$planned_lead_time_days > 0L),
  all(order_data$lead_time_pressure_days >= 0L),
  all(order_data$order_size_ratio > 0)
)

message(
  "Purchase-order lines available for scoring: ",
  format(nrow(order_data), big.mark = ","),
  "."
)

# -------------------------------------------------------------------------
# Create indexed history lookups
# -------------------------------------------------------------------------

relationship_history_lookup <- split(
  order_data,
  order_data$supplier_material_id
)

supplier_history_lookup <- split(
  order_data,
  order_data$supplier_id
)

# -------------------------------------------------------------------------
# Safe history summaries
# -------------------------------------------------------------------------

standard_deviation_safe <- function(values) {
  valid_values <- values[!is.na(values)]

  if (length(valid_values) < 2L) {
    return(NA_real_)
  }

  sd(valid_values)
}

on_time_rate_safe <- function(history_data) {
  if (nrow(history_data) == 0L) {
    return(NA_real_)
  }

  mean(history_data$late_delivery_flag == 0L)
}

late_quantity_rate_safe <- function(history_data) {
  total_quantity <- sum(
    history_data$ordered_quantity,
    na.rm = TRUE
  )

  if (nrow(history_data) == 0L || total_quantity <= 0) {
    return(NA_real_)
  }

  sum(
    history_data$ordered_quantity * history_data$late_delivery_flag,
    na.rm = TRUE
  ) / total_quantity
}

average_late_days_safe <- function(history_data) {
  if (nrow(history_data) == 0L) {
    return(NA_real_)
  }

  mean(history_data$late_days, na.rm = TRUE)
}

maximum_late_days_safe <- function(history_data) {
  if (nrow(history_data) == 0L) {
    return(NA_real_)
  }

  max(history_data$late_days, na.rm = TRUE)
}

summarize_completed_history <- function(
  completed_history,
  current_scoring_date,
  prefix = ""
) {
  history_90d <- completed_history[
    completed_history$actual_delivery_date > current_scoring_date - 90L,
  ]

  history_180d <- completed_history[
    completed_history$actual_delivery_date > current_scoring_date - 180L,
  ]

  history_365d <- completed_history[
    completed_history$actual_delivery_date > current_scoring_date - 365L,
  ]

  summary_values <- tibble(
    completed_order_count_90d = nrow(history_90d),
    completed_order_count_180d = nrow(history_180d),
    completed_order_count_365d = nrow(history_365d),
    historical_quantity_90d = sum(history_90d$ordered_quantity, na.rm = TRUE),
    historical_quantity_180d = sum(history_180d$ordered_quantity, na.rm = TRUE),
    historical_quantity_365d = sum(history_365d$ordered_quantity, na.rm = TRUE),
    on_time_rate_90d = on_time_rate_safe(history_90d),
    on_time_rate_180d = on_time_rate_safe(history_180d),
    on_time_rate_365d = on_time_rate_safe(history_365d),
    late_quantity_rate_90d = late_quantity_rate_safe(history_90d),
    late_quantity_rate_180d = late_quantity_rate_safe(history_180d),
    late_quantity_rate_365d = late_quantity_rate_safe(history_365d),
    average_late_days_90d = average_late_days_safe(history_90d),
    average_late_days_180d = average_late_days_safe(history_180d),
    delivery_variability_180d = standard_deviation_safe(history_180d$late_days),
    maximum_late_days_180d = maximum_late_days_safe(history_180d),
    recent_late_order_count_90d = sum(
      history_90d$late_delivery_flag,
      na.rm = TRUE
    ),
    days_since_last_completed_delivery = if (
      nrow(completed_history) == 0L
    ) {
      NA_real_
    } else {
      as.numeric(
        current_scoring_date -
          max(completed_history$actual_delivery_date, na.rm = TRUE)
      )
    }
  )

  if (nzchar(prefix)) {
    names(summary_values) <- paste0(prefix, names(summary_values))
  }

  summary_values
}

# -------------------------------------------------------------------------
# Calculate supplier-material delivery history
# -------------------------------------------------------------------------

calculate_relationship_history <- function(current_order) {
  current_scoring_date <- current_order$scoring_date[[1]]
  current_relationship_id <- current_order$supplier_material_id[[1]]
  current_order_line_id <- current_order$purchase_order_line_id[[1]]

  relationship_orders <-
    relationship_history_lookup[[current_relationship_id]]

  if (is.null(relationship_orders)) {
    relationship_orders <- order_data[0, ]
  }

  completed_history <- relationship_orders[
    relationship_orders$purchase_order_line_id != current_order_line_id &
      relationship_orders$actual_delivery_date <= current_scoring_date,
  ]

  summarize_completed_history(
    completed_history = completed_history,
    current_scoring_date = current_scoring_date
  )
}

# -------------------------------------------------------------------------
# Calculate supplier-level delivery history
# -------------------------------------------------------------------------

calculate_supplier_delivery_history <- function(current_order) {
  current_scoring_date <- current_order$scoring_date[[1]]
  current_supplier_id <- current_order$supplier_id[[1]]
  current_order_line_id <- current_order$purchase_order_line_id[[1]]

  supplier_orders <- supplier_history_lookup[[current_supplier_id]]

  if (is.null(supplier_orders)) {
    supplier_orders <- order_data[0, ]
  }

  completed_history <- supplier_orders[
    supplier_orders$purchase_order_line_id != current_order_line_id &
      supplier_orders$actual_delivery_date <= current_scoring_date,
  ]

  summarize_completed_history(
    completed_history = completed_history,
    current_scoring_date = current_scoring_date,
    prefix = "supplier_"
  )
}

# -------------------------------------------------------------------------
# Calculate supplier workload at order creation
# -------------------------------------------------------------------------

calculate_supplier_workload <- function(current_order) {
  current_scoring_date <- current_order$scoring_date[[1]]
  current_supplier_id <- current_order$supplier_id[[1]]

  supplier_orders <- supplier_history_lookup[[current_supplier_id]]

  if (is.null(supplier_orders)) {
    supplier_orders <- order_data[0, ]
  }

  # End-of-day scoring assumption: all orders created on the scoring date are
  # visible. Orders delivered on the scoring date remain included here to
  # preserve the current project definition in this single-purpose update.
  supplier_open_orders <- supplier_orders[
    supplier_orders$order_date <= current_scoring_date &
      supplier_orders$actual_delivery_date >= current_scoring_date,
  ]

  supplier_recent_orders <- supplier_orders[
    supplier_orders$order_date <= current_scoring_date &
      supplier_orders$order_date > current_scoring_date - 90L,
  ]

  tibble(
    supplier_open_order_count = nrow(supplier_open_orders),
    supplier_open_order_quantity = sum(
      supplier_open_orders$ordered_quantity,
      na.rm = TRUE
    ),
    supplier_open_order_value = sum(
      supplier_open_orders$order_value,
      na.rm = TRUE
    ),
    supplier_recent_order_count_90d = nrow(supplier_recent_orders),
    supplier_recent_order_quantity_90d = sum(
      supplier_recent_orders$ordered_quantity,
      na.rm = TRUE
    ),
    supplier_average_open_order_size_ratio = if (
      nrow(supplier_open_orders) == 0L
    ) {
      NA_real_
    } else {
      mean(
        supplier_open_orders$order_size_ratio,
        na.rm = TRUE
      )
    }
  )
}

# -------------------------------------------------------------------------
# Build one analytical scoring record per purchase-order line
# -------------------------------------------------------------------------

message(
  paste(
    "Calculating time-aware relationship history,",
    "supplier history, and workload features..."
  )
)

scoring_rows <- lapply(
  seq_len(nrow(order_data)),
  function(row_index) {
    current_order <- order_data[row_index, ]

    relationship_history <- calculate_relationship_history(current_order)
    supplier_delivery_history <-
      calculate_supplier_delivery_history(current_order)
    supplier_workload <- calculate_supplier_workload(current_order)

    bind_cols(
      current_order %>%
        transmute(
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
          order_date,
          promised_delivery_date,
          actual_delivery_date,
          planned_lead_time_days,
          quoted_lead_time_days,
          lead_time_pressure_days,
          ordered_quantity,
          minimum_order_quantity,
          standard_order_quantity,
          order_size_ratio,
          unit_price,
          order_value,
          sourcing_allocation,
          preferred_supplier_flag,
          supplier_priority_rank,
          approved_supplier_count,
          actual_late_days = late_days,
          late_delivery_target = late_delivery_flag
        ),
      relationship_history,
      supplier_delivery_history,
      supplier_workload
    )
  }
)

# -------------------------------------------------------------------------
# Assemble the scoring dataset
# -------------------------------------------------------------------------

purchase_order_scoring_dataset <- bind_rows(scoring_rows) %>%
  mutate(
    workload_pressure_90d = pmax(
      supplier_recent_order_quantity_90d / 8000 - 1,
      0
    )
  ) %>%
  arrange(scoring_date, purchase_order_line_id) %>%
  mutate(
    scoring_record_id = sprintf("SCR-%06d", row_number()),
    scoring_month = as.integer(format(scoring_date, "%m")),
    scoring_month_sin = sin(2 * pi * scoring_month / 12),
    scoring_month_cos = cos(2 * pi * scoring_month / 12),
    promised_delivery_month = as.integer(
      format(promised_delivery_date, "%m")
    ),
    promised_delivery_month_sin = sin(
      2 * pi * promised_delivery_month / 12
    ),
    promised_delivery_month_cos = cos(
      2 * pi * promised_delivery_month / 12
    ),
    has_90d_relationship_history = completed_order_count_90d > 0L,
    has_180d_relationship_history = completed_order_count_180d > 0L,
    has_365d_relationship_history = completed_order_count_365d > 0L,
    has_90d_supplier_history = supplier_completed_order_count_90d > 0L,
    has_180d_supplier_history = supplier_completed_order_count_180d > 0L,
    has_365d_supplier_history = supplier_completed_order_count_365d > 0L
  ) %>%
  relocate(scoring_record_id, .before = scoring_date)

# -------------------------------------------------------------------------
# Validate the completed scoring dataset
# -------------------------------------------------------------------------

required_supplier_history_fields <- c(
  "supplier_completed_order_count_90d",
  "supplier_completed_order_count_180d",
  "supplier_completed_order_count_365d",
  "supplier_on_time_rate_90d",
  "supplier_on_time_rate_180d",
  "supplier_on_time_rate_365d",
  "supplier_late_quantity_rate_90d",
  "supplier_late_quantity_rate_180d",
  "supplier_late_quantity_rate_365d",
  "supplier_average_late_days_90d",
  "supplier_average_late_days_180d",
  "supplier_delivery_variability_180d",
  "supplier_recent_late_order_count_90d",
  "supplier_days_since_last_completed_delivery",
  "has_90d_supplier_history",
  "has_180d_supplier_history",
  "has_365d_supplier_history"
)

stopifnot(
  nrow(purchase_order_scoring_dataset) == nrow(purchase_orders),
  !anyDuplicated(purchase_order_scoring_dataset$scoring_record_id),
  !anyDuplicated(purchase_order_scoring_dataset$purchase_order_line_id),
  all(
    required_supplier_history_fields %in%
      names(purchase_order_scoring_dataset)
  ),
  all(
    purchase_order_scoring_dataset$scoring_date ==
      purchase_order_scoring_dataset$order_date
  ),
  all(purchase_order_scoring_dataset$planned_lead_time_days > 0L),
  all(purchase_order_scoring_dataset$lead_time_pressure_days >= 0L),
  all(purchase_order_scoring_dataset$order_size_ratio > 0),
  all(purchase_order_scoring_dataset$supplier_open_order_count >= 1L),
  all(purchase_order_scoring_dataset$supplier_recent_order_count_90d >= 1L),
  all(purchase_order_scoring_dataset$supplier_recent_order_quantity_90d > 0),
  all(purchase_order_scoring_dataset$workload_pressure_90d >= 0),
  all(
    purchase_order_scoring_dataset$late_delivery_target %in% c(0L, 1L)
  ),
  all(
    purchase_order_scoring_dataset$late_delivery_target ==
      as.integer(purchase_order_scoring_dataset$actual_late_days > 7L)
  )
)

forbidden_fields <- c(
  "latent_reliability",
  "latent_relationship_effect",
  "latent_late_probability"
)

stopifnot(
  !any(
    forbidden_fields %in%
      names(purchase_order_scoring_dataset)
  )
)

# -------------------------------------------------------------------------
# Export the analytical dataset
# -------------------------------------------------------------------------

dir.create(
  file.path("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  purchase_order_scoring_dataset,
  file.path(
    "data",
    "processed",
    "purchase_order_scoring_dataset.csv"
  ),
  na = ""
)

# -------------------------------------------------------------------------
# Print concise profile
# -------------------------------------------------------------------------

late_delivery_rate <- mean(
  purchase_order_scoring_dataset$late_delivery_target
)

records_without_90d_relationship_history <- sum(
  !purchase_order_scoring_dataset$has_90d_relationship_history
)

records_without_180d_relationship_history <- sum(
  !purchase_order_scoring_dataset$has_180d_relationship_history
)

records_without_365d_relationship_history <- sum(
  !purchase_order_scoring_dataset$has_365d_relationship_history
)

records_without_90d_supplier_history <- sum(
  !purchase_order_scoring_dataset$has_90d_supplier_history
)

records_without_180d_supplier_history <- sum(
  !purchase_order_scoring_dataset$has_180d_supplier_history
)

records_without_365d_supplier_history <- sum(
  !purchase_order_scoring_dataset$has_365d_supplier_history
)

elapsed_seconds <- as.numeric(
  difftime(
    Sys.time(),
    script_start_time,
    units = "secs"
  )
)

cat("\nPURCHASE-ORDER SCORING DATASET\n")
cat("Scoring records:", format(nrow(purchase_order_scoring_dataset), big.mark = ","), "\n")
cat("First scoring date:", format(min(purchase_order_scoring_dataset$scoring_date)), "\n")
cat("Last scoring date:", format(max(purchase_order_scoring_dataset$scoring_date)), "\n")
cat("Minimum planned lead time:", min(purchase_order_scoring_dataset$planned_lead_time_days), "days\n")
cat("Maximum planned lead time:", max(purchase_order_scoring_dataset$planned_lead_time_days), "days\n")
cat("Late-delivery target rate:", scales::percent(late_delivery_rate, accuracy = 0.1), "\n")
cat("Records without 90-day relationship history:", format(records_without_90d_relationship_history, big.mark = ","), "\n")
cat("Records without 180-day relationship history:", format(records_without_180d_relationship_history, big.mark = ","), "\n")
cat("Records without 365-day relationship history:", format(records_without_365d_relationship_history, big.mark = ","), "\n")
cat("Records without 90-day supplier history:", format(records_without_90d_supplier_history, big.mark = ","), "\n")
cat("Records without 180-day supplier history:", format(records_without_180d_supplier_history, big.mark = ","), "\n")
cat("Records without 365-day supplier history:", format(records_without_365d_supplier_history, big.mark = ","), "\n")
cat("Elapsed time:", round(elapsed_seconds, digits = 1), "seconds\n")

message("Purchase-order scoring dataset created successfully.")
message("Relationship-level and supplier-level histories were created.")
message("Every purchase-order line was scored on its order date.")
message("Actual delivery dates and outcomes were retained only for evaluation.")

print(
  head(
    purchase_order_scoring_dataset %>%
      select(
        scoring_date,
        purchase_order_line_id,
        supplier_name,
        material_name,
        completed_order_count_180d,
        on_time_rate_180d,
        supplier_completed_order_count_180d,
        supplier_on_time_rate_180d,
        supplier_late_quantity_rate_180d,
        supplier_recent_order_quantity_90d,
        workload_pressure_90d,
        late_delivery_target
      ),
    12
  )
)
