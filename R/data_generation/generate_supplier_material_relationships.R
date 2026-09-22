# Generate Supplier-Material Relationships
#
# Assigns fictional materials to approved fictional suppliers.
# No employer, client, or prospective-employer data is used.

set.seed(421)

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

if (!file.exists(supplier_file)) {
  stop(
    "supplier_master.csv was not found. ",
    "Run R/data_generation/generate_supplier_master.R first."
  )
}

if (!file.exists(material_file)) {
  stop(
    "material_master.csv was not found. ",
    "Run R/data_generation/generate_material_master.R first."
  )
}

supplier_master <- read.csv(
  supplier_file,
  stringsAsFactors = FALSE
)

material_master <- read.csv(
  material_file,
  stringsAsFactors = FALSE
)

# Convert logical fields because read.csv behavior can vary.
supplier_master$active_flag <- as.logical(
  supplier_master$active_flag
)

material_master$active_flag <- as.logical(
  material_master$active_flag
)

active_suppliers <- supplier_master[
  supplier_master$active_flag,
]

active_materials <- material_master[
  material_master$active_flag,
]

if (nrow(active_suppliers) < 3L) {
  stop(
    "At least three active suppliers are required."
  )
}

# Generates sourcing allocations that sum to exactly 1.0.
#
# Primary suppliers generally receive the largest allocation, but the
# allocation is not forced to be equal across approved suppliers.
generate_sourcing_allocations <- function(supplier_count) {
  if (supplier_count == 1L) {
    return(1)
  }

  allocation_weights <- rexp(
    supplier_count,
    rate = 1
  )

  allocations <- allocation_weights /
    sum(allocation_weights)

  allocations <- sort(
    allocations,
    decreasing = TRUE
  )

  allocations <- round(
    allocations,
    digits = 4
  )

  # Correct rounding so allocations sum to exactly 1.0.
  allocations[1] <- allocations[1] +
    (1 - sum(allocations))

  allocations
}

relationship_rows <- vector(
  mode = "list",
  length = nrow(active_materials)
)

relationship_counter <- 1L

for (material_index in seq_len(nrow(active_materials))) {
  material <- active_materials[
    material_index,
  ]

  approved_supplier_count <- min(
    material$approved_supplier_count,
    nrow(active_suppliers)
  )

  selected_supplier_rows <- sample(
    seq_len(nrow(active_suppliers)),
    size = approved_supplier_count,
    replace = FALSE
  )

  selected_suppliers <- active_suppliers[
    selected_supplier_rows,
  ]

  sourcing_allocations <- generate_sourcing_allocations(
    approved_supplier_count
  )

  # Rank suppliers based on sourcing allocation.
  supplier_priority_rank <- rank(
    -sourcing_allocations,
    ties.method = "first"
  )

  relationship_data <- data.frame(
    supplier_material_id = sprintf(
      "SM-%04d",
      relationship_counter +
        seq_len(approved_supplier_count) -
        1L
    ),
    supplier_id = selected_suppliers$supplier_id,
    material_id = rep(
      material$material_id,
      approved_supplier_count
    ),
    relationship_start_date = as.character(
      as.Date("2022-01-01") +
        sample(
          0:730,
          size = approved_supplier_count,
          replace = TRUE
        )
    ),
    relationship_end_date = rep(
      NA_character_,
      approved_supplier_count
    ),
    relationship_status = rep(
      "Active",
      approved_supplier_count
    ),
    quoted_lead_time_days = pmax(
      5L,
      selected_suppliers$standard_lead_time_days +
        sample(
          seq(
            from = -5,
            to = 10,
            by = 5
          ),
          size = approved_supplier_count,
          replace = TRUE,
          prob = c(
            0.10,
            0.30,
            0.40,
            0.20
          )
        )
    ),
    minimum_order_quantity = pmax(
      1L,
      round(
        material$average_daily_demand *
          sample(
            c(
              3,
              5,
              7
            ),
            size = approved_supplier_count,
            replace = TRUE,
            prob = c(
              0.25,
              0.50,
              0.25
            )
          )
      )
    ),
    standard_order_quantity = pmax(
      1L,
      round(
        material$average_daily_demand *
          sample(
            c(
              14,
              21,
              30
            ),
            size = approved_supplier_count,
            replace = TRUE,
            prob = c(
              0.25,
              0.50,
              0.25
            )
          ) *
          sourcing_allocations
      )
    ),
    sourcing_allocation = sourcing_allocations,
    preferred_supplier_flag = (
      supplier_priority_rank == 1L
    ),
    supplier_priority_rank = as.integer(
      supplier_priority_rank
    ),
    unit_price = round(
      material$unit_cost *
        runif(
          approved_supplier_count,
          min = 0.92,
          max = 1.12
        ),
      digits = 2
    ),
    stringsAsFactors = FALSE
  )

  # Ensure that the typical order quantity is never lower than
  # the supplier's minimum order quantity.
  relationship_data$standard_order_quantity <- pmax(
    relationship_data$standard_order_quantity,
    relationship_data$minimum_order_quantity
  )

  # Store the corrected relationship records for the current material.
  relationship_rows[[material_index]] <- relationship_data

  relationship_counter <- relationship_counter +
    approved_supplier_count
}


supplier_material_relationships <- do.call(
  rbind,
  relationship_rows
)

row.names(supplier_material_relationships) <- NULL

# Create latent relationship parameters.
#
# These hidden values allow the same supplier to perform differently across
# different materials. They are not observable business fields and will not
# be exported, modeled, or displayed in the application.
relationship_latent_parameters <- data.frame(
  supplier_material_id =
    supplier_material_relationships$supplier_material_id,
  latent_relationship_effect = round(
    rnorm(
      nrow(supplier_material_relationships),
      mean = 0,
      sd = 0.35
    ),
    digits = 3
  ),
  stringsAsFactors = FALSE
)

# Update approved supplier counts using the relationships actually created.
actual_supplier_counts <- aggregate(
  supplier_id ~ material_id,
  data = supplier_material_relationships,
  FUN = length
)

names(actual_supplier_counts)[
  names(actual_supplier_counts) == "supplier_id"
] <- "actual_supplier_count"

# Validate relationship identifiers and keys.
stopifnot(
  nrow(supplier_material_relationships) > 0,
  !anyDuplicated(
    supplier_material_relationships$supplier_material_id
  ),
  !anyDuplicated(
    supplier_material_relationships[
      c(
        "supplier_id",
        "material_id"
      )
    ]
  ),
  all(
    supplier_material_relationships$supplier_id %in%
      active_suppliers$supplier_id
  ),
  all(
    supplier_material_relationships$material_id %in%
      active_materials$material_id
  ),
  all(
    supplier_material_relationships$quoted_lead_time_days > 0
  ),
  all(
    supplier_material_relationships$minimum_order_quantity > 0
  ),
  all(
    supplier_material_relationships$standard_order_quantity > 0
  ),
  all(
    supplier_material_relationships$unit_price > 0
  ),
    all(
    supplier_material_relationships$standard_order_quantity >=
      supplier_material_relationships$minimum_order_quantity
  )
)

# Validate that each material has exactly one preferred supplier.
preferred_supplier_counts <- aggregate(
  preferred_supplier_flag ~ material_id,
  data = supplier_material_relationships,
  FUN = sum
)

stopifnot(
  all(
    preferred_supplier_counts$preferred_supplier_flag == 1L
  )
)

# Validate that sourcing allocations sum to 1.0 for each material.
allocation_totals <- aggregate(
  sourcing_allocation ~ material_id,
  data = supplier_material_relationships,
  FUN = sum
)

stopifnot(
  all(
    abs(
      allocation_totals$sourcing_allocation - 1
    ) < 0.0001
  )
)

# Validate that generated relationship counts match the material master.
supplier_count_check <- merge(
  active_materials[
    c(
      "material_id",
      "approved_supplier_count"
    )
  ],
  actual_supplier_counts,
  by = "material_id",
  all.x = TRUE
)

stopifnot(
  all(
    supplier_count_check$approved_supplier_count ==
      supplier_count_check$actual_supplier_count
  )
)

# Validate the hidden relationship parameters.
stopifnot(
  nrow(relationship_latent_parameters) ==
    nrow(supplier_material_relationships),
  all(
    relationship_latent_parameters$supplier_material_id ==
      supplier_material_relationships$supplier_material_id
  )
)

write.csv(
  supplier_material_relationships,
  file = file.path(
    "data",
    "raw",
    "supplier_material_relationships.csv"
  ),
  row.names = FALSE,
  na = ""
)

message(
  "Created supplier_material_relationships.csv with ",
  nrow(supplier_material_relationships),
  " approved supplier-material relationships."
)

message(
  "Assigned suppliers to ",
  length(
    unique(
      supplier_material_relationships$material_id
    )
  ),
  " active materials."
)

message(
  "Created generator-only latent parameters for ",
  nrow(relationship_latent_parameters),
  " relationships. These parameters were not exported."
)

print(
  head(
    supplier_material_relationships,
    12
  )
)

print(
  table(
    active_materials$approved_supplier_count
  )
)
