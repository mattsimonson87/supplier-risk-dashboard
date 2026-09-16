# Generate Supplier Master
#
# Creates fictional supplier data for the Supplier Risk Intelligence project.
# No employer, client, or prospective-employer data is used.

set.seed(419)

supplier_count <- 40L

name_prefixes <- c(
  "Apex",
  "Blue Ridge",
  "Cedar",
  "Clearwater",
  "Frontier",
  "Granite",
  "Great Lakes",
  "Ironwood",
  "Northstar",
  "Prairie",
  "Red River",
  "Silver Peak",
  "Summit",
  "Timberline",
  "Twin Harbor"
)

name_suffixes <- c(
  "Components",
  "Industries",
  "Manufacturing",
  "Precision",
  "Systems",
  "Technologies",
  "Works"
)

possible_names <- as.vector(
  outer(
    name_prefixes,
    name_suffixes,
    paste
  )
)

supplier_names <- sample(
  possible_names,
  size = supplier_count,
  replace = FALSE
)

supplier_regions <- c(
  "Midwest",
  "Northeast",
  "Southeast",
  "Southwest",
  "West",
  "Canada",
  "Mexico"
)

# Create the observable supplier master.
#
# These are fields that could reasonably be available to a procurement or
# supply chain team.
supplier_master <- data.frame(
  supplier_id = sprintf(
    "SUP-%03d",
    seq_len(supplier_count)
  ),
  supplier_name = supplier_names,
  supplier_region = sample(
    supplier_regions,
    size = supplier_count,
    replace = TRUE,
    prob = c(
      0.25,
      0.10,
      0.15,
      0.10,
      0.15,
      0.10,
      0.15
    )
  ),
  supplier_tier = sample(
    c(
      "Tier 1",
      "Tier 2",
      "Tier 3"
    ),
    size = supplier_count,
    replace = TRUE,
    prob = c(
      0.20,
      0.50,
      0.30
    )
  ),
  standard_lead_time_days = sample(
    seq(
      from = 10,
      to = 60,
      by = 5
    ),
    size = supplier_count,
    replace = TRUE
  ),
  active_flag = sample(
    c(
      TRUE,
      FALSE
    ),
    size = supplier_count,
    replace = TRUE,
    prob = c(
      0.95,
      0.05
    )
  ),
  stringsAsFactors = FALSE
)

supplier_master <- supplier_master[
  order(supplier_master$supplier_id),
]

row.names(supplier_master) <- NULL

# Create hidden supplier parameters used only by the synthetic-data generator.
#
# Latent reliability creates persistent differences in delivery performance
# among fictional suppliers. It is not an observed business field and must
# not be exported, provided to a predictive model, or displayed in the app.
supplier_latent_parameters <- data.frame(
  supplier_id = supplier_master$supplier_id,
  latent_reliability = round(
    rbeta(
      supplier_count,
      shape1 = 12,
      shape2 = 2
    ),
    digits = 3
  ),
  stringsAsFactors = FALSE
)

# Validate the observable supplier master.
stopifnot(
  nrow(supplier_master) == supplier_count,
  !anyDuplicated(supplier_master$supplier_id),
  !anyDuplicated(supplier_master$supplier_name),
  !anyNA(supplier_master$supplier_id),
  !anyNA(supplier_master$supplier_name),
  !anyNA(supplier_master$supplier_region),
  !anyNA(supplier_master$supplier_tier),
  all(supplier_master$standard_lead_time_days > 0)
)

# Validate the hidden supplier parameters.
stopifnot(
  nrow(supplier_latent_parameters) == supplier_count,
  !anyDuplicated(supplier_latent_parameters$supplier_id),
  all(
    supplier_latent_parameters$supplier_id ==
      supplier_master$supplier_id
  ),
  all(
    supplier_latent_parameters$latent_reliability >= 0 &
      supplier_latent_parameters$latent_reliability <= 1
  )
)

# Create the output directory if it does not already exist.
dir.create(
  file.path(
    "data",
    "raw"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

# Export only the observable supplier master.
#
# supplier_latent_parameters is intentionally not written to a file.
write.csv(
  supplier_master,
  file = file.path(
    "data",
    "raw",
    "supplier_master.csv"
  ),
  row.names = FALSE,
  na = ""
)

message(
  "Created supplier_master.csv with ",
  nrow(supplier_master),
  " fictional suppliers."
)

message(
  "Created generator-only latent parameters for ",
  nrow(supplier_latent_parameters),
  " suppliers. These parameters were not exported."
)

print(
  head(
    supplier_master,
    10
  )
)