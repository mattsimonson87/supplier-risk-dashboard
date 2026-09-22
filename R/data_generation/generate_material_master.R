# Generate Material Master
#
# Creates fictional material data for the Supplier Risk Intelligence project.
# Material names are generic manufacturing components and do not represent
# actual products, part numbers, or bills of material from any company.

set.seed(420)

materials_per_category <- 12L

material_components <- list(
  "Powertrain" = c(
    "Drive Clutch Assembly",
    "Fuel Injector",
    "Oil Pump Housing",
    "Engine Mount",
    "Throttle Body",
    "Transmission Gear Set",
    "Intake Manifold",
    "Exhaust Manifold",
    "Crankcase Cover",
    "Starter Motor",
    "Drive Shaft",
    "Gearcase Housing"
  ),
  "Electrical" = c(
    "Electronic Control Module",
    "Main Wiring Harness",
    "Voltage Regulator",
    "Ignition Coil",
    "Battery Cable Assembly",
    "Position Sensor",
    "Temperature Sensor",
    "Instrument Cluster",
    "Lighting Control Module",
    "Fuse Box Assembly",
    "Relay Module",
    "Power Distribution Harness"
  ),
  "Chassis" = c(
    "Front Control Arm",
    "Rear Control Arm",
    "Frame Crossmember",
    "Steering Linkage",
    "Shock Mount Bracket",
    "Trailing Arm",
    "Stabilizer Bar",
    "Steering Column",
    "Suspension Knuckle",
    "Frame Support Bracket",
    "Tie Rod Assembly",
    "Structural Tube Assembly"
  ),
  "Braking" = c(
    "Brake Rotor",
    "Brake Caliper Assembly",
    "Hydraulic Brake Line",
    "Master Cylinder",
    "Brake Pad Set",
    "Parking Brake Cable",
    "Brake Pedal Assembly",
    "Wheel Speed Sensor",
    "Brake Hose",
    "Caliper Mounting Bracket",
    "Brake Fluid Reservoir",
    "Pressure Control Valve"
  ),
  "Thermal Management" = c(
    "Radiator Core",
    "Cooling Fan Assembly",
    "Coolant Hose",
    "Thermostat Housing",
    "Heat Shield",
    "Coolant Reservoir",
    "Oil Cooler",
    "Fan Shroud",
    "Coolant Pump",
    "Radiator Mount",
    "Temperature Control Valve",
    "Air Duct Assembly"
  ),
  "Body and Cab" = c(
    "Molded Side Panel",
    "Floor Panel",
    "Seat Frame",
    "Windshield Bracket",
    "Door Hinge Assembly",
    "Roof Support",
    "Cargo Box Panel",
    "Front Grille",
    "Fender Panel",
    "Dashboard Support",
    "Protective Skid Plate",
    "Cab Mount Bracket"
  ),
  "Drivetrain" = c(
    "Primary Drive Belt",
    "Axle Shaft",
    "Wheel Hub",
    "Sealed Wheel Bearing",
    "Constant Velocity Joint",
    "Propeller Shaft",
    "Differential Gear",
    "Drive Sprocket",
    "Chain Tensioner",
    "Hub Bearing Assembly",
    "Coupling Assembly",
    "Final Drive Gear"
  ),
  "Raw Material" = c(
    "Aluminum Casting",
    "Steel Tube",
    "Molded Polymer Resin",
    "Sheet Steel",
    "Rubber Compound",
    "Aluminum Extrusion",
    "Stainless Steel Coil",
    "Structural Adhesive",
    "Powder Coating Material",
    "Glass Fiber Composite",
    "Copper Wire",
    "Protective Foam Sheet"
  )
)

material_categories <- names(material_components)

material_master <- do.call(
  rbind,
  lapply(
    seq_along(material_categories),
    function(category_index) {
      category <- material_categories[category_index]
      component_names <- material_components[[category]]

      data.frame(
        material_category = category,
        material_name = component_names,
        stringsAsFactors = FALSE
      )
    }
  )
)

row.names(material_master) <- NULL

material_count <- nrow(material_master)

material_master$material_id <- sprintf(
  "MAT-%04d",
  seq_len(material_count)
)

# Assign criticality probabilities by material category.
criticality_probabilities <- list(
  "Powertrain" = c(0.05, 0.25, 0.70),
  "Electrical" = c(0.10, 0.35, 0.55),
  "Chassis" = c(0.10, 0.40, 0.50),
  "Braking" = c(0.05, 0.20, 0.75),
  "Thermal Management" = c(0.05, 0.30, 0.65),
  "Body and Cab" = c(0.30, 0.50, 0.20),
  "Drivetrain" = c(0.05, 0.25, 0.70),
  "Raw Material" = c(0.20, 0.50, 0.30)
)

material_master$material_criticality <- vapply(
  material_master$material_category,
  function(category) {
    sample(
      c("Low", "Medium", "High"),
      size = 1,
      prob = criticality_probabilities[[category]]
    )
  },
  character(1)
)

# Use category-specific cost ranges to create plausible differences.
unit_cost_ranges <- list(
  "Powertrain" = c(75, 900),
  "Electrical" = c(20, 500),
  "Chassis" = c(30, 600),
  "Braking" = c(15, 350),
  "Thermal Management" = c(20, 450),
  "Body and Cab" = c(15, 400),
  "Drivetrain" = c(25, 700),
  "Raw Material" = c(2, 150)
)

material_master$unit_cost <- vapply(
  material_master$material_category,
  function(category) {
    cost_range <- unit_cost_ranges[[category]]

    round(
      runif(
        n = 1,
        min = cost_range[1],
        max = cost_range[2]
      ),
      digits = 2
    )
  },
  numeric(1)
)

# Generate typical daily demand.
#
# A log-normal distribution creates many moderate-demand materials and a
# smaller number of high-demand materials.
material_master$average_daily_demand <- round(
  rlnorm(
    material_count,
    meanlog = log(25),
    sdlog = 0.75
  ),
  digits = 1
)

# Demand variability is represented as a coefficient of variation.
material_master$demand_variability <- round(
  runif(
    material_count,
    min = 0.10,
    max = 0.65
  ),
  digits = 2
)

material_master$safety_stock_days <- sample(
  c(7L, 14L, 21L, 30L, 45L),
  size = material_count,
  replace = TRUE,
  prob = c(0.10, 0.30, 0.30, 0.20, 0.10)
)

# Most materials have one or two approved suppliers. A smaller number have
# three approved suppliers.
material_master$approved_supplier_count <- sample(
  c(1L, 2L, 3L),
  size = material_count,
  replace = TRUE,
  prob = c(0.35, 0.50, 0.15)
)

material_master$active_flag <- sample(
  c(TRUE, FALSE),
  size = material_count,
  replace = TRUE,
  prob = c(0.97, 0.03)
)

# Arrange the exported columns intentionally.
material_master <- material_master[
  c(
    "material_id",
    "material_name",
    "material_category",
    "material_criticality",
    "unit_cost",
    "average_daily_demand",
    "demand_variability",
    "safety_stock_days",
    "approved_supplier_count",
    "active_flag"
  )
]

# Validation checks
stopifnot(
  nrow(material_master) == material_count,
  !anyDuplicated(material_master$material_id),
  !anyDuplicated(material_master$material_name),
  !anyNA(material_master$material_id),
  !anyNA(material_master$material_name),
  all(material_master$unit_cost > 0),
  all(material_master$average_daily_demand > 0),
  all(
    material_master$demand_variability >= 0 &
      material_master$demand_variability <= 1
  ),
  all(material_master$safety_stock_days > 0),
  all(
    material_master$approved_supplier_count %in% c(1L, 2L, 3L)
  )
)

dir.create(
  file.path("data", "raw"),
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  material_master,
  file = file.path(
    "data",
    "raw",
    "material_master.csv"
  ),
  row.names = FALSE,
  na = ""
)

message(
  "Created material_master.csv with ",
  nrow(material_master),
  " fictional materials across ",
  length(unique(material_master$material_category)),
  " categories."
)

print(
  head(
    material_master,
    12
  )
)

print(
  table(
    material_master$material_category,
    material_master$material_criticality
  )
)