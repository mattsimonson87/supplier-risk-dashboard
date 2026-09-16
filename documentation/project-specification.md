# Project Specification

## Business Problem

Supply chain teams need to identify which supplier-material relationships
are most likely to experience a late delivery during the next
30 days.

Because not every late delivery has the same operational consequence, teams
must also determine which predicted delays would have the greatest business
impact and require immediate mitigation.

## Unit of Analysis

One supplier-material relationship evaluated at a specific monthly scoring date. A supplier may provide multiple materials, and a material may be associated with multiple approved suppliers.

## Prediction Target

Predict whether an expected delivery for a supplier-material relationship
will arrive more than 7 calendar days after its promised delivery date during
the 30 days following the scoring date.

All suppliers are assumed to eventually provide the full ordered quantity.
Partial and incomplete deliveries are outside the scope of the initial
project version.

## Target Definition

A supplier-material relationship is eligible for scoring when at least one
open purchase-order line has a promised delivery date within the 30 days
following the scoring date.

For each eligible purchase-order line:

- The late-delivery indicator equals 1 when the actual delivery date is more
  than 7 calendar days after the promised delivery date.
- The late-delivery indicator equals 0 when the actual delivery date is no
  more than 7 calendar days after the promised delivery date.

The supplier-material target is based on the proportion of quantity due
during the prediction window that arrives more than 7 calendar days after
its promised delivery date.

The late quantity rate is calculated as:

late quantity rate = quantity delivered more than 7 calendar days late /
total quantity due during the 30-day prediction window

The supplier-material target equals 1 when the late quantity rate is greater
than 20 percent. Otherwise, the target equals 0.

Supplier-material relationships without an expected delivery during the
30-day prediction window are not included in that scoring period.

## Prediction Timing

All model features must use information available on or before the scoring
date. Delivery outcomes occurring after the scoring date must not be used
as predictors.

## Primary Decision

Which supplier-material risks should supply chain teams investigate and
mitigate first?

## Analytical Outputs

The project will generate:

- A predicted late-delivery probability
- An operational impact score
- An overall mitigation priority
- The primary factors contributing to each prediction
- A supplier and material risk watchlist

## Modeling Approach

The project will compare:

1. Logistic regression as an interpretable baseline
2. XGBoost as the primary nonlinear model

Models will be evaluated using an out-of-time validation strategy.

## Data Approach

All supplier, material, purchase order, delivery, inventory, and demand data
will be synthetically generated.

No employer, client, or prospective-employer data or proprietary logic will
be used.

## Initial Scope

The first version will include:

- Supplier master data
- Material master data
- Purchase order and delivery history
- Monthly inventory and demand snapshots
- Time-aware supplier performance features
- Predictive model evaluation
- An interactive Shiny and Plotly application

## Synthetic Data Tables

### Supplier Master

The supplier master will contain one row per supplier.

Planned fields:

- `supplier_id`: Unique synthetic supplier identifier
- `supplier_name`: Fictional supplier name used in the application
- `supplier_region`: Geographic region in which the supplier operates
- `supplier_tier`: Strategic classification such as Tier 1, Tier 2, or Tier 3
- `standard_lead_time_days`: Typical number of calendar days between order
  placement and promised delivery
- `baseline_reliability`: Underlying synthetic tendency to deliver orders
  on time
- `active_flag`: Indicates whether the supplier is currently active

The supplier master will contain only fictional suppliers and synthetically
generated attributes.

### Material Master

The material master will contain one row per material.

Planned fields:

- `material_id`: Unique synthetic material identifier
- `material_name`: Fictional material name used in the application
- `material_category`: Broad grouping such as electronics, mechanical,
  packaging, or raw material
- `material_criticality`: Operational importance classified as low, medium,
  or high
- `unit_cost`: Synthetic cost per unit
- `average_daily_demand`: Typical number of units consumed per day
- `demand_variability`: Synthetic measure of variation in daily demand
- `safety_stock_days`: Target number of days of inventory maintained as
  protection against uncertainty
- `approved_supplier_count`: Number of suppliers approved to provide the
  material
- `active_flag`: Indicates whether the material is currently active

A material may be associated with multiple approved suppliers. The actual
supplier-material relationships will be stored separately rather than
assigning a single supplier directly in the material master.

### Supplier-Material Relationship

The supplier-material relationship table will contain one row per approved
supplier and material pairing.

This table represents the many-to-many relationship between suppliers and
materials. A supplier may provide multiple materials, and a material may be
available from multiple approved suppliers.

Planned fields:

- `supplier_material_id`: Unique identifier for the supplier-material
  relationship
- `supplier_id`: Supplier identifier linked to the supplier master
- `material_id`: Material identifier linked to the material master
- `relationship_start_date`: Date on which the supplier became approved to
  provide the material
- `relationship_end_date`: Optional date on which the relationship became
  inactive
- `relationship_status`: Indicates whether the relationship is active,
  suspended, or inactive
- `quoted_lead_time_days`: Expected number of calendar days between order
  placement and promised delivery for this specific supplier-material
  relationship
- `minimum_order_quantity`: Minimum quantity accepted for an individual
  purchase order
- `standard_order_quantity`: Typical quantity ordered from the supplier for
  the material
- `sourcing_allocation`: Expected proportion of the material's demand
  allocated to this supplier
- `preferred_supplier_flag`: Indicates whether the supplier is the preferred
  source for the material
- `supplier_priority_rank`: Supplier preference rank for the material, where
  1 represents the first-choice supplier
- `unit_price`: Synthetic negotiated price per unit for this supplier and
  material combination
- `relationship_risk_factor`: Latent synthetic factor used only when
  generating delivery outcomes

The combination of `supplier_id` and `material_id` must be unique in this
table.

The `sourcing_allocation` values for all active suppliers associated with a
material should total approximately 1.0. A material with one active supplier
will therefore have a sourcing allocation of 1.0.

The `relationship_risk_factor` will help generate realistic differences in
delivery performance across supplier-material relationships. It will not be
provided directly to the predictive model because it represents an
unobservable synthetic characteristic rather than information available to
a supply chain analyst.

### Purchase-Order and Delivery History

The purchase-order and delivery history table will contain one row per
purchase-order line.

Planned fields:

- `purchase_order_line_id`: Unique identifier for the purchase-order line
- `purchase_order_id`: Identifier shared by all lines belonging to the same
  purchase order
- `supplier_material_id`: Identifier linking the order line to an approved
  supplier-material relationship
- `supplier_id`: Supplier identifier included for validation and convenient
  analysis
- `material_id`: Material identifier included for validation and convenient
  analysis
- `order_date`: Date on which the purchase order was placed
- `promised_delivery_date`: Date on which the supplier committed to deliver
  the order
- `actual_delivery_date`: Date on which the full ordered quantity was
  delivered
- `ordered_quantity`: Number of units ordered
- `unit_price`: Synthetic negotiated price per unit at the time of the order
- `order_value`: Ordered quantity multiplied by unit price
- `late_days`: Number of calendar days between the promised and actual
  delivery dates, with early deliveries represented by negative values
- `late_delivery_flag`: Indicates whether the delivery occurred more than
  7 calendar days after the promised delivery date

All purchase-order lines will eventually be delivered in full. Partial
deliveries, cancellations, and undelivered orders are outside the scope of
the initial project version.

The promised delivery date will be based primarily on the order date and the
quoted lead time for the supplier-material relationship.

Synthetic delivery performance will vary based on hidden supplier and
supplier-material reliability factors, order size relative to the standard
order quantity, seasonal patterns, recent order volume, and random variation.

The hidden reliability factors will be used only to generate synthetic
delivery outcomes and will not be provided directly to the predictive model.

### Monthly Inventory and Demand Snapshot

The monthly inventory and demand snapshot table will contain one row per
material and monthly scoring date.

Planned fields:

- `snapshot_id`: Unique identifier for the material and scoring-date
  combination
- `scoring_date`: Monthly date on which inventory exposure and supplier risk
  are evaluated
- `material_id`: Material identifier linked to the material master
- `on_hand_quantity`: Usable inventory available on the scoring date
- `open_order_quantity`: Quantity already ordered but not yet delivered as
  of the scoring date
- `average_daily_demand_30d`: Average daily demand during the 30 days
  preceding the scoring date
- `average_daily_demand_90d`: Average daily demand during the 90 days
  preceding the scoring date
- `forecast_demand_30d`: Expected material demand during the 30 days
  following the scoring date
- `demand_variability_90d`: Variability in daily demand during the 90 days
  preceding the scoring date
- `days_of_supply`: Number of days that current on-hand inventory is expected
  to support based on recent average daily demand
- `projected_inventory_30d`: Estimated inventory remaining after expected
  demand and scheduled deliveries during the next 30 days
- `inventory_value`: On-hand quantity multiplied by the material's unit cost
- `stockout_risk_flag`: Rule-based indicator showing whether projected
  inventory is expected to fall below zero during the next 30 days

The days-of-supply measure will be calculated as:

days of supply = on-hand quantity / average daily demand during the prior
30 days

Materials with no recent demand will have a missing days-of-supply value
rather than an infinite value. These materials will be retained and
identified separately so that no recent demand is not confused with missing
or invalid data.

The projected inventory measure will be calculated as:

projected inventory = on-hand quantity + expected receipts - forecast demand

Only purchase orders expected to arrive by the end of the 30-day forecast
window will be included in expected receipts.

Inventory and demand fields will be used primarily to estimate the potential
operational impact of a late delivery. They will not be assumed to cause a
supplier to deliver late.

## Current Non-Goals

The first version will not attempt to simulate:

- A complete manufacturing network
- Bills of material
- Real transportation routes
- Real-time scoring
- Causal effects of mitigation actions
- Production-scale model deployment