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

All supplier, material, purchase order, delivery, quality, inventory, and
demand data will be synthetically generated.

No employer, client, or prospective-employer data or proprietary logic will
be used.

## Initial Scope

The first version will include:

- Supplier master data
- Material master data
- Purchase order and delivery history
- Supplier quality events
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
- `capacity_utilization`: Estimated proportion of the supplier's available
  production capacity currently in use
- `baseline_reliability`: Underlying synthetic tendency to deliver orders
  on time
- `financial_risk_level`: Synthetic categorical indicator of financial risk
- `regional_disruption_exposure`: Synthetic measure of exposure to regional
  transportation or operational disruptions
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

## Current Non-Goals

The first version will not attempt to simulate:

- A complete manufacturing network
- Bills of material
- Real transportation routes
- Real-time scoring
- Causal effects of mitigation actions
- Production-scale model deployment