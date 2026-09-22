# Project Specification

## Business Problem

Supply chain teams need to identify which open purchase-order lines are at
risk of arriving materially late.

Earlier identification allows purchasing and supply chain teams to
investigate supplier constraints, review inventory exposure, evaluate
alternate sourcing, and prioritize mitigation activity before the promised
delivery date.

Because not every late order creates the same operational consequence, the
probability of lateness will be evaluated separately from the potential
business impact of the delay.

## Unit of Analysis

One purchase-order line evaluated at a defined scoring date.

Each purchase-order line represents an order for one material from one
approved supplier. A supplier may provide multiple materials, and a material
may be associated with multiple approved suppliers.

## Prediction Target

Predict the probability that a purchase-order line will arrive more than
7 calendar days after its promised delivery date.

All purchase-order lines are assumed to eventually be delivered in full.
Partial deliveries, shortages, cancellations, and undelivered orders are
outside the scope of the initial project version.

## Target Definition

The late-delivery target is defined as:

- `late_delivery_target = 1` when the actual delivery date is more than
  7 calendar days after the promised delivery date.
- `late_delivery_target = 0` when the actual delivery date is no more than
  7 calendar days after the promised delivery date.

The target is calculated only after the delivery outcome is known. The
actual delivery date, number of late days, and late-delivery target will not
be available to the model at the scoring date.

## Prediction Timing

Each purchase-order line will receive one primary prediction when the
purchase order is created.

The scoring date will equal the purchase-order date:

scoring date = purchase-order date

At the scoring date, the company knows the supplier, material, ordered
quantity, order value, promised delivery date, quoted lead time, and other
purchase-order characteristics.

Historical delivery features must be calculated only from purchase-order
lines with actual delivery dates on or before the scoring date. The current
order's actual delivery date, number of late days, and final delivery outcome
are not known at the scoring date and will not be used as predictors.

This point-in-time design replicates the business question:

> At the time this purchase order is created, what is the probability that
> the order will arrive more than 7 calendar days after its promised delivery
> date?

The initial proof of concept will produce one prediction per purchase-order
line. A production implementation could rescore open orders periodically as
new supplier communications, shipment milestones, inventory conditions, or
other operational information become available.

## Primary Decision

Which newly created purchase-order lines have the highest late-delivery risk
and should receive additional monitoring or mitigation planning?

## Analytical Outputs

The project will generate:

- A predicted probability that each purchase-order line will arrive more than
  7 calendar days late
- A risk tier based on the predicted probability
- An operational impact score calculated separately from the probability
  model
- An overall mitigation priority
- The primary factors contributing to each prediction
- A prioritized open purchase-order watchlist

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
- Approved supplier-material relationships
- Purchase-order and delivery history
- Purchase-order-level scoring records
- Time-aware historical delivery-performance features
- Logistic regression and XGBoost model evaluation
- A separate operational impact framework
- Monthly inventory and demand snapshots
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
- `active_flag`: Indicates whether the supplier is currently active

The synthetic-data generator will assign each supplier a latent reliability
parameter to create persistent differences in delivery performance.

This parameter represents an unobserved characteristic of the synthetic
simulation. It will not be exported as part of the supplier master, provided
to the predictive model, or displayed in the application. The model must
estimate supplier reliability from observable historical delivery
performance.

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

The combination of `supplier_id` and `material_id` must be unique in this
table.

The `sourcing_allocation` values for all active suppliers associated with a
material should total approximately 1.0. A material with one active supplier
will therefore have a sourcing allocation of 1.0.

The synthetic-data generator will assign each supplier-material relationship
a latent relationship effect to create realistic differences in delivery
performance across materials supplied by the same supplier.

This parameter is part of the hidden simulation process. It will not be
exported as a business field, provided to the predictive model, or displayed
in the application.

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

The purchase-order and delivery history table contains both information
available at prediction time and outcomes observed after delivery.

The actual delivery date, late days, and late-delivery flag will be used to
construct historical features for prior completed orders and to define the
target for the order being predicted. They will not be used as future-known
predictors for that order.

### Purchase-Order Scoring Dataset

The purchase-order scoring dataset will contain one row per purchase-order
line.

The scoring dataset is an analytical table created from the synthetic source
tables rather than a raw business-system table.

Planned fields will include:

- `scoring_record_id`: Unique identifier for the analytical record
- `scoring_date`: Date on which the purchase-order risk prediction is made,
  equal to the purchase-order date in the initial proof of concept
- `purchase_order_line_id`: Purchase-order line being evaluated
- `supplier_material_id`: Approved supplier-material relationship
- `supplier_id`: Supplier associated with the order
- `material_id`: Material associated with the order
- `order_date`: Date on which the purchase order was placed
- `promised_delivery_date`: Supplier-committed delivery date
- `actual_delivery_date`: Final delivery date retained only for target
  construction and retrospective model evaluation
- `planned_lead_time_days`: Number of calendar days between the purchase-order
  date and promised delivery date
- `ordered_quantity`: Quantity ordered
- `order_value`: Financial value of the purchase-order line
- `order_size_ratio`: Ordered quantity divided by the standard order quantity
  for the supplier-material relationship
- `quoted_lead_time_days`: Expected lead time for the supplier-material
  relationship
- Historical supplier-material delivery-performance features calculated
  using only deliveries completed on or before the purchase-order date
- Supplier workload features calculated from purchase orders visible on the
  purchase-order date
- `late_delivery_target`: Indicator showing whether the order ultimately
  arrived more than 7 calendar days after the promised delivery date

The actual delivery date and late-delivery target will be retained for model
development and evaluation but excluded from predictor inputs.

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

When calculating operational impact for a purchase-order line, the
application will use the most recent inventory and demand snapshot available
on or before the purchase-order date.

Inventory snapshots will not be used as predictors of supplier lateness.
They will be used only in the separate operational impact calculation.

## Current Non-Goals

The first version will not attempt to simulate:

- A complete manufacturing network
- Bills of material
- Real transportation routes
- Real-time scoring
- Causal effects of mitigation actions
- Production-scale model deployment