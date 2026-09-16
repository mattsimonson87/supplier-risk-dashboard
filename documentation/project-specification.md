# Project Specification

## Business Problem

Supply chain teams need to identify which supplier-material relationships
are most likely to experience a late or incomplete delivery during the next
30 days.

Because not every delivery problem has the same operational consequence,
teams must also determine which predicted disruptions would have the
greatest business impact and require immediate mitigation.

## Unit of Analysis

One supplier-material relationship evaluated at a specific monthly scoring date. A supplier may provide multiple materials, and a material may be associated with multiple approved suppliers.

## Prediction Target

Predict whether an open or expected delivery for a supplier-material relationship will be late or incomplete during the 30 days following the scoring date.

## Prediction Timing

All model features must use information available on or before the scoring
date. Delivery outcomes occurring after the scoring date must not be used
as predictors.

## Primary Decision

Which supplier-material risks should supply chain teams investigate and
mitigate first?

## Analytical Outputs

The project will generate:

- A predicted disruption probability
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

## Current Non-Goals

The first version will not attempt to simulate:

- A complete manufacturing network
- Bills of material
- Real transportation routes
- Real-time scoring
- Causal effects of mitigation actions
- Production-scale model deployment