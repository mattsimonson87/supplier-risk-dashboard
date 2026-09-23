# Project Specification

## Project Summary

Supplier Risk Intelligence is a synthetic, end-to-end supply-chain analytics project that identifies purchase-order lines at elevated risk of arriving materially late and prioritizes a limited mitigation workload using a separate operational-impact framework.

The project predicts, when a purchase order is created, the probability that the purchase-order line will arrive more than seven calendar days after its promised delivery date.

The analytical workflow combines:

- Reproducible synthetic supply-chain data
- Point-in-time feature engineering
- Chronological model development and evaluation
- Logistic regression and XGBoost comparison
- Explanatory linear probability and logistic models
- Capacity-constrained risk review
- A transparent operational-impact rubric
- A prioritized supplier-risk watchlist
- Power BI-ready decision-support outputs

No employer, client, or prospective-employer data or proprietary logic is used.

## Business Problem

Supply-chain teams need to identify, when a purchase order is created, which purchase-order lines are most likely to arrive materially late.

Earlier identification allows purchasing and supply-chain teams to investigate supplier constraints, confirm delivery commitments, evaluate sourcing options, and prioritize mitigation activity before the promised delivery date.

Because review capacity is limited, the project ranks newly created purchase-order lines by predicted late-delivery probability. The highest-risk 20% of orders form the active risk-review population.

Late-delivery probability is evaluated separately from the potential operational consequence of the delay. A transparent operational-impact rubric then determines intervention priority within the active watchlist.

The project addresses two related but distinct questions:

1. How likely is each purchase-order line to arrive more than seven calendar days after its promised delivery date?
2. If the order is materially late, how consequential could the delay be?

## Unit of Analysis

The unit of analysis is one purchase-order line evaluated at a defined scoring date.

Each purchase-order line represents an order for one material from one approved supplier. A supplier may provide multiple materials, and a material may be associated with multiple approved suppliers.

In the current synthetic process, each purchase order contains one line. The data model retains separate purchase-order and purchase-order-line identifiers so that a future implementation can support multi-line orders.

## Prediction Target

The model predicts the probability that a purchase-order line will arrive more than seven calendar days after its promised delivery date.

The binary target is defined as:

- `late_delivery_target = 1` when actual delivery occurs more than seven calendar days after the promised delivery date.
- `late_delivery_target = 0` when actual delivery occurs no more than seven calendar days after the promised delivery date.

All purchase-order lines are assumed to eventually be delivered in full. Partial deliveries, shortages, cancellations, and undelivered orders are outside the implemented scope.

The target is calculated only after the delivery outcome is known. Actual delivery date, actual late days, and the final target are retained for retrospective development and evaluation but are prohibited as predictors for the order being scored.

## Prediction Timing

Each purchase-order line receives one primary prediction when the order is created.

```text
scoring date = purchase-order date
```

At the scoring date, the workflow can use supplier, material, quantity, promised-delivery, quoted-lead-time, price, sourcing, and other current-order information.

Historical delivery features include only prior orders with actual delivery dates on or before the scoring date. Supplier workload features include only purchase orders visible by that date.

The proof of concept assumes end-of-day batch scoring, so orders created on the same date are visible in that date's workload totals.

The design answers:

> At the time this purchase order is created, what is the probability that it will arrive more than seven calendar days after its promised delivery date?

A production implementation could rescore open orders as new supplier communications, shipment milestones, inventory conditions, or other operational information become available.

## Primary Decision

Which newly created purchase-order lines fall within the highest-risk 20% and therefore warrant additional review?

Within that capacity-constrained population, which orders require the greatest intervention urgency based on their potential operational impact?

## Analytical Outputs

The implemented workflow generates:

- Predicted late-delivery probability
- Risk rank
- Locked top-20% active-review flag
- Reporting risk tier
- Separate operational-impact score from 0 to 100
- Fixed operational-impact tier
- Intervention-priority category
- Recommended action
- Prioritized supplier-risk watchlist
- Capacity-based performance summaries
- Risk-decile and cumulative-gains outputs
- Power BI-ready reporting tables

Actual delivery outcomes are retained for retrospective evaluation but do not determine risk rank, impact score, intervention priority, or recommended action at scoring time.

## Synthetic Data Approach

All suppliers, materials, supplier-material relationships, purchase orders, delivery outcomes, and analytical records are synthetically generated.

The synthetic outcome mechanism uses a calibrated logistic probability process containing:

- Persistent hidden supplier reliability
- Persistent hidden supplier-material effects
- Order size above the normal supplier-material quantity
- Recent supplier workload pressure
- Promised lead-time compression
- Promised-delivery seasonality
- Random outcome variation

Hidden simulation fields are used only to generate synthetic outcomes. They are not exported, supplied to predictive models, or displayed in reporting outputs.

The final generated dataset contains 8,699 purchase-order lines with an overall materially late rate of approximately 12%.

## Source Data Tables

### Supplier Master

The supplier master contains one row per fictional supplier. Fields include:

- `supplier_id`
- `supplier_name`
- `supplier_region`
- `supplier_tier`
- `standard_lead_time_days`
- `active_flag`

The generator assigns each supplier a latent reliability parameter. This hidden parameter is not exported. Observable supplier-wide historical performance must act as its business-available proxy.

### Material Master

The material master contains one row per fictional material. Fields include:

- `material_id`
- `material_name`
- `material_category`
- `material_criticality`
- `unit_cost`
- `average_daily_demand`
- `demand_variability`
- `safety_stock_days`
- `approved_supplier_count`
- `active_flag`

A material may be associated with multiple approved suppliers.

### Supplier-Material Relationships

This table contains one row per approved supplier and material pairing. Fields include:

- `supplier_material_id`
- `supplier_id`
- `material_id`
- `relationship_start_date`
- `relationship_end_date`
- `relationship_status`
- `quoted_lead_time_days`
- `minimum_order_quantity`
- `standard_order_quantity`
- `sourcing_allocation`
- `preferred_supplier_flag`
- `supplier_priority_rank`
- `unit_price`

The combination of supplier and material is unique. Active sourcing allocations for a material total approximately 1.0.

The generator assigns each relationship a hidden effect that is not exported or used as a predictor.

### Purchase-Order and Delivery History

The history table contains one row per purchase-order line. Fields include:

- `purchase_order_line_id`
- `purchase_order_id`
- `supplier_material_id`
- `supplier_id`
- `material_id`
- `order_date`
- `promised_delivery_date`
- `actual_delivery_date`
- `ordered_quantity`
- `unit_price`
- `order_value`
- `late_days`
- `late_delivery_flag`

The table contains both information available at order creation and retrospective delivery outcomes. Post-delivery fields are used only to create historical features for later orders and to evaluate the current order after its outcome is known.

## Purchase-Order Scoring Dataset

The scoring dataset contains one analytical record per purchase-order line.

Every order is scored on its purchase-order date. Historical delivery features include only orders completed on or before that date. Supplier workload includes only orders visible by the scoring date.

The scoring dataset includes:

- Analytical and business identifiers
- Supplier and material reporting attributes
- Order and promised-delivery dates
- Planned and quoted lead times
- Lead-time pressure
- Ordered quantity and order-size ratio
- Supplier-material completed-order history
- Supplier-material reliability and delay-severity history
- Supplier-wide completed-order history
- Supplier-wide reliability and delay-severity history
- Open-order and trailing supplier workload
- Promised-delivery seasonality
- Limited-history indicators
- Operational-impact fields
- Retrospective outcomes for development and evaluation

The predictive model uses a locked 40-predictor subset. Identifiers, post-delivery outcomes, and operational-impact fields are excluded from predictive inputs.

Hidden supplier reliability, hidden relationship effects, and latent late-delivery probability are explicitly prohibited from the exported scoring dataset.

## Point-in-Time Controls

The implemented controls include:

- `scoring_date` equals `order_date`.
- Historical delivery features require `actual_delivery_date <= scoring_date`.
- The current order is excluded from its own completed-delivery history.
- Workload uses only orders created by the scoring date.
- Hidden simulation fields are excluded from exported analytical data.
- Future outcome fields are excluded from model inputs.
- Preprocessing parameters are learned only from the applicable fitting population.
- Chronological periods are used instead of random train-test splits.

## Predictive Features

The locked predictive dataset contains 40 fields representing:

- Current-order characteristics
- Promised-delivery seasonality
- Supplier-material historical volume and reliability
- Supplier-material delay severity
- Supplier-wide historical reliability
- Supplier workload
- Limited-history indicators

Operational-impact fields are deliberately excluded from the probability model so likelihood and consequence remain distinct.

## Chronological Evaluation Design

The scoring dataset spans January 1, 2023 through August 31, 2026.

The chronological periods are:

- Warm-up: January 1, 2023 through December 31, 2023
- Training: January 1, 2024 through October 31, 2025
- Validation: November 1, 2025 through February 28, 2026
- Test: March 1, 2026 through August 31, 2026

The warm-up period provides historical context but is excluded from model estimation and evaluation.

The training period was used to estimate candidate models and preprocessing. The validation period was used to compare model families, improve observable feature engineering, and lock the review-capacity and decision-support policies.

After all analytical and operating decisions were locked, the selected logistic workflow was refit on training plus validation and evaluated once on the final test period.

The next model artifact will be a deployment model refit on training, validation, and test data. That model will use all post-warm-up history for future scoring but will not provide another unbiased performance estimate.

## Modeling Approach

### Predictive Model Comparison

An unpenalized logistic regression and 12 controlled XGBoost candidates were compared on the chronological validation period.

Model selection considered:

- ROC AUC
- Precision-recall AUC
- Log loss
- Brier score
- Performance at fixed review capacities
- Interpretability and implementation complexity

Validation performance was:

| Model | ROC AUC | PR AUC | Log Loss | Brier Score |
|---|---:|---:|---:|---:|
| Logistic regression | 0.717 | 0.292 | 0.373 | 0.113 |
| Selected XGBoost candidate | 0.708 | 0.297 | 0.375 | 0.113 |

Logistic regression was selected because XGBoost improved PR AUC by only 0.005 while reducing ROC AUC and slightly worsening probability-accuracy measures. The small nonlinear gain did not justify the additional complexity.

### Final Test Evaluation

After the feature set, preprocessing workflow, model family, review capacity, impact rubric, and watchlist policy were locked, logistic regression was refit on 5,496 combined training and validation records.

It was evaluated once on the later 1,288-record test period.

Final test performance was:

| Metric | Result |
|---|---:|
| ROC AUC | 0.692 |
| PR AUC | 0.211 |
| Log loss | 0.336 |
| Brier score | 0.0976 |
| Test late-delivery rate | 11.5% |

The lower discrimination relative to validation is reported transparently. No model, feature, capacity, impact, or watchlist changes will be made in response to the final test result.

### Explanatory Models

A separate linear probability model and logistic regression were estimated on the chronological training period using a concise 12-predictor specification.

The linear probability model uses HC3 robust standard errors. The explanatory logistic model reports robust odds ratios, average marginal effects, and selected probability contrasts.

Key conditional associations include:

- An order 50% larger than the normal supplier-material order was associated with a 6.6-percentage-point increase in average predicted late-delivery probability.
- Ten days of promised lead-time compression was associated with a 5.8-percentage-point increase.
- Moving from lower to higher supplier-wide historical late-quantity performance was associated with a 2.9-percentage-point increase.
- Higher recent supplier workload pressure had a smaller positive association.
- Promised-delivery seasonality was statistically detectable.

These results describe conditional associations in the synthetic data, not causal effects.

## Locked Review-Capacity Policy

The active supplier-risk watchlist contains the highest-risk 20% of newly scored orders.

The policy is rank-based rather than tied to a fixed probability threshold. This maintains a stable review workload when probability distributions change between scoring periods or after refitting.

Validation performance under the locked rule was:

- 168 of 836 orders reviewed
- 48 of 119 late orders identified
- 40.3% recall
- 28.6% precision
- 2.01 times baseline lift

Final test performance under the same unchanged rule was:

- 258 of 1,288 orders reviewed
- 56 of 148 late orders identified
- 37.8% recall
- 21.7% precision
- 1.89 times baseline lift

Alternative capacities may be displayed in Power BI for scenario analysis, but the designated operating policy and final test evaluation use the top-20% rule.

## Operational-Impact Framework

Predicted probability measures likelihood, not consequence.

A separate operational-impact rubric scores each order from 0 to 100 using information available at order creation.

The locked components are:

- Material criticality: maximum 35 points
- Approved-supplier availability: maximum 25 points
- Sourcing allocation: maximum 20 points
- Order value: maximum 20 points

Order-value scoring uses thresholds learned from the chronological training period:

- First quartile: $28,908.02
- Median: $68,280.66
- Third quartile: $125,244.00

The fixed impact tiers are:

- Low: below 40
- Moderate: 40 to below 60
- High: 60 to below 80
- Very High: 80 to 100

The rubric is a transparent prototype policy rather than a statistically estimated impact model. In a real implementation, stakeholders would validate the components, weights, and thresholds.

## Watchlist Priority Logic

Risk-review status and operational impact determine priority:

1. **Immediate Mitigation**
   - Highest-risk 20%
   - Very High impact

2. **Priority Review**
   - Highest-risk 20%
   - High impact

3. **Standard Risk Review**
   - Highest-risk 20%
   - Moderate or Low impact

4. **Impact Monitoring**
   - Outside the highest-risk 20%
   - Very High impact

5. **Routine Monitoring**
   - All remaining orders

Impact Monitoring does not expand the active predictive review workload. It preserves visibility to highly consequential orders for contingency awareness.

A probability-times-impact field may be used for visualization, but it does not determine watchlist membership or priority.

## Deferred Inventory and Demand Enhancement

Monthly inventory and demand snapshots were originally proposed for the initial impact framework. They are deferred to keep the implemented portfolio project focused and reproducible.

The current impact score uses material criticality, approved supplier count, sourcing allocation, and order value.

A future version could include:

- On-hand inventory
- Days of supply
- Forecast demand
- Expected receipts
- Projected inventory
- Safety-stock exposure
- Projected stockout timing

These fields would primarily refine the consequence of a delayed order. They would not automatically be included in the late-delivery probability model.

## Implemented Scope

The completed analytical scope includes:

- Reproducible synthetic source data
- Point-in-time purchase-order scoring records
- Supplier-material and supplier-wide historical features
- Supplier workload features
- Chronological warm-up, training, validation, and test periods
- Logistic regression and XGBoost comparison
- Explanatory linear probability and logistic models
- Robust inference, marginal effects, and probability contrasts
- Capacity-based validation analysis
- Locked top-20% review policy
- Locked operational-impact rubric
- Locked intervention-priority logic
- Validation and final-test watchlists
- One-time final test evaluation without post-test tuning
- Power BI-ready CSV outputs

## Remaining Scope

The remaining work is:

- Refit the locked logistic workflow on training, validation, and test data for deployment
- Generate a future or current synthetic scoring population
- Create final deployment watchlist outputs
- Build the Power BI decision-support report
- Finalize project documentation, screenshots, and interview materials

## Current Non-Goals

The implemented version does not attempt to provide:

- A complete manufacturing network simulation
- Bills of material or production-line dependencies
- Real transportation routes or shipment milestones
- Partial-delivery or cancellation outcomes
- Real-time model scoring
- Causal estimates of supplier behavior or mitigation effectiveness
- A statistically estimated impact model
- Production cloud deployment
- Automated source-system integration
- Real supplier or employer data
- A claim that synthetic-data performance will transfer directly to a real procurement environment

A real implementation would require stakeholder validation, source-system integration, prospective testing, intervention-outcome collection, calibration monitoring, drift monitoring, access controls, and governance.
