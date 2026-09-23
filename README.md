# Supplier Risk Intelligence

A synthetic, end-to-end supplier-risk analytics project that uses point-in-time feature engineering, chronological model validation, logistic regression, operational-impact scoring, and a capacity-constrained watchlist to prioritize purchase-order mitigation activity.

## Project Objective

The project predicts, when a purchase order is created, the probability that the purchase-order line will arrive more than seven calendar days after its promised delivery date.

The model ranks orders by late-delivery risk. The highest-risk 20% form the active review population. A separate operational-impact score then determines how urgently each reviewed order should be handled.

The final reporting layer will be a Power BI decision-support report.

## Why This Project Exists

A late order is not automatically an important order, and a high-impact order is not automatically likely to be late.

This project deliberately separates:

```text
Late-delivery probability = likelihood
Operational-impact score  = potential consequence
Intervention priority     = locked risk and impact policy
```

The result is a transparent workflow that helps answer:

> Which newly created orders should a capacity-constrained supplier-risk team review first?

## Data Statement

All data are synthetic and were generated specifically for this portfolio project.

The repository contains no employer, client, or prospective-employer data, code, business rules, or proprietary information.

Synthetic results demonstrate analytical design and implementation. They do not claim expected performance on real procurement data.

## Implemented Workflow

```text
Synthetic source tables
        ↓
Point-in-time purchase-order scoring dataset
        ↓
Chronological warm-up, training, validation, and test periods
        ↓
Logistic regression and XGBoost comparison
        ↓
Locked logistic model and top-20% review-capacity policy
        ↓
Separate 100-point operational-impact rubric
        ↓
Prioritized supplier-risk watchlist
        ↓
Final one-time out-of-time test evaluation
        ↓
Deployment refit and Power BI outputs
```

## Key Analytical Controls

- Each order is scored on its purchase-order date.
- Historical delivery features use only deliveries completed by the scoring date.
- Supplier workload uses only orders visible by the scoring date.
- Hidden simulation fields are never exported or supplied to the model.
- Future delivery outcomes are excluded from predictors.
- Chronological periods are used instead of random train-test splits.
- Model and decision policies were locked before final test evaluation.
- Final test results are documented without post-test tuning.

## Data and Features

The synthetic workflow generates:

- Supplier master data
- Material master data
- Approved supplier-material relationships
- Purchase-order and delivery history
- Point-in-time scoring records

The locked predictive dataset contains 40 fields covering:

- Current-order characteristics
- Promised-delivery seasonality
- Supplier-material historical performance
- Supplier-wide historical performance
- Supplier workload
- Limited-history indicators

The scoring dataset contains 8,699 purchase-order lines from January 1, 2023 through August 31, 2026. Approximately 12% arrive more than seven days after their promised delivery dates.

## Chronological Evaluation Design

| Period | Dates | Records | Purpose |
|---|---|---:|---|
| Warm-up | 2023-01-01 to 2023-12-31 | 1,915 | Historical context only |
| Training | 2024-01-01 to 2025-10-31 | 4,660 | Model and preprocessing estimation |
| Validation | 2025-11-01 to 2026-02-28 | 836 | Model comparison and policy selection |
| Test | 2026-03-01 to 2026-08-31 | 1,288 | Final one-time evaluation |

Warm-up records are not used to estimate or evaluate the model. Their completed deliveries contribute to later historical features.

## Model Selection

Logistic regression and 12 controlled XGBoost candidates were compared on the validation period.

| Model | ROC AUC | PR AUC | Log Loss | Brier Score |
|---|---:|---:|---:|---:|
| Logistic regression | 0.717 | 0.292 | 0.373 | 0.113 |
| Selected XGBoost | 0.708 | 0.297 | 0.375 | 0.113 |

Logistic regression was selected because XGBoost's 0.005 PR AUC improvement did not offset its lower ROC AUC, slightly worse probability metrics, and additional complexity.

The project therefore demonstrates a deliberate choice of the simpler model when the nonlinear alternative did not provide a meaningful improvement.

## Final Test Results

After the predictor set, preprocessing, model family, review capacity, impact rubric, and watchlist logic were locked, the logistic workflow was refit on the combined training and validation periods and evaluated once on the later test period.

| Metric | Final Test Result |
|---|---:|
| ROC AUC | 0.692 |
| PR AUC | 0.211 |
| Log loss | 0.336 |
| Brier score | 0.0976 |
| Late-delivery rate | 11.5% |

The lower test discrimination relative to validation is reported transparently. No model or policy changes are made in response to the test results.

## Locked Review-Capacity Policy

The active watchlist contains the highest-risk 20% of scored orders.

The rule is rank-based rather than tied to a fixed probability threshold, allowing the workflow to maintain a stable review workload after refitting or across changing scoring populations.

| Measure | Validation | Final Test |
|---|---:|---:|
| Orders reviewed | 168 | 258 |
| Late orders identified | 48 | 56 |
| Recall | 40.3% | 37.8% |
| Precision | 28.6% | 21.7% |
| Baseline late rate | 14.2% | 11.5% |
| Lift over baseline | 2.01x | 1.89x |

On the final test period, reviewing one in five orders captured 37.8% of materially late deliveries and produced a reviewed population with 1.89 times the portfolio's baseline late rate.

## Operational-Impact Score

The project uses a separate 100-point impact rubric:

| Component | Maximum Points |
|---|---:|
| Material criticality | 35 |
| Limited approved-supplier availability | 25 |
| Sourcing allocation | 20 |
| Order value | 20 |

Order-value bands use thresholds learned from the chronological training period and frozen for later scoring.

Impact tiers are:

- Low: below 40
- Moderate: 40 to below 60
- High: 60 to below 80
- Very High: 80 to 100

This is a transparent prototype policy, not a statistically estimated impact model.

## Intervention Priorities

The locked watchlist logic is:

1. **Immediate Mitigation**: top-20% risk and Very High impact
2. **Priority Review**: top-20% risk and High impact
3. **Standard Risk Review**: top-20% risk and Moderate or Low impact
4. **Impact Monitoring**: outside top-20% risk and Very High impact
5. **Routine Monitoring**: all remaining orders

The final test active watchlist contained:

| Priority | Orders | Late Orders | Actual Late Rate |
|---|---:|---:|---:|
| Immediate Mitigation | 90 | 23 | 25.6% |
| Priority Review | 87 | 18 | 20.7% |
| Standard Risk Review | 81 | 15 | 18.5% |

Impact Monitoring preserves visibility to highly consequential orders without expanding the active predictive review workload.

## Explanatory Analysis

Separate linear probability and logistic models use a concise 12-predictor specification to describe conditional associations.

Selected findings include:

- An order 50% larger than the normal supplier-material order was associated with a 6.6-percentage-point increase in average predicted late-delivery probability.
- Ten days of promised lead-time compression was associated with a 5.8-percentage-point increase.
- Higher supplier-wide historical late-quantity performance was associated with a 2.9-percentage-point increase when comparing representative lower and upper values.
- Higher recent supplier workload pressure had a smaller positive association.
- Promised-delivery seasonality was statistically detectable.

These are conditional associations within synthetic data and are not interpreted as causal effects.

## Power BI Reporting Plan

The final Power BI report will contain three focused pages.

### 1. Supplier Risk Overview

- Orders scored
- Active watchlist count
- Expected late deliveries
- Immediate Mitigation count
- Risk versus impact view
- Supplier and material summaries

### 2. Prioritized Watchlist

- Purchase-order line
- Supplier
- Material
- Promised-delivery date
- Predicted late probability
- Risk tier
- Impact score and tier
- Intervention priority
- Recommended action

### 3. Model Performance and Policy

- Validation and test metrics
- Precision, recall, and lift at review capacities
- Risk-decile performance
- Cumulative gains
- Logistic versus XGBoost comparison
- Explanatory findings and limitations

## Repository Structure

```text
R/
├── data_generation/
│   ├── generate_supplier_master.R
│   ├── generate_material_master.R
│   ├── generate_supplier_material_relationships.R
│   └── generate_purchase_order_history.R
├── feature_engineering/
│   ├── build_purchase_order_scoring_dataset.R
│   └── profile_purchase_order_scoring_dataset.R
├── modeling/
│   ├── create_order_level_model_splits.R
│   ├── prepare_order_level_model_data.R
│   ├── fit_logistic_baseline.R
│   ├── fit_xgboost_candidates.R
│   ├── fit_explanatory_models.R
│   ├── evaluate_logistic_review_capacity.R
│   └── evaluate_final_logistic_test.R
└── decision_support/
    ├── build_operational_impact_scores.R
    └── build_validation_watchlist.R
```

## Reproduction Order

Run the scripts from the project root in this order:

```r
source("R/data_generation/generate_purchase_order_history.R")
source("R/feature_engineering/build_purchase_order_scoring_dataset.R")
source("R/feature_engineering/profile_purchase_order_scoring_dataset.R")
source("R/modeling/create_order_level_model_splits.R")
source("R/modeling/prepare_order_level_model_data.R")
source("R/modeling/fit_logistic_baseline.R")
source("R/modeling/fit_explanatory_models.R")
source("R/modeling/fit_xgboost_candidates.R")
source("R/modeling/evaluate_logistic_review_capacity.R")
source("R/decision_support/build_operational_impact_scores.R")
source("R/decision_support/build_validation_watchlist.R")
```

The following script performs the final test evaluation and should be run only after all model and policy decisions are locked:

```r
source("R/modeling/evaluate_final_logistic_test.R")
```

## Current Status

Completed:

- Synthetic data generation
- Point-in-time feature engineering
- Chronological data splitting
- Logistic and XGBoost validation comparison
- Explanatory modeling
- Capacity-based validation
- Locked top-20% review policy
- Locked operational-impact rubric
- Locked watchlist logic
- One-time final test evaluation

Remaining:

- Refit the locked logistic workflow on training, validation, and test data for deployment
- Generate future or current synthetic scoring records
- Export final deployment watchlist tables
- Build the Power BI report
- Add report screenshots and interview demonstration materials

## Limitations

- All data are synthetic.
- The target covers full deliveries only.
- The proof of concept scores orders once at creation.
- Inventory and demand exposure are deferred from the current impact rubric.
- The impact score is a transparent business rubric, not an estimated model.
- Test performance does not establish expected performance on real procurement data.
- A production implementation would require stakeholder validation, source-system integration, prospective testing, monitoring, access controls, and governance.
