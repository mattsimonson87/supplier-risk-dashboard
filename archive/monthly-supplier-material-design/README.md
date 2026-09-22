# Archived Monthly Supplier-Material Design

These scripts represent an earlier analytical design in which one model record
was created for each supplier-material relationship at a monthly scoring date.

The original target identified whether more than 20 percent of the quantity
due during the following 30 days arrived more than 7 calendar days after its
promised delivery date.

The design was superseded by a purchase-order-level approach because an
individual open purchase-order line provides a clearer and more realistic
unit of analysis for delivery-risk prediction.

The active project predicts the probability that a specific purchase-order
line will arrive more than 7 calendar days after its promised delivery date.

These scripts are retained to document the project’s design evolution and
should not be used in the active modeling workflow.