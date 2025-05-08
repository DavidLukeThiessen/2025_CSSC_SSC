# This will be a very basic dummy model that always outputs the average event rate.
# Only useful for testing the other code or as a baseline.
# TODO: Make this nice

preds_180_naive <- mean(df180_cleaned$afib_by_cutoff)
preds_365_naive <- mean(df365_cleaned$afib_by_cutoff)
