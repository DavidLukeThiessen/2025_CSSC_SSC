# This will be a very basic dummy model that always outputs the average event rate.
# Only useful for testing the other code or as a baseline.
# TODO: Make this nice

preds_180_naive <- rep(mean(train_data_180$afib_by_cutoff), 
                       times = nrow(test_data_180))
preds_365_naive <- rep(mean(train_data_365$afib_by_cutoff), 
                       times = nrow(test_data_365))
