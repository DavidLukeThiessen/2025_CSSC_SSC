get_calib_data <- function(pred, actual, bins = 10) {
  calib_df <- data.frame(pred = pred, actual = actual)
  breaks <- unique(quantile(calib_df$pred, probs = seq(0, 1, length.out = bins + 1)))
  if (length(breaks) < 3) stop("Not enough unique values to form bins.")
  calib_df$bin <- cut(calib_df$pred, breaks = breaks, include.lowest = TRUE)
  calib_df %>%
    group_by(bin) %>%
    summarise(bin_midpoint = mean(pred), observed = mean(actual), .groups = "drop")
}



# RANDOM FOREST – 180 Days With ECG 
set.seed(123)
trainIndex_rf_ecg <- createDataPartition(df180_ecg$afib_by_cutoff, p = 0.8, list = FALSE)
train_rf_180_ecg <- df180_ecg[trainIndex_rf_ecg, ]
test_rf_180_ecg <- df180_ecg[-trainIndex_rf_ecg, ]

rf_model_180_ecg <- ranger(
  afib_by_cutoff ~ ., data = train_rf_180_ecg, probability = TRUE, num.trees = 100
)

rf_pred_180_ecg <- predict(rf_model_180_ecg, data = test_rf_180_ecg)$predictions[, 2]
actual_rf_180_ecg <- test_rf_180_ecg$afib_by_cutoff

roc_rf_180_ecg <- roc(actual_rf_180_ecg, rf_pred_180_ecg)
auc_rf_180_ecg <- auc(roc_rf_180_ecg)

pr_rf_180_ecg <- pr.curve(
  scores.class0 = rf_pred_180_ecg[actual_rf_180_ecg == 1],
  scores.class1 = rf_pred_180_ecg[actual_rf_180_ecg == 0],
  curve = TRUE
)

class_rf_180_ecg <- ifelse(rf_pred_180_ecg > 0.10, 1, 0)
cm_rf_180_ecg <- confusionMatrix(as.factor(class_rf_180_ecg), as.factor(actual_rf_180_ecg))
bal_acc_rf_180_ecg <- cm_rf_180_ecg$byClass["Balanced Accuracy"]

rf_calib_180_ecg <- get_calib_data(rf_pred_180_ecg, actual_rf_180_ecg)
rf_calib_180_ecg$model <- "With ECG"

confusionMatrix(as.factor(class_rf_180_ecg), as.factor(actual_rf_180_ecg))

# RANDOM FOREST – 180 Days Without ECG
set.seed(123)
trainIndex_rf <- createDataPartition(df180_noecg$afib_by_cutoff, p = 0.8, list = FALSE)
train_rf_180_noecg <- df180_noecg[trainIndex_rf, ]
test_rf_180_noecg <- df180_noecg[-trainIndex_rf, ]

rf_model_180_noecg <- ranger(
  afib_by_cutoff ~ ., data = train_rf_180_noecg, probability = TRUE, num.trees = 100
)

rf_pred_180_noecg <- predict(rf_model_180_noecg, data = test_rf_180_noecg)$predictions[, 2]
actual_rf_180_noecg <- test_rf_180_noecg$afib_by_cutoff

roc_rf_180_noecg <- roc(actual_rf_180_noecg, rf_pred_180_noecg)
auc_rf_180_noecg <- auc(roc_rf_180_noecg)

pr_rf_180_noecg <- pr.curve(
  scores.class0 = rf_pred_180_noecg[actual_rf_180_noecg == 1],
  scores.class1 = rf_pred_180_noecg[actual_rf_180_noecg == 0],
  curve = TRUE
)

class_rf_180_noecg <- ifelse(rf_pred_180_noecg > 0.10, 1, 0)
cm_rf_180_noecg <- confusionMatrix(as.factor(class_rf_180_noecg), as.factor(actual_rf_180_noecg))
bal_acc_rf_180_noecg <- cm_rf_180_noecg$byClass["Balanced Accuracy"]

rf_calib_180_noecg <- get_calib_data(rf_pred_180_noecg, actual_rf_180_noecg)
rf_calib_180_noecg$model <- "Without ECG"

# Combined Calibration Plot
rf_calib_combined <- rbind(rf_calib_180_ecg, rf_calib_180_noecg)

ggplot(rf_calib_combined, aes(x = bin_midpoint, y = observed, color = model)) +
  geom_point() + geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - Random Forest", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

# Precision-Recall Curve Plot
plot(pr_rf_180_noecg, col = "blue", main = "PR Curve - RF With vs Without ECG")
lines(pr_rf_180_ecg$curve[,1:2], col = "darkgreen")
legend("topright", legend = c("Without ECG", "With ECG"), col = c("blue", "darkgreen"), lwd = 2)

confusionMatrix(as.factor(class_rf_180_noecg), as.factor(actual_rf_180_noecg))

# Summary Output
x <- confusionMatrix(as.factor(class_rf_180_ecg), as.factor(actual_rf_180_ecg))
y <- confusionMatrix(as.factor(class_rf_180_noecg), as.factor(actual_rf_180_noecg))

cat("Random Forest AUC (With ECG):", round(auc_rf_180_ecg, 4), "\n")
cat("Sensitivity (With ECG):", round(x$byClass["Sensitivity"], 4), "\n")
cat("Specificity (With ECG):", round(x$byClass["Specificity"], 4), "\n")
cat("Balanced Accuracy (With ECG):", round(x$byClass["Balanced Accuracy"], 4), "\n\n")

cat("Random Forest AUC (No ECG):", round(auc_rf_180_noecg, 4), "\n")
cat("Sensitivity (No ECG):", round(y$byClass["Sensitivity"], 4), "\n")
cat("Specificity (No ECG):", round(y$byClass["Specificity"], 4), "\n")
cat("Balanced Accuracy (No ECG):", round(y$byClass["Balanced Accuracy"], 4), "\n")
