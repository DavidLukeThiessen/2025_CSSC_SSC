
library(openxlsx)
library(caret)
library(randomForest)
library(xgboost)
library(PRROC)
library(ranger)
library(dplyr)

df <- read.xlsx("ssc.xlsx")

make_afib_flag_df <- function(data, cutoff_days) {
  data %>%
    mutate(
      afib_by_cutoff = as.integer(
        !is.na(time_to_outcome_afib_aflutter_new_post) &
          time_to_outcome_afib_aflutter_new_post <= cutoff_days
      )
    )
}

make_bloodwork_flags <- function(data, ..., prefix = "observed_") {
  data %>%
    mutate(across(c(...), ~ as.integer(!is.na(.x)), .names = paste0(prefix, "{.col}")))
}

names(df) <- gsub("obstructive _sleep_apnea_icd10",
                  
                  "obstructive_sleep_apnea_icd10",
                  names(df))


df_flags <- make_bloodwork_flags(df,
                                 
                                 troponin_t_hs_peri_highest,
                                 potassium_peri,
                                 crp_high_sensitive_peri,
                                 tsh_peri,
                                 hga1c_peri_highest,
                                 glucose_fasting_peri_highest)
df180 <- make_afib_flag_df(df_flags, cutoff_days = 180)


vars <- c(
  "afib_by_cutoff", 
  "observed_troponin_t_hs_peri_highest",
  "observed_potassium_peri",
  "observed_crp_high_sensitive_peri",
  "observed_tsh_peri",
  "observed_hga1c_peri_highest",
  "observed_glucose_fasting_peri_highest",
  "demographics_age_index_ecg",          
  "demographics_birth_sex",              
  "hypertension_icd10",               
  "ecg_resting_hr"                     
)

data_model <- df180[, vars]

data_model$afib_by_cutoff <- as.factor(data_model$afib_by_cutoff)

set.seed(123)
trainIndex <- createDataPartition(data_model$afib_by_cutoff, p = 0.8, list = FALSE)
train <- data_model[trainIndex, ]
test <- data_model[-trainIndex, ]

#------- Precision Recall Curve -------

#Random Forest
rf_model <- ranger(
  afib_by_cutoff ~ ., 
  data = train,
  num.trees = 100,
  probability = TRUE,
  respect.unordered.factors = "partition",
  na.action = "na.omit"
)

rf_pred <- predict(rf_model, data = test)$predictions[, 2]

#XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(train[, -1]), label = as.numeric(train$afib_by_cutoff) - 1)
xgb_test <- xgb.DMatrix(data = as.matrix(test[, -1]), label = as.numeric(test$afib_by_cutoff) - 1)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  scale_pos_weight = sum(train$afib_by_cutoff == "0") / sum(train$afib_by_cutoff == "1")
)

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 100,
  verbose = 0
)

xgb_pred <- predict(xgb_model, xgb_test)

actual <- as.numeric(as.character(test$afib_by_cutoff))
# Precision Recall Curve for RF
pr_auc_noecg <- round(pr_rf_180_noecg$auc.integral, 4)
pr_auc_ecg   <- round(pr_rf_180_ecg$auc.integral, 4)

title_text <- paste0("PR Curve - Random Forest\n",
                     "PR AUC (Without ECG): ", pr_auc_noecg,
                     " | PR AUC (With ECG): ", pr_auc_ecg)

plot(pr_rf_180_noecg, col = "blue", main = title_text, auc.main = FALSE)
lines(pr_rf_180_ecg$curve[, 1:2], col = "darkgreen")

legend("topright", legend = c("Without ECG", "With ECG"),
       col = c("blue", "darkgreen"), lwd = 2)

# Precision Recall Curve for XGBoost
pr_auc_xgb_noecg <- round(pr_xgb_180_noecg$auc.integral, 3)
pr_auc_xgb_ecg   <- round(pr_xgb_180_ecg$auc.integral, 3)

title_xgb <- paste0("PR Curve - XGBoost\n",
                    "PR AUC (Without ECG): ", pr_auc_xgb_noecg,
                    " | PR AUC (With ECG): ", pr_auc_xgb_ecg)

plot(pr_xgb_180_noecg, col = "blue", main = title_xgb, auc.main = FALSE)
lines(pr_xgb_180_ecg$curve[, 1:2], col = "darkgreen")

legend("bottomleft", legend = c("Without ECG", "With ECG"),
       col = c("blue", "darkgreen"), lwd = 2)

# ----- Calibration Curve -------

get_calib_data <- function(pred, actual, bins = 10) {
  calib_df <- data.frame(pred = pred, actual = actual)
  calib_df$bin <- cut(calib_df$pred, breaks = quantile(calib_df$pred, probs = seq(0, 1, length.out = bins + 1)), include.lowest = TRUE)
  
  calib_summary <- calib_df %>%
    group_by(bin) %>%
    summarise(
      bin_midpoint = mean(pred),
      observed = mean(actual),
      .groups = "drop"
    )
  return(calib_summary)
}

rf_calib <- get_calib_data(rf_pred, actual)
xgb_calib <- get_calib_data(xgb_pred, actual)

# RF calibration
ggplot(rf_calib, aes(x = bin_midpoint, y = observed)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - Random Forest", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

# XGBoost calibration
ggplot(xgb_calib, aes(x = bin_midpoint, y = observed)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - XGBoost", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

# ---- Combined RF and XGBoost Calibration---
rf_calib$model <- "Random Forest"
xgb_calib$model <- "XGBoost"

combined_calib <- rbind(rf_calib, xgb_calib)

ggplot(combined_calib, aes(x = bin_midpoint, y = observed, color = model)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve Comparison", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()
