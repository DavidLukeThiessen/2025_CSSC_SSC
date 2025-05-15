#Libraries
library(nnet)
library(readxl)
library(pROC)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

af_data <- read_xlsx("synthetic_data_stats_competition_2025_final.xlsx")

# Run clean data.R

#Decision tree (180 days)
```{r}
tree_model <- rpart(afib_by_cutoff ~ ., 
                    data = df180_cleaned, 
                    method = "class", 
                    control = rpart.control(cp = 0.00079))  #cp = complexity parameter

rpart.plot(tree_model, type = 3, extra = 101, under = TRUE) 

important_vars <- names(tree_model$variable.importance)
print(important_vars)

df180_full <- df180_cleaned[, colSums(is.na(df180_cleaned)) == 0]  # Keep only fully observed columns. Can be removed if we clean data in other file.
```

# 180 Days NN With ECG
```{r}
library(pROC)

vars_to_use <- c("ecg_resting_qtc", "demographics_age_index_ecg", "ecg_resting_pr", "ecg_resting_hr", "observed_potassium_peri", "diabetes_combined", "observed_ldl_peri_highest", "ecg_resting_qrs", "event_cv_cns_tia_icd10_prior", "observed_hdl_peri_lowest", "observed_inr_peri", "hypertension_icd10", "observed_albumin_peri", "observed_urea_peri", "observed_urine_alb_cr_ratio_peri", "dyslipidemia_combined", "observed_sodium_peri", "observed_creatinine_peri", "aortic_aneurysm_icd10", "observed_tchol_peri_highest", "observed_bilirubin_total_peri", "observed_tsh_peri", "demographics_birth_sex", "ecg_resting_paced", "cancer_any_icd10", "observed_chloride_peri", "observed_hga1c_peri_highest", "observed_iron_peri", "observed_esr_peri", "observed_hct_peri", "observed_hgb_peri", "observed_plt_peri", "observed_rdw_peri", "observed_wbc_peri", "observed_glucose_fasting_peri_highest", "event_cv_cad_acs_other_icd10_prior", "ecg_resting_incomplete_LBBB", "ecg_resting_LBBB", "cabg_prior", "ecg_resting_RBBB", "pericarditis_icd10_prior", "ecg_resting_LPFB", "rheumatoid_arthritis_icd10", "ecg_resting_intraventricular_conduction_delay", "event_cv_ep_sca_survived_icd10_cci_prior")

af_data_clean <- df180_cleaned[complete.cases(df180[, vars_to_use]), ]

set.seed(123)
train_index <- createDataPartition(af_data_clean$afib_by_cutoff, p = 0.8, list = FALSE)
train_data <- af_data_clean[train_index, ]
test_data  <- af_data_clean[-train_index, ]

nn_formula <- as.formula(
  paste("afib_by_cutoff ~", paste(vars_to_use, collapse = " + "))
)

nn_model <- nnet(nn_formula,
                 data = train_data,
                 size = 5,
                 decay = 0.01,
                 maxit = 500,
                 linout = FALSE)

predicted_probs_test <- predict(nn_model, test_data, type = "raw")

predicted_classes_test <- ifelse(predicted_probs_test > 0.1, 1, 0)

confusionMatrix(factor(predicted_classes_test), 
                factor(test_data$afib_by_cutoff), 
                positive = "1")

roc_obj <- pROC::roc(test_data$afib_by_cutoff, predicted_probs_test)
plot(roc_obj, col = "blue")
pROC::auc(roc_obj)

#precision recall curve
library(PRROC)
pr_NN_180 <- pr.curve(
  scores.class0 = predicted_probs_test[test_data$afib_by_cutoff == 1],
  scores.class1 = predicted_probs_test[test_data$afib_by_cutoff == 0],
  curve = TRUE)

plot(pr_NN_180, main = "Precision-Recall Curve - NN scaled 180 days ", col = "red")

# Calibration Curve

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
NN_calib_180 <- get_calib_data(predicted_probs_test, test_data$afib_by_cutoff)

ggplot(NN_calib_180, aes(x = bin_midpoint, y = observed)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - NN 180 days", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()
```

# Decision tree (180 days) NO ECG
```{r}
library(dplyr)
library(caret)
ecg_remove <- c("ecg_resting_hr", "ecg_resting_pr", "ecg_resting_qrs", "ecg_resting_qtc", "ecg_resting_afib", "ecg_resting_aflutter", "ecg_resting_paced", "ecg_resting_bigeminy", "ecg_resting_LBBB", "ecg_resting_RBBB", "ecg_resting_incomplete_LBBB", "ecg_resting_incomplete_RBBB", "ecg_resting_LAFB", "ecg_resting_LPFB", "ecg_resting_bifascicular_block", "ecg_resting_trifascicular_block", "ecg_resting_intraventricular_conduction_delay")
df180_cleaned_noecg <- dplyr::select(df180_cleaned, -dplyr::all_of(ecg_remove))


library(rpart)
library(rpart.plot)

tree_model <- rpart(afib_by_cutoff ~ ., 
                    data = df180_cleaned_noecg, 
                    method = "class", 
                    control = rpart.control(cp = 0.00079))  #cp = complexity parameter

rpart.plot(tree_model, type = 3, extra = 101, under = TRUE) 

important_vars <- names(tree_model$variable.importance)
print(important_vars)

df180_full <- df180_cleaned_noecg[, colSums(is.na(df180_cleaned_noecg)) == 0]  # Keep only fully observed columns
```

# 180 Days NN NO ECG
```{r}
library(nnet)
vars_to_use <- c("demographics_age_index_ecg", "diabetes_combined", "event_cv_cns_tia_icd10_prior", "observed_ldl_peri_highest", "demographics_birth_sex", "observed_creatinine_peri", "event_cv_cad_acs_unstable_angina_icd10_prior", "observed_hdl_peri_lowest", "observed_tchol_peri_highest", "hypertension_icd10", "observed_aspartate_transaminase_peri", "observed_troponin_t_hs_peri_highest", "observed_alkaline_phophatase_peri", "observed_ck_peri", "dyslipidemia_combined", "observed_ferritin_peri", "event_cv_ep_sca_survived_icd10_cci_prior", "event_cv_cns_stroke_ischemic_icd10_prior", "lvad_cci_prior", "obstructive_sleep_apnea_icd10", "pacemaker_permanent_cci_prior")

af_data_clean <- df180_cleaned_noecg[complete.cases(df180_cleaned_noecg[, vars_to_use]), ]

set.seed(123)
train_index <- createDataPartition(af_data_clean$afib_by_cutoff, p = 0.8, list = FALSE)
train_data <- af_data_clean[train_index, ]
test_data  <- af_data_clean[-train_index, ]

nn_formula <- as.formula(
  paste("afib_by_cutoff ~", paste(vars_to_use, collapse = " + "))
)

nn_model <- nnet(nn_formula,
                 data = train_data,
                 size = 5,
                 decay = 0.01,
                 maxit = 500,
                 linout = FALSE)

predicted_probs_test <- as.vector(predict(nn_model, test_data, type = "raw"))

predicted_classes_test <- ifelse(predicted_probs_test > 0.1, 1, 0)

caret::confusionMatrix(factor(predicted_classes_test), 
                factor(test_data$afib_by_cutoff), 
                positive = "1")

roc_obj <- pROC::roc(test_data$afib_by_cutoff, predicted_probs_test)
plot(roc_obj, col = "blue")
pROC::auc(roc_obj)

#precision recall curve
library(PRROC)
pr_NN_180_NOECG <- pr.curve(
  scores.class0 = predicted_probs_test[test_data$afib_by_cutoff == 1],
  scores.class1 = predicted_probs_test[test_data$afib_by_cutoff == 0],
  curve = TRUE)

plot(pr_NN_180_NOECG, main = "Precision-Recall Curve - NN scaled 180 days ", col = "red")

# Calibration Curve

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
NN_calib_180_NOECG <- get_calib_data(predicted_probs_test, test_data$afib_by_cutoff)

ggplot(NN_calib_180, aes(x = bin_midpoint, y = observed)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - NN 180 days", x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()
```

# Calibration Curves and PR Together
```{r}
NN_calib_180_NOECG$model <- "NN Calibration 180 No ECG"
NN_calib_180$model <- "NN Calibration 180 ECG"

combined_calib <- rbind(NN_calib_180_NOECG, NN_calib_180)

ggplot(combined_calib, aes(x = bin_midpoint, y = observed, color = model)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Curve - Neural Network", x = "Predicted Probability", y = "Observed Proportion") +
  xlim(0,0.08) + 
  ylim(0,0.01)+
  theme_minimal()

plot(pr_NN_180_NOECG, col = "blue", main = "PR Curve - Neural Network")
lines(pr_NN_180$curve[,1:2], col = "darkgreen")
legend("topright", legend = c("Without ECG", "With ECG"), col = c("blue", "darkgreen"), lwd = 2.5)
```


