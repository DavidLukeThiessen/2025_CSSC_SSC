#Libraries
library(nnet)
library(readxl)
library(pROC)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

af_data <- read_xlsx("synthetic_data_stats_competition_2025_final.xlsx")

#Decision tree (180 days)
tree_model <- rpart(afib_by_cutoff ~ ., 
                    data = df180_cleaned, 
                    method = "class", 
                    control = rpart.control(cp = 0.00079))  #cp = complexity parameter

rpart.plot(tree_model, type = 3, extra = 101, under = TRUE) 

important_vars <- names(tree_model$variable.importance)
print(important_vars)

df180_full <- df180_cleaned[, colSums(is.na(df180_cleaned)) == 0]  # Keep only fully observed columns. Can be removed if we clean data in other file.


#Decision tree (365 days)
tree_model <- rpart(afib_by_cutoff ~ ., 
                    data = df365_cleaned, 
                    method = "class", 
                    control = rpart.control(cp = 0.0008))  #cp = complexity parameter

rpart.plot(tree_model, type = 3, extra = 101, under = TRUE)

important_vars <- names(tree_model$variable.importance)
print(important_vars)

df365_full <- df365_cleaned[, colSums(is.na(df365_cleaned)) == 0]  # Keep only fully observed columns. Can be removed if we clean data in other file.

#180 Days NN
vars_to_use <- c("demographics_age_index_ecg", "diabetes_combined", "event_cv_cns_tia_icd10_prior", "observed_ptt_peri", "outcome_all_cause_death", "observed_tibc_peri", "dyslipidemia_combined", "observed_albumin_peri", "observed_creatinine_peri", "observed_glucose_fasting_peri_highest", "observed_glucose_random_peri_highest", "observed_alkaline_phophatase_peri", "cancer_any_icd10", "observed_chloride_peri", "observed_tg_peri_highest", "observed_sodium_peri", "event_cv_cad_acs_unstable_angina_icd10_prior", "hypertension_icd10", "observed_tsh_peri", "observed_potassium_peri", "observed_tchol_peri_highest", "observed_urea_peri", "pci_prior", "copd_icd10", "event_cv_hf_admission_icd10_prior", "observed_hct_peri", "observed_hgb_peri", "event_cv_cns_stroke_ischemic_icd10_prior", "event_cv_cad_acs_acute_mi_icd10_prior", "rheumatoid_arthritis_icd10", "observed_esr_peri", "observed_urine_alb_cr_ratio_peri", "event_cv_ep_vt_any_icd10_prior", "lvad_cci_prior", "obstructive_sleep_apnea_icd10", "cabg_prior", "icd_cci_prior")

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

roc_obj <- roc(test_data$afib_by_cutoff, predicted_probs_test)
plot(roc_obj, col = "blue")
auc(roc_obj)

#365 Days NN
vars_to_use2 <- c("demographics_age_index_ecg", "diabetes_combined", "event_cv_cad_acs_unstable_angina_icd10_prior", "event_cv_cns_tia_icd10_prior", "observed_alkaline_phophatase_peri", "observed_tchol_peri_highest", "observed_creatinine_peri", "outcome_all_cause_death", "hypertension_icd10", "observed_iron_peri", "observed_crp_high_sensitive_peri", "copd_icd10", "observed_tibc_peri", "observed_urine_alb_cr_ratio_peri", "event_cv_cns_stroke_ischemic_icd10_prior", "observed_hga1c_peri_highest", "pci_prior", "hcm_icd10", "lvad_cci_prior", "myocarditis_icd10_prior", "pacemaker_permanent_cci_prior", "obstructive_sleep_apnea_icd10")

af_data_clean2 <- df365_cleaned[complete.cases(df365_cleaned[, vars_to_use]), ]

set.seed(123)
train_index <- createDataPartition(af_data_clean$afib_by_cutoff, p = 0.8, list = FALSE)
train_data2 <- af_data_clean2[train_index, ]
test_data2  <- af_data_clean2[-train_index, ]

nn_formula2 <- as.formula(
  paste("afib_by_cutoff ~", paste(vars_to_use2, collapse = " + "))
)

nn_model2 <- nnet(nn_formula2,
                 data = train_data,
                 size = 5,
                 decay = 0.01,
                 maxit = 500,
                 linout = FALSE)

predicted_probs_test <- predict(nn_model2, test_data2, type = "raw")

predicted_classes_test <- ifelse(predicted_probs_test > 0.1, 1, 0)

confusionMatrix(factor(predicted_classes_test), 
                factor(test_data2$afib_by_cutoff), 
                positive = "1")

roc_obj <- roc(test_data2$afib_by_cutoff, predicted_probs_test)
plot(roc_obj, col = "blue")
auc(roc_obj)
