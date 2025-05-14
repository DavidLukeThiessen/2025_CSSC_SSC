# This will be a file that takes the raw data, creates the response indicators and
# lab bloodwork flags, and removes uneeded variables. After running this script,
# you should be left with two clean datasets with only the binary response 
# variable and the allowed explanatory variables.

# TODO:
# - Include a train-test split in this code?
# - Check for missing data and also remove it as part of this script?

library(readxl)
library(dplyr)

# MANUAL STEP: Read in the raw data from your local directory and name it af_data.
# af_data <- read_xlsx(".....synthetic_data_stats_competition_2025_final.xlsx")

make_afib_flag_df <- function(data, cutoff_days) {
  data %>%
    mutate(
      afib_by_cutoff = as.integer(
        !is.na(time_to_outcome_afib_aflutter_new_post) &
        time_to_outcome_afib_aflutter_new_post <= cutoff_days))}

make_bloodwork_flags <- function(data, ..., prefix = "observed_") {
  data %>%
    mutate(across(c(...),
                  ~ as.integer(!is.na(.x)),
                  .names = paste0(prefix, "{.col}")))}

names(af_data) <- gsub("obstructive _sleep_apnea_icd10", "obstructive_sleep_apnea_icd10", names(af_data))

df_flags <- make_bloodwork_flags(af_data, troponin_t_hs_peri_highest, crp_high_sensitive_peri, tsh_peri, hga1c_peri_highest, 
                                 glucose_fasting_peri_highest, hgb_peri, hct_peri, rdw_peri, wbc_peri, plt_peri, inr_peri, 
                                 ptt_peri, esr_peri, albumin_peri, alkaline_phophatase_peri, alanine_transaminase_peri, 
                                 aspartate_transaminase_peri, bilirubin_total_peri, bilirubin_direct_peri, urea_peri, 
                                 creatinine_peri, urine_alb_cr_ratio_peri, sodium_peri,potassium_peri, chloride_peri, ck_peri,
                                 NTproBNP_peri_highest ,glucose_random_peri_highest, tchol_peri_highest, ldl_peri_highest, 
                                 hdl_peri_lowest, tg_peri_highest, iron_peri, tibc_peri, ferritin_peri)

df180 <- make_afib_flag_df(df_flags, cutoff_days = 180)
df365 <- make_afib_flag_df(df_flags, cutoff_days = 365)

# Note to David, next time I teach Stat 378, show them how to programatically manage variable names instead of
# copying or typing them all out.
vars_to_remove <- c(
  "troponin_t_hs_peri_highest", "potassium_peri", "crp_high_sensitive_peri", "tsh_peri", 
  "hga1c_peri_highest", "glucose_fasting_peri_highest", "hgb_peri", "hct_peri", 
  "rdw_peri", "wbc_peri", "plt_peri", "inr_peri", "ptt_peri", "esr_peri", 
  "albumin_peri", "alkaline_phophatase_peri", "alanine_transaminase_peri", 
  "aspartate_transaminase_peri", "bilirubin_total_peri", "bilirubin_direct_peri", 
  "urea_peri", "creatinine_peri", "urine_alb_cr_ratio_peri", "sodium_peri", 
  "chloride_peri", "ck_peri", "NTproBNP_peri_highest", "glucose_random_peri_highest", 
  "tchol_peri_highest", "ldl_peri_highest", "hdl_peri_lowest", "tg_peri_highest", 
  "iron_peri", "tibc_peri", "ferritin_peri", "anti_platelet_oral_non_asa_any_peri", "anti_coagulant_oral_any_peri", "nitrates_any_peri", 
  "ranolazine_peri", "acei_peri", "arb_peri", "arni_entresto_peri", "beta_blocker_any_peri", "ivabradine_peri", "ccb_dihydro_peri", 
  "ccb_non_dihydro_peri", "diuretic_loop_peri", "diuretic_thiazide_peri", "diuretic_low_ceiling_non_thiazide_peri", "diuretic_metolazone_peri", 
  "diuretic_indapamide_peri", "diuretic_mra_peri", "diuretic_vasopressin_antagonist_peri", "anti_arrhythmic_any_peri", 
  "anti_arrhythmic_amiodarone_peri", "anti_arrhythmic_disopyramide_peri", "digoxin_peri", "amyloid_therapeutics_tafamidis_peri", 
  "amyloid_therapeutics_diflunisal_peri", "amyloid_therapeutics_patisiran_peri", "amyloid_therapeutics_inotersen_peri", "lipid_statin_peri", 
  "lipid_fibrate_peri", "lipid_ezetimibe_peri", "lipid_PCKSK9_peri", "lipid_other_peri", "glucose_insulin_peri",
  "glucose_glp_1_agonsist_peri", "glucose_ohg_biguanide_peri", "glucose_ohg_alphagluc_peri", "glucose_ohg_dpp_4_peri", "glucose_ohg_sglt_2_peri", 
  "glucose_ohg_thiazolidine_peri", "glucose_ohg_repaglinide_peri", "glucose_ohg_sulfonylurea_peri", "glucose_ohg_other_peri", 
  "smoking_cessation_oral_peri", "smoking_cessation_nicotine_replacement_peri", "patient_id", "outcome_afib_aflutter_new_post", "outcome_all_cause_death",
  "time_to_outcome_afib_aflutter_new_post", "follow_up_duration", "time_to_outcome_all_cause_death")

df180_cleaned <- df180 %>% select(-all_of(vars_to_remove))
df365_cleaned <- df365 %>% select(-all_of(vars_to_remove))

