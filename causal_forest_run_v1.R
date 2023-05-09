##-------------------------------## 
##------UGS Causal Forests-------##
##-------------------------------## 
## Author: Nathan Haddon
## Last Modified: 02/21/2023
## Purpose: test run for causal forests at UGS endline
## Note: adapted from causal forest project 
## used in Columbia Flex Credit 
## test line for github training 
##-------------------------------## 
##-------------------------------## 

# todo

# UNDERSTAND calibration / omnibus test, what's the logic for heterogeneity exists (coef=1)
# check Chernozhukov, Victor, Mert Demirer, Esther Duflo, and Ivan Fernandez-Val. "Generic Machine Learning Inference on Heterogenous Treatment Effects in Randomized Experiments." arXiv preprint arXiv:1712.04802 (2017).

# add permutation importance?
# add group importance?

# save causal forests 
# do with dummies vs. continuous? 
# how to aggregate SEs? dont think it's possible

 rm(list = ls())
 #install.packages("dplyr")
#library(dplyr)
 #install.packages("haven")
 #library(haven)
 #install.packages("tidyr")
 #library(tidyr)
 #install.packages("grf")
 #library(grf)
 #install.packages("log4r")
 #library(log4r)
 #install.packages("tidyr")
 #library(tidyr)
 #install.packages("logr")
 #library(logr)
 #install.packages("foreign")
 #library("foreign")
 #install.packages("openxlsx")
 #library(openxlsx)
 #install.packages("openxlsx")
 library(openxlsx)

logfile <- file.path("C:", "Users", "nsh5897", "Box", "Kamwenge Graduation Study", "07_Questionnaires&Data", "Endline", "02_dofiles", "04_causal_forests", "UGS_Causal_Forest", "04_log", "0415_25k_tree_test_pooled_noasset_only.txt")
cf_log <- log_open(logfile, autolog = TRUE)
sep("UGS C1 Causal Forests Omnibus Tests: Ref Full Grad")

# directories
setwd("X:/Box/Kamwenge Graduation Study/07_Questionnaires&Data/Endline/14_Coh1_Hetfx/01_Causal_Forests/UGS_Causal_Forest/02_data")

# load data
# ugs_endline = read_dta("cf_for_omnibus_no_asset_v3.dta")
ugs_endline = read_dta("cf_for_omnibus_full_grad_v3.dta")
log_print("importing UGS C1 EL Data", hide_notes = TRUE)
log_print(ugs_endline, hide_notes = TRUE)

#make labelled become factor
## LB: not sure yet if I need this
# dta[,sapply(dta, class) == "labelled"] = 
#  lapply(dta[,sapply(dta, class) == "labelled"], as.factor)

# drop variables that are missing for all observations and obs that are missing the key values 
## ugs_endline = Filter(function(x)!all(is.na(x)), ugs_endline)

# filtering out the hosts or refugees
ugs_endline = ugs_endline[which(ugs_endline$refugee == 0),]

# confirm hhid is unique id
if (any(duplicated(ugs_endline$hhid))) stop()

# sorting by hhid 
ugs_endline = ugs_endline[order(ugs_endline$hhid),]
# sapply(ugs_endline, class) # table(sapply(dta, class))

# Prepare the variables we need for fitting Causal Forests.
# outcome vars of interest
ys = c(#'cons_monthly_adeq_usd_imp',
       #'prod_assets_tot_val_imp',
       'total_income_monthly_imp',
       'inc_employment_1m_usdppp',
       'business_profit_1m_tot', 
       'net_inc_lstock_total_imp',
       'm_d_crop_prodval_ags2_ppp_imp')
       #'wellbeing_index',
       #'foodsec_index')
       #'inc_employment_1m_usdppp',
       #'business_profit_1m_tot', 
       #'net_inc_lstock_total_imp',
       #'m_d_crop_prodval_ags2_ppp_imp',
       #'cons_mfood_adeq_usd',
       #'cons_30dnonfood_adeq_usd')
       
log_print("Outcomes of Interest", hide_notes = TRUE)
log_print(ys, hide_notes = TRUE)
# baseline controls, strat/rerand vars 
xs = c('bsl_hhmems_count', 'bsl_hhh_highest_edu', 'bsl_avg_yrs_educ_adults', 
       'bsl_outhh_empl', 'bsl_fambus','bsl_sources_income', 'bsl_lstock_sold_any', 'bsl_land_total_sqm',
       'bsl_land_owned_total_sqm', 'bsl_seed_exp_tot', 'bsl_aginput_index', 'bsl_crop_earn_total_ppp_w', 'bsl_z_crops_2018_ind',
       'bsl_D_crops_value_3', 'bsl_D_crops_value_23', 'bsl_D_crops_value_20', 'bsl_D_crops_value_9',
       'bsl_D_crops_value_10', 'bsl_dietary_quality_score_index', 'bsl_z_lstockasset_index',
       'bsl_z_hhasset_index', 'bsl_z_prodasset_index', 'bsl_wellbeing_index', 
       'refugee', 'score_vil',
       'score_ind',  'hhsize_vil', 'head_female_ind', 'head_female_vil', 'eligible_hhpervil', 
       'vilRand_strata_0', 'vilRand_strata_1', 'vilRand_strata_2',
       'vilRand_strata_3', 'vilRand_strata_4', 'vilRand_strata_5',
       'vilRand_strata_6','vilcombo_parish_code_0', 'vilcombo_parish_code_1', 'vilcombo_parish_code_2', 
       'vilcombo_parish_code_3', 'vilcombo_parish_code_4', 'vilcombo_parish_code_5', 
       'vilcombo_parish_code_6', 'vilcombo_parish_code_7', 'vilcombo_parish_code_8', 'vilcombo_parish_code_9', 
       'vilcombo_parish_code_10', 'vilcombo_parish_code_11', 'vilcombo_parish_code_12',
       'vilcombo_parish_code_13', 'vilcombo_parish_code_14', 'vilcombo_parish_code_15', 
       'vilcombo_parish_code_16', 'vilcombo_parish_code_17', 'vilcombo_parish_code_18', 
       'bsl_mh_neg_kessler6',
       'bsl_yrs_settled', 'bsl_sn_eff_index', 'bsl_sn_grit_index', 'bsl_sn_hhrel_index', 
       'bsl_sn_hhrel_num_peers_known', 'bsl_socap_index', 'bsl_phys_health_index', 
       'bsl_life_sat_index', 'bsl_soccoh_ind_helps', 'bsl_soccoh_worktog', 
       'bsl_soccoh_trustpplgen', 'bsl_soccoh_diff_land1', 'bsl_soccoh_diff_politic1', 
       'bsl_soccoh_diff_ethnic1', 'bsl_soccoh_diff_religion1', 'bsl_add_dead', 
       'bsl_z_save_loc_index', 'bsl_initial_loanask_n', 'bsl_ind_age', 
       'bsl_head_part', 'bsl_ind_gender')

xs_host = c('bsl_hhmems_count', 'bsl_hhh_highest_edu', 'bsl_avg_yrs_educ_adults', 
       'bsl_outhh_empl', 'bsl_fambus','bsl_sources_income', 'bsl_lstock_sold_any', 'bsl_land_total_sqm',
       'bsl_land_owned_total_sqm', 'bsl_seed_exp_tot', 'bsl_aginput_index', 'bsl_crop_earn_total_ppp_w', 
       'bsl_z_crops_2018_ind', 'bsl_D_crops_value_3', 'bsl_D_crops_value_23', 'bsl_D_crops_value_20', 
       'bsl_D_crops_value_9', 'bsl_D_crops_value_10', 'bsl_dietary_quality_score_index', 
       'bsl_z_lstockasset_index', 'bsl_z_hhasset_index', 'bsl_z_prodasset_index', 'bsl_wellbeing_index',
       'score_ind', 'head_female_ind',
       'bsl_mh_neg_kessler6',
       'hhsize_vil', 'score_vil', 'head_female_vil', 'eligible_hhpervil',
       'vilcombo_parish_code_1', 'vilcombo_parish_code_2', 
       'vilcombo_parish_code_3', 'vilcombo_parish_code_4', 'vilcombo_parish_code_5', 
       'vilcombo_parish_code_6', 'vilcombo_parish_code_7', 'vilcombo_parish_code_8', 'vilcombo_parish_code_9', 
       'vilcombo_parish_code_10', 'vilcombo_parish_code_11', 'vilcombo_parish_code_12',
       'vilcombo_parish_code_13', 'vilcombo_parish_code_14', 'vilcombo_parish_code_15', 
       'vilcombo_parish_code_16', 'vilcombo_parish_code_17', 'vilcombo_parish_code_18', 
       'bsl_yrs_settled', 'bsl_sn_eff_index', 'bsl_sn_grit_index', 'bsl_sn_hhrel_index', 
       'bsl_sn_hhrel_num_peers_known', 'bsl_socap_index', 'bsl_phys_health_index', 
       'bsl_life_sat_index', 'bsl_soccoh_ind_helps', 'bsl_soccoh_worktog', 
       'bsl_soccoh_trustpplgen', 'bsl_soccoh_diff_land1', 'bsl_soccoh_diff_politic1', 
       'bsl_soccoh_diff_ethnic1', 'bsl_soccoh_diff_religion1', 'bsl_add_dead', 
       'bsl_z_save_loc_index', 'bsl_initial_loanask_n', 'bsl_ind_age', 
       'bsl_head_part', 'bsl_ind_gender')

xs_ref = c('bsl_hhmems_count', 'bsl_hhh_highest_edu', 'bsl_avg_yrs_educ_adults', 
       'bsl_outhh_empl', 'bsl_fambus','bsl_sources_income', 'bsl_lstock_sold_any', 'bsl_land_total_sqm',
       'bsl_land_owned_total_sqm', 'bsl_seed_exp_tot', 'bsl_aginput_index', 'bsl_crop_earn_total_ppp_w', 'bsl_z_crops_2018_ind',
       'bsl_D_crops_value_3', 'bsl_D_crops_value_23', 'bsl_D_crops_value_20',
       'bsl_D_crops_value_10', 'bsl_dietary_quality_score_index', 'bsl_z_lstockasset_index',
       'bsl_z_hhasset_index', 'bsl_z_prodasset_index', 'bsl_wellbeing_index', 
       'score_vil',
       'score_ind',  'hhsize_vil', 'head_female_ind', 'head_female_vil', 'eligible_hhpervil',
       'vilRand_strata_1', 'vilRand_strata_2',
       'vilRand_strata_3', 'vilRand_strata_4', 'vilRand_strata_5',
       'vilRand_strata_6',
       'bsl_mh_neg_kessler6',
       'bsl_yrs_settled', 'bsl_sn_eff_index', 'bsl_sn_grit_index', 'bsl_sn_hhrel_index', 
       'bsl_sn_hhrel_num_peers_known', 'bsl_socap_index', 'bsl_phys_health_index', 
       'bsl_life_sat_index', 'bsl_soccoh_ind_helps', 'bsl_soccoh_worktog', 
       'bsl_soccoh_trustpplgen', 'bsl_soccoh_diff_land1', 'bsl_soccoh_diff_politic1', 
       'bsl_soccoh_diff_ethnic1', 'bsl_soccoh_diff_religion1', 'bsl_add_dead', 
       'bsl_z_save_loc_index', 'bsl_initial_loanask_n', 'bsl_ind_age', 
       'bsl_head_part', 'bsl_ind_gender')

#num_controls <- length(xs)
#for (var in 1:num_controls) {
#  max_x = max(xs[ugs_endline$var])
#  print(max_x)
#  min_x = min(xs[ugs_endline$var])
#  print(min_x)
#  if (min_x == max_x) {
#    xs = xs[!xs == var]
#  }
#}

#vals = c("consumption", "assetval", "income")
#omnibus_results <- data.frame(outcome = vals)
#num_chars = length(vals)
#for (char in vals) {
#  test_results = runif(4, min = 0, max = 1)
#  omnibus_results[char] = test_results[1]
#}
# print(omnibus_results)

log_print("Controls", hide_notes = TRUE)
log_print(xs_ref, hide_notes = TRUE)

## setting up empty data frame to be filled with results of omnibus test  
omnibus_output_all <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(omnibus_output_all) <- c('outcome','mfp.est', 'mfp.se', 'mfp.pval', 'dfp.est', 'dfp.se', 'dfp.pval')

# setting up data frame of ATES for each outcome 
ATE_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(ATE_all) <- c('outcome', 'ATE', 'SE')

# Fit Causal Forests
set.seed(1)

# data frame of variable importances
var_imp_all = data.frame(var_name = xs_ref)
print(var_imp_all, hide_notes = TRUE)
te_all = data.frame()
log_print(te_all, hide_notes = TRUE)

num_ys = length(ys)
log_print("number of Y variables", hide_notes = TRUE)
log_print(num_ys, hide_notes = TRUE)

for (val in 1:num_ys) {
  
  log_print("current Y variable", hide_notes = TRUE)
  log_print(ys[val], hide_notes = TRUE)
  
  # remove observation that have missing values in the responses
  aux = ugs_endline %>% select_at(ys[val])
  ugs_endline = ugs_endline[!is.na(aux),]
  rm(aux)
  
  Y = as.numeric(unlist(ugs_endline[,match(ys[val],names(ugs_endline))]))
  W = as.integer(ugs_endline$treatment_2)
  X = ugs_endline[,xs_ref]
  
  # convert factor variables to numeric
  #X[,sapply(X, class) == "factor"] = 
  #  lapply(X[,sapply(X, class) == "factor"], as.numeric)
  # table(sapply(X, class))
  # X

  # estimate causal forest
  cf1 = causal_forest(X, Y, W, num.trees = 1000,
                      cluster = ugs_endline$vil_clus_id,
                      equalize.cluster.weights = FALSE)

  # omnibus test -> run test and add test results to data frame of results 
  # steps: 
  # 1) run the test and print 
  # 2) create data frame of relevant stats from the test (estimate, SE, p=-val) 
  # 3) name the columns to merge 
  # 4) merge 
  omnibus_test <- test_calibration(cf1)
  log_print(omnibus_test, hide_notes = TRUE)
  omnibus.result <- data.frame(ys[val], omnibus_test[1,1], omnibus_test[1,2], omnibus_test[1,4], omnibus_test[2,1], omnibus_test[2,2], omnibus_test[2,4])
  names(omnibus.result) <- c("outcome", "mfp.est", "mfp.se", "mfp.pval", "dfp.est", "dfp.se", "dfp.pval")
  omnibus_output_all <- rbind(omnibus_output_all, omnibus.result)
  rm(omnibus.result)

  var_imp = data.frame(perc_importance = variable_importance(cf1),
                       var_name = names(X))

  print(var_imp[order(var_imp$perc_importance, decreasing = TRUE),], hide_notes = TRUE)
  
  names(var_imp)[1] = paste0('pimp_',ys[val])
  var_imp_all <- merge(var_imp_all,var_imp)
  
  # print(average_treatment_effect(cf1, target.sample = "all"), hide_notes = TRUE)
  ATE <- average_treatment_effect(cf1, target.sample = "all")
  print(ATE)
  ATE_outcome <- data.frame(ys[val], ATE[1], ATE[2])    # data frame of the ATE and SE
  colnames(ATE_outcome) <- c('outcome', 'ATE', 'SE')    # name the columns to merge 
  ATE_all <- rbind(ATE_all, ATE_outcome)                # merge 
  rm(ATE_outcome)

  # auxmed <- median(unlist(X[,2]))
  #print(average_treatment_effect(cf1, target.sample = "all", subset = unlist(X[,2] > auxmed))) # women
  
  tau.hat = predict(cf1, estimate.variance = TRUE)
  # log_print(tau.hat, hide_notes = TRUE)
  sigma.hat = sqrt(tau.hat$variance.estimates)
  #log_print(sigma.hat, hide_notes = TRUE)

  te_var = data.frame(tau.hat$predictions, ys[val], ugs_endline$hhid)
  te_all <- rbind(te_all, te_var)
  
  # Estimate the conditional average treatment effect on the full sample (CATE).
  # average_treatment_effect(cf1, target.sample = "all")
  
  # Estimate the conditional average treatment effect on the treated sample (CATT).
  # average_treatment_effect(cf1, target.sample = "treated")
  
  # rm(cf1, var_imp, pred, te, X, Y, W)
}

adsfasfadsfdsafasdfa

te_all <- te_all %>% spread(key=ys.val., value=tau.hat.predictions)
names(te_all[]) <- paste0("te_",names(te_all))
names(te_all) <- c('hhid',paste0("te_",names(te_all)[2:ncol(te_all)]))

# load data

dta = ugs_endline
dta = dta[,c('hhid',xs, ys, 'treatment_2')]

# merge causal forest treatment effects
names(te_all)
names(dta)

cf_tes_X = left_join(dta, te_all, by = "hhid")
# rm(te_all, dta)

# Output the data to dta
# require(foreign)

write.dta(cf_tes_X, "xls_creation_testing_tes.dta")
write.dta(var_imp_all, "xls_creation_testing_output.dta")

log_close()
