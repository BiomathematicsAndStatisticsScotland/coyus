context("Testcase to detect values out-of-order ('bug' reported by Sally Watson 2021-12-14)")

## Originally tested with the following:
##
## value_order_data=readCoyu9File("/scratch/coyu/bug_reports/sally_2021-12-15/COYUS9.DAT")
## trial_data=value_order_data$trial_data
## coyu_parameters=value_order_data$coyu_parameters
## probability_set=value_order_data$probability_set
## character_number=71
## year="2018"

data(test_3_year_withmissing)

trial_data=test_3_year_withmissing$trial_data
coyu_parameters=test_3_year_withmissing$coyu_parameters
probability_set=test_3_year_withmissing$probability_set
character_number=71
year="2013"

##Extract our data for the character we want and get the logSD manually
character_stddev=name_stddev(character_number)
character_mean=name_mean(character_number)
  
data_candidates=get_varieties(trial_data,coyu_parameters$candidates,character_number)
data_references=get_varieties(trial_data,coyu_parameters$references,character_number)
  
data_candidates$logSD=log(data_candidates[,character_stddev]+1)
data_references$logSD=log(data_references[,character_stddev]+1)

original_cand_data=data_candidates[data_candidates$year==year,c("AFP","logSD")]
original_cand_data=original_cand_data[with(original_cand_data,order(AFP)),]

char_log_sd_data_orig=setNames(original_cand_data$logSD,
                               original_cand_data$AFP)

char_results = run_COYU(character_number, trial_data, coyu_parameters, probability_set)

char_results_3yr=char_results$`3_year_reject`

char_log_sd_data_fromresults=char_results_3yr$mean_sd_data[[3]]$cand_logsd


test_that("logSD data is the same from results object as it is from original data",{
  expect_true(all(char_log_sd_data_orig==char_log_sd_data_fromresults))
})

