context("Testcase for data with single years missing SD and mean data")

## This is to confirm the fix of a bug where all year means and SDs
## were omitted for a character if one mean or SD was missing.

data(test_missing_values_extra)

print("Computing single-year missing data results")

results_all<-COYU_all_results(test_missing_values_extra$trial_data,
                              test_missing_values_extra$coyu_parameters,
                              test_missing_values_extra$probability_sets)[[1]]

char_3_results=COYU_results_by_character(results_all, 3)[[1]]

test_that("AFP 3 has SD 1st year, not 2nd year, and has over-year SD", {
    yr1_results = char_3_results$yearly_results[[1]]$ref_results
    yr2_results = char_3_results$yearly_results[[2]]$ref_results
    expect_true(!is.na(yr1_results[yr1_results$AFP==3,"logSD"]))
    expect_true(is.na(yr2_results[yr2_results$AFP==3,"logSD"]))
    expect_equal(char_3_results$reference[3,"reference_actual_logSD"], 2.4, tolerance=0.01)
    expect_true(!is.na(yr1_results[yr1_results$AFP=="3","mn"]))
    expect_true(!is.na(yr2_results[yr2_results$AFP=="3","mn"]))
})

test_that("AFP 50 has means but no SDs", {
    yr1_results = char_3_results$yearly_results[[1]]$ref_results
    yr2_results = char_3_results$yearly_results[[2]]$ref_results
    expect_true(is.na(yr1_results[yr1_results$AFP=="50","logSD"]))
    expect_true(is.na(yr2_results[yr2_results$AFP=="50","logSD"]))
    expect_true(is.na(char_3_results$reference[50,"reference_actual_logSD"]))
    expect_true(!is.na(yr1_results[yr1_results$AFP=="50","mn"]))
    expect_true(!is.na(yr2_results[yr2_results$AFP=="50","mn"]))
    expect_equal(char_3_results$reference[50,"reference_means"], 77, tolerance=0.01)
})
