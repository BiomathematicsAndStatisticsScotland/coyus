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
    expect_true(!is.na(char_3_results$mean_sd_data[[1]]$ref_logsd["3"]))
    expect_true(is.na(char_3_results$mean_sd_data[[2]]$ref_logsd["3"]))
    expect_equal(char_3_results$reference[3,"reference_actual_logSD"], 2.4, tolerance=0.01)
    expect_true(!is.na(char_3_results$mean_sd_data[[1]]$ref_mean["3"]))
    expect_true(!is.na(char_3_results$mean_sd_data[[2]]$ref_mean["3"]))
})

test_that("AFP 50 has means but no SDs", {
    expect_true(is.na(char_3_results$mean_sd_data[[1]]$ref_logsd["50"]))
    expect_true(is.na(char_3_results$mean_sd_data[[2]]$ref_logsd["50"]))
    expect_true(is.na(char_3_results$reference[50,"reference_actual_logSD"]))
    expect_true(!is.na(char_3_results$mean_sd_data[[1]]$ref_mean["50"]))
    expect_true(!is.na(char_3_results$mean_sd_data[[2]]$ref_mean["50"]))
    expect_equal(char_3_results$reference[50,"reference_means"], 77, tolerance=0.01)
})
