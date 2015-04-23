context("Testcase for missing means")

data(test_missing_means)

print("Computing missing mean results - all")

## missing_data<-test_missing_means$trial_data[rowSums(is.na(test_missing_means$trial_data)) > 0,
##                                              c("year","AFP","variety","UP78","sUP78")]

## test_missing_means$trial_data[test_missing_means$trial_data$AFP=="2046",
##                               c("year","AFP","variety","UP78","sUP78")]


## test_missing_means$trial_data[test_missing_means$trial_data$AFP==20423,c("year","AFP","variety","UP78","sUP78")]
## char_78_reject$reference[char_78_reject$reference$reference_afp==20423,]


results_all<-COYU_all_results(test_missing_means$trial_data,
                              test_missing_means$coyu_parameters,
                              test_missing_means$probability_sets)[[1]]

char78_only_params<-test_missing_means$coyu_parameters
char78_only_params$characters<-c(78)

results_char78_only<-COYU_all_results(test_missing_means$trial_data,
                                      char78_only_params,
                                      test_missing_means$probability_sets)[[1]]
##Compare with running single variety/character

test_that("Results with missing data in 1st year of input have missing data in output",{
  char_result<-results_all[["2_year_reject",1]]$reference
  char_result_single<-results_char78_only[["2_year_reject",1]]$reference
  
  expect_equal(sum(is.na(char_result)),2) #2 missing values
  expect_true(is.na( char_result[char_result$reference_afp==2046,]$reference_actual_logSD))
  expect_true(is.na( char_result[char_result$reference_afp==2046,]$reference_adjusted_logSD))

  expect_equal(sum(is.na(char_result_single)),2) #2 missing values
  expect_true(is.na( char_result_single[char_result_single$reference_afp==2046,]$reference_actual_logSD))
  expect_true(is.na( char_result_single[char_result_single$reference_afp==2046,]$reference_adjusted_logSD))
})
