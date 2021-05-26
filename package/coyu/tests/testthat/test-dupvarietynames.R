context("Testcase for data with duplicate variety names with different AFP numbers")

## This testcase checks that data with same variety name associated
## with different AFP numbers works correctly
##
## This is necessary when testing different batches of seed for the
## same variety - the tests are done under different AFP numbers but
## the variety name is the same
##
## At the moment we just run to make sure the results printing code does not crash.
data(test_2_year)

test_trial_data=test_2_year$trial_data
test_trial_data[test_trial_data$variety=="AFP_1080",]$variety="AFP_1088"

print("Computing results - dup reference variety")
results1<-COYU_all_results(test_trial_data,
                           test_2_year$coyu_parameters,
                           test_2_year$probability_sets)[[1]]

print("Testing results printing - dup reference variety")

  
COYU_print_results(results1,
                   test_2_year$coyu_parameters,
                   test_2_year$character_key,
                   test_2_year$probability_sets[1,],
                   connection=stdout(),
                   verbose=TRUE) 


print("Computing results - dup candidate variety")

test_trial_data=test_2_year$trial_data
test_trial_data[test_trial_data$variety=="AFP_994",]$variety="AFP_109"
results2<-COYU_all_results(test_trial_data,
                           test_2_year$coyu_parameters,
                           test_2_year$probability_sets)[[1]]

print("Testing results printing - dup candidate variety")
COYU_print_results(results2,
                   test_2_year$coyu_parameters,
                   test_2_year$character_key,
                   test_2_year$probability_sets[1,],
                   connection=stdout(),
                   verbose=TRUE) 
