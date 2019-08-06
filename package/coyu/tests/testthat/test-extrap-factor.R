context("Testcase for correct behaviour of extrapolation factors")

##- where there are two years with extrapolation=1, the aggregated
##extrapolation factor should be the *maximum *of the two

##- where there is only one year with extrapolation=1, the aggregated
##extrapolation factor should bethe factor for the non-missing year

##- where there are no years with extrapolation=1, the aggregated
##extrapolation factor should be NA (and present)

data(test_2_year) 
# from example data set in package
print("Computing results for test dataset")
results1<-COYU_all_results(test_2_year$trial_data,
                           test_2_year$coyu_parameters,
                           test_2_year$probability_sets)[[1]]

df_results = COYU_results_as_dataframe(results1)

test_that("Two years with extrapolation - value should be MAX of both years", {
    result8 =  results1[,6][["2_year_reject"]]
    expect_equal( 8, result8$character_number) #Check character number just in case

    ##cand109 should have extrapolation, not NA
    char_8_cand_109 = result8$candidates[1,]

    expect_equal("109", as.character(char_8_cand_109$candidate_afp))
    expect_true(!is.na(char_8_cand_109$extrapolation_factor)
                && char_8_cand_109$extrapolation==1)
})

test_that("One year with extrapolation value should be factor for non-missing year", {
    result15 =  results1[,10][["2_year_reject"]]
    expect_equal( 15, result15$character_number) #Check character number just in case

    ##cand109 should have extrapolation, not NA
    char_15_cand_109 = result15$candidates[1,]
    expect_equal("109", as.character(char_15_cand_109$candidate_afp))
    expect_true(!is.na(char_15_cand_109$extrapolation_factor)
                && char_15_cand_109$extrapolation==1)
})

test_that("No years with extrapolation - extrapolation factor should be NA", {
    result15 =  results1[,10][["2_year_reject"]]
    expect_equal( 15, result15$character_number) #Check character number just in case

  
    ##cand994 should not have extrapolation, value NA
    char_15_cand_994 = result15$candidates[2,]
    expect_equal("994", as.character(char_15_cand_994$candidate_afp))
    expect_true(is.na(char_15_cand_994$extrapolation_factor)
                 && char_15_cand_994$extrapolation==0)

})

# note [[1]] selects the results for the first probability set
#COYU_print_results(results1, test_2_year$coyu_parameters, test_2_year$character_key, test_2_year$probability_set[1,])
# note test_2_year$probability_set[1,] gives the probabilities for this set
#write.csv(COYU_results_as_dataframe(results1, "2_year_reject"), "tester.csv")
#COYU_plot_results(results1, character_key = test_2_year$character_key, plot_file="MyPlots.pdf")

#temp<-test_2_year$trial_data
