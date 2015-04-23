context("Testcase for cyclic planting and data ordering")

data(test_cyclic_planting)

print("Computing cyclic planting results - all")

results_all<-COYU_all_results(test_cyclic_planting$trial_data,
                              test_cyclic_planting$coyu_parameters,
                              test_cyclic_planting$probability_sets)[[1]]

#Run single candidate trials for testing - these will not be misaligned. Then compare
print("Computing cyclic planting results - 2865 only")
afp2865_params <- test_cyclic_planting$coyu_parameters
afp2865_params$candidates=c(2865)

afp2865_results<-COYU_all_results(test_cyclic_planting$trial_data,
                                 afp2865_params,
                                 test_cyclic_planting$probability_sets)[[1]]

print("Computing cyclic planting results - 9623 only")
afp9623_params <- test_cyclic_planting$coyu_parameters
afp9623_params$candidates=c(9623)

afp9623_results<-COYU_all_results(test_cyclic_planting$trial_data,
                                 afp9623_params,
                                 test_cyclic_planting$probability_sets)[[1]]

print("Computing cyclic planting results - 2771 only")
afp2771_params <- test_cyclic_planting$coyu_parameters
afp2771_params$candidates=c(2771)

afp2771_results<-COYU_all_results(test_cyclic_planting$trial_data,
                                 afp2771_params,
                                 test_cyclic_planting$probability_sets)[[1]]


print("Computing cyclic planting results - 2840 only")
afp2840_params <- test_cyclic_planting$coyu_parameters
afp2840_params$candidates=c(2840)

afp2840_results<-COYU_all_results(test_cyclic_planting$trial_data,
                                 afp2840_params,
                                 test_cyclic_planting$probability_sets)[[1]]

afp2840_54only_params<-afp2840_params
afp2840_54only_params$characters=c(54)
afp2840_54only_results<- COYU_all_results(test_cyclic_planting$trial_data,
                                          afp2840_54only_params,
                                          test_cyclic_planting$probability_sets)[[1]]

single_results=list(
  "2840"=afp2840_results,
  "2865"=afp2865_results,
  "9623"=afp9623_results,
  "2771"=afp2771_results
  )



test_that("Results look basically sane",{
  expect_true(is_3_year(results_all))
})

#The logic behind these test cases could be improved - essentially we just look at a sample of data

test_that("Extrapolation detection is working", {

  
  #Character 4
  #   year mn.MIN mn.MAX
  # 1 2012 44.339 55.081
  # 2 2013 31.261 42.990
  # 3 2014 39.545 48.325

  results_4<-results_all[["3_year_reject",1]]$candidates

  expect_true(any(results_4$extrapolation==1))
  expect_equal(3,sum(results_4$extrapolation==1))

  #2865 (2012-55.6 2014-56.603)
  afp2865_4=results_4[results_4$candidate_afp==2865,]  
  expect_true(afp2865_4$extrapolation==1)
  expect_equal(51.578,afp2865_4$candidate_means,tolerance=0.01)
 
  #2840(2013 - 44.071), yes.
  expect_true(results_4[results_4$candidate_afp==2840,"extrapolation"]==1)
  
  #9639 no (any year)
  expect_true(results_4[results_4$candidate_afp==9639,"extrapolation"]==0)
  
  #Character 70
  # Means by year
  #   year    MIN    MAX
  # 1 2012 51.712 66.695
  # 2 2013 41.833 56.225
  # 3 2014 51.031 66.184
  results_70=results_all[["3_year_reject",2]]$candidates

  expect_true(any(results_70$extrapolation==1))
  expect_equal(7,sum(results_70$extrapolation==1))

  #2865 means(40.077, 35.508, 39.569), extrapolated
  afp2865_70=results_70[results_70$candidate_afp==2865,]  
  expect_true(afp2865_70$extrapolation==1)
  expect_equal(38.38467,afp2865_70$candidate_means,tolerance=0.01)
  
  #2771 means(2013=58.658) yes
  expect_true(results_70[results_70$candidate_afp==2771,"extrapolation"]==1)
  #2854 means(2031=56.534) yes
  expect_true(results_70[results_70$candidate_afp==2854,"extrapolation"]==1)

  #9639 no (any year)
  expect_true(results_70[results_70$candidate_afp==9639,"extrapolation"]==0)
  #2846 no (any year)
  expect_true(results_70[results_70$candidate_afp==2846,"extrapolation"]==0)
  
  #Character 102
  #Means by year
  #   year mn.MIN mn.MAX
  # 1 2012  3.835  9.331
  # 2 2013  5.171  9.636
  # 3 2014  3.651  9.359
  # NO EXTRAPOLATED VARIETIES
  expect_true(!any(results_all[["3_year_reject",26]]$candidates$extrapolation==1))
})


test_that("Varieties are not being mis-ordered", {

  
  #Any misordering should show up in a mismatch between the candidate
  #results in the single candidate trial and the all results dataset
 
  nchars <- 30
  for (char_col in 1:nchars) {
    char_number <- results_all[["3_year_reject",char_col]]$character_number
    
    char_results <- results_all[["3_year_reject",char_col]]$candidates

    
     for (afp_num in names(single_results)) {

      #print(sprintf("Checking character %s for afp %s against single variety trial",char_number,afp_num))
      
      single_afp_result <- single_results[[afp_num]]
      
      expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_means,
                   char_results[char_results$candidate_afp==afp_num,"candidate_means"])
    
      expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$extrapolation,
                   char_results[char_results$candidate_afp==afp_num,"extrapolation"])
      
      expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_adjusted_logSD,
                   char_results[char_results$candidate_afp==afp_num,"candidate_adjusted_logSD"])
    
      expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_COYU_pvalue,
                   char_results[char_results$candidate_afp==afp_num,"candidate_COYU_pvalue"])
    }
  }

  

  
})

test_that("Single character, single variety trials give same result as all results",{

  #N.B reference varieties must be the same for this to work.
  
  char_results <- results_all[["3_year_reject",23]]$candidates
  single_afp_result <- afp2840_54only_results
  afp_num<-2840
  char_col<-1
  
  expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_means,
                   char_results[char_results$candidate_afp==afp_num,"candidate_means"])
    
  expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$extrapolation,
               char_results[char_results$candidate_afp==afp_num,"extrapolation"])
      
  expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_adjusted_logSD,
               char_results[char_results$candidate_afp==afp_num,"candidate_adjusted_logSD"])
    
  expect_equal(single_afp_result[["3_year_reject",char_col]]$candidates$candidate_COYU_pvalue,
               char_results[char_results$candidate_afp==afp_num,"candidate_COYU_pvalue"])
})


