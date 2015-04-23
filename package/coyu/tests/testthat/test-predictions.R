context("Test COYU predictions")

data(test_2_year)
data(test_2_year_withmissing)
data(test_3_year_withmissing)
data(test_2_year_dirty) 

#Run COYU routines and generate predictions
print("Computing results for test dataset 1")
results1<-COYU_all_results(test_2_year$trial_data,
                           test_2_year$coyu_parameters,
                           test_2_year$probability_sets)[[1]]

print("Computing results for test dataset 2")
results2<-COYU_all_results(test_2_year_withmissing$trial_data,
                           test_2_year_withmissing$coyu_parameters,
                           test_2_year_withmissing$probability_sets)[[1]]

print("Computing results for test dataset 3")
results3<-COYU_all_results(test_3_year_withmissing$trial_data,
                           test_3_year_withmissing$coyu_parameters,
                           test_3_year_withmissing$probability_sets)[[1]]

#Some checks to make sure results are roughly what we expect. 

test_that("Correct result set dimensions",{
  expect_true(is_2_year(results1))
  expect_true(is_2_year(results2))
  expect_true(is_3_year(results3))
})

test_that("Expected characters in results",{
  expect_that(setdiff(as.numeric(results1["character",]),
                      test_2_year$coyu_parameters$characters),
              is_equivalent_to(numeric(0)))

    expect_that(setdiff(as.numeric(results2["character",]),
                      test_2_year_withmissing$coyu_parameters$characters),
              is_equivalent_to(numeric(0)))

  expect_that(setdiff(as.numeric(results3["character",]),
                      test_3_year_withmissing$coyu_parameters$characters),
              is_equivalent_to(numeric(0)))
})

test_that("Dirty dataset fails sanity check",{
  expect_false(COYU_sanity_check(test_2_year_dirty$trial_data,test_2_year_dirty$coyu_parameters))
})

test_that("Other dirty dataset fails sanity check",{
  with_negs<-test_2_year$trial_data
  with_negs[1,4]=-10
  with_negs[3,8]=-1
  expect_false(COYU_sanity_check(with_negs,test_2_year$coyu_parameters))
})

test_that("Predictions in resultset 1",{
  result_df <- COYU_results_as_dataframe(results1)
  expect_equal( nrow(result_df), 34)
  expect_equal( result_df[result_df$character_number==8,"candidate_means"], c(75.0315,83.6), tolerance=1e-04)
  expect_equal( result_df[result_df$character_number==70,"candidate_adjusted_logSD"], c(2.157,2.292), tolerance=1e-04)
})


test_that("Predictions in resultset 2",{
  result_df <- COYU_results_as_dataframe(results2)
  expect_equal( result_df[result_df$character_number==41,"candidate_not_uniform"],c(FALSE,TRUE), tolerance=1e-04)
  expect_equal( result_df[result_df$character_number==41,"candidate_means"],c(68.3500, 74.5475), tolerance=1e-04)
})

test_that("Predictions in resultset 3",{
  result_df <- COYU_results_as_dataframe(results3)
  expect_equal(result_df[result_df$candidate_afp==2709,"candidate_prediction_err"],
               c(0.01878927, 0.05940191, 0.06144965, 0.05827952, 0.06774688, 0.05963770,
                 0.07558062, 0.06415667, 0.05904060, 0.05188961, 0.03553067, 0.06034569,
                 0.05042147, 0.05703776, 0.06131047, 0.05556145, 0.05544977, 0.04310948,
                 0.05164742, 0.04798215, 0.03215538, 0.01330550, 0.01650311, 0.06808038,
                 0.01736875, 0.01660654, 0.01612078, 0.04671508, 0.05951884, 0.05897640,
                 0.02784152, 0.05607608, 0.05689484, 0.06772849, 0.07675528), tolerance=1e-04)
})

test_that("Single character trial - local data",{
  the_con<-textConnection("text_con", open="w")
  params3<-COYU_parameters_from_df(candidates=c(109,994),trial_data=test_2_year$trial_data)
  params3$characters=c(4)
  res3<-COYU_all_results(coyu_parameters=params3,
                         probability_sets=test_2_year$probability_sets,
                         trial_data=test_2_year$trial_data)
  COYU_print_results(res3[[1]],
                     params3,
                     test_2_year$character_key,
                     test_2_year$probability_sets[1,],
                     connection=the_con)
  close(the_con)
})

test_that("Single candidate trial - local data",{
  the_con<-textConnection("text_con", open="w")
  params<-COYU_parameters_from_df(candidates=c(109),trial_data=test_2_year$trial_data)
  res1<-COYU_all_results(coyu_parameters=params,probability_sets=test_2_year$probability_sets,trial_data=test_2_year$trial_data)
  COYU_print_results(res1[[1]],params,test_2_year$character_key,test_2_year$probability_sets[1,],connection=the_con)
  close(the_con)
})

test_that("Single candidate trial - marcin's data", {
  the_con<-textConnection("text_con", open="w")

  testdata_root = file.path(system.file(package="coyu"),"extdata","marcin-testdata")
  
  X1<-read.table(file.path(testdata_root,"means1.txt"),h=F)
  X2<-read.table(file.path(testdata_root,"means2.txt"),h=F)
  std1<-read.table(file.path(testdata_root,"std1.txt"))
  std2<-read.table(file.path(testdata_root,"std2.txt"))
  x1<-as.matrix(X1)
  x2<-as.matrix(X2)
  std11<-as.matrix(std1)
  std22<-as.matrix(std2)

  test_2_year
  years=c(1992,1993)
  characters=c(1,2,3,4,5,6,7,8)
  varieties=c(9555,13276,8366,2008,2007,2006,8367,3820,2018,13255,3818,2005,18489,2002,7407,
    10326,2011,7619,10877,7035,3819,10879,11668,9720,10878,16870,10521,12499,12387,
    2010,19537,12498)

  fake_mean_data<-rbind(x1,x2)
  fake_stddev_data<-rbind(std11,std22)

### tworzenie danych wejsciowych 
  trial_data<-COYU_data_skeleton(years,characters,varieties,
                                 mean_data=fake_mean_data,
                                 stddev_data=fake_stddev_data)
  is.list(trial_data)

  a<-COYU_parameters_from_df(trial_data,c(18489))
                                        #,9555,8367,9720
  y<-COYU_probability_set(reject_3_year = c(0.05,0.02,0.01), 
                          reject_2_year = c(0.05,0.02,0.01), accept_2_year = c(0.1,0.05,0.02))
  y
### tworzenie danych wejsciowych 
  trial_data<-COYU_data_skeleton(years,characters,varieties,
                                 mean_data=fake_mean_data,
                                 stddev_data=fake_stddev_data)

  trial_data

  params <- COYU_parameters_from_df(trial_data,c(18489))
  params
  COYU_sanity_check(trial_data = trial_data, 
                    coyu_parameters = params)


  marcin_results<-COYU_all_results(trial_data,
                             a,
                             y)[[1]]
  char_key<-COYU_character_key(c(1:8),c("RNWW","ZDNZ","ZDGM","LFD","LFS","KD","LFW","LFK"))

                                        #test_2_year$probability_sets
### Wydruk
  COYU_print_results(marcin_results,
                     a,
                     char_key,
                     y[1,],
                     connection=the_con)

  df <- COYU_results_as_dataframe(marcin_results)
  
  COYU_plot_results(marcin_results, trial_data$character_key, plot_options=2, plot_file=file.path(tempdir(),"plot.pdf"))

  close(the_con)
})
