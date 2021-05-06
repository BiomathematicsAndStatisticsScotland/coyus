
#'COYU_print_all_results
#'
#' Print a list of COYU results in a pleasant human-readable format
#'
#' @param results_list COYUs9ResultsList object
#' @param coyu_parameters COYUs9Parameters object describing the parameter set used to generate the results in the first parameter
#' @param character_key COYUs9CharacterKey object containing two columns, the first (CCode) a numeric character code and the second (CName) a text description of the character
#' @param probability_sets Numeric vector with named values 2_year_reject, 2_year_accept and 3_year_reject. Usually extracted from a COYUs9ProbabilitySet object
#' @param connection Optional, default "". Pass a connection to which to write the results. By default the results will be written to STDOUT.
#' @param verbose Optional, default TRUE. Print per-character results in addition to the summary file. 
#' @seealso COYU_print_results
#'@export
COYU_print_all_results <- function(results_list,
                                   coyu_parameters,
                                   character_key,
                                   probability_sets,
                                   connection="",
                                   verbose=TRUE) UseMethod("COYU_print_all_results")

COYU_print_all_results.COYUs9ResultsList <- function(results_list,
                                                     coyu_parameters,
                                                     character_key,
                                                     probability_sets,
                                                     connection="",
                                                     verbose=TRUE)
{
  if (nrow(probability_sets) != length(results_list)) {
    stop(sprintf("results list length (%d) and number of probability sets (%d) must be equal",length(results_list),nrow(probability_sets)))
  }

  for (i in 1:length(results_list) ) {
    COYU_print_results(results_list[[i]],coyu_parameters,character_key,probability_sets[i,],connection,verbose)
  }
}

#' COYU_print_all_results.list
#'
#' Print a list of COYUs9AllResults objects
#'
#' Deprecate this in the fullness of time as its functionality is provided by
#' COYU_print_all_results.COYUs9ResultsList
#' 
#' @param results_list COYUs9ResultsList object
#' @param coyu_parameters COYUs9Parameters object describing the parameter set used to generate the results in the first parameter
#' @param character_key COYUs9CharacterKey object containing two columns, the first (CCode) a numeric character code and the second (CName) a text description of the character
#' @param probability_sets Numeric vector with named values 2_year_reject, 2_year_accept and 3_year_reject. Usually extracted from a COYUs9ProbabilitySet object
#' @param connection Optional, default "". Pass a connection to which to write the results. By default the results will be written to STDOUT.
#' @param verbose Optional, default TRUE. Print per-character results in addition to the summary file. 
#'@export
COYU_print_all_results.list <- function(results_list,
                                        coyu_parameters,
                                        character_key,
                                        probability_sets,
                                        connection="",
                                        verbose=TRUE)
{
  class(results_list)<-append(class(results_list),"COYUs9ResultsList")
  COYU_print_all_results.COYUs9ResultsList(results_list,coyu_parameters,character_key,probability_sets,connection,verbose)
}
