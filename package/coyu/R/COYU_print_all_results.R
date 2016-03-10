
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
#' Deprecate this in the fullness of time. Also see about shipping parameters as attributes to the result set/list. Much better
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
