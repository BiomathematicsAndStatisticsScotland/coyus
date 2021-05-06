#' COYU_results_by_character
#'
#' Utility method to return all results for particular character(s)
#'
#' @param results COYUs9AllResults object to operate on
#' @param character_number Vector of character numbers to select
#' @param dataset Name of dataset from which to obtain results (e.g. 2_year_reject, 2_year_accept etc)
#'
#' @examples
#'
#' data(test_2_year,package="coyu") 
#' 
#' results1<-COYU_all_results(test_2_year$trial_data,
#'                            test_2_year$coyu_parameters,
#'                            test_2_year$probability_sets)[[1]]
#'
#' char_3_results=COYU_results_by_character(results1, c(3, 5), dataset="2_year_reject")[[1]]
#' 
#' @export
COYU_results_by_character<-function(results, character_number, dataset="2_year_reject") {
    UseMethod("COYU_results_by_character")
}

#' @export
COYU_results_by_character.COYUs9AllResults<-function(results, character_number, dataset="2_year_reject") {
    if (! dataset %in% dataset_names(results)) {
        stop(sprintf("Dataset %s not present in these results which contain names %s", dataset, paste(dataset_names(results), collapse=",")))
    }

    character_ind = results["character",] %in% character_number

    if (any(character_ind)) {
        return (results[dataset,character_ind])
    }

    return(NULL)
}
