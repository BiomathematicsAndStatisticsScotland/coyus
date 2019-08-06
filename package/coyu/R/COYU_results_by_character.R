#' @export
COYU_results_by_character<-function(results, character_number, dataset="2_year_reject") {
    UseMethod("COYU_results_by_character")
}

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
