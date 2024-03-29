
# Copyright (c) 2015, Biomathematics and Statistics Scotland
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
# 
#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
# 
#     Neither the name of Biomathematics and Statistics Scotland nor the
#     names of its contributors may be used to endorse or promote
#     products derived from this software without specific prior written
#     permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' COYU_results_as_dataframe
#'
#' Transform a COYU results object into a useful dataframe format. 
#'
#' @param results COYUs9AllResults object
#' @param alpha_name Optional. Name of the dataset to transform
#'     (e.g. "2_year_reject"). If not provided first_dataset will be
#'     used to find a dataset.
#' @return data frame containing the results of interest
#' @examples
#' 
#' ## an example using the test_2_year example included in the COYU package
#' 
#' data(test_2_year,package="coyu") 
#' 
#' results1<-COYU_all_results(test_2_year$trial_data,
#'                            test_2_year$coyu_parameters,
#'                            test_2_year$probability_sets)[[1]]
#' 
#' write.csv(COYU_results_as_dataframe(results1, "2_year_reject"),
#'           "tester.csv")
#' 
#' @export                                        
COYU_results_as_dataframe <- function(results,
                                      alpha_name=first_dataset(results)) {
  UseMethod("COYU_results_as_dataframe")
}


#' @export
COYU_results_as_dataframe.COYUs9AllResults <- function(results,
                                                       alpha_name=first_dataset(results)) {

    output_results<-as.matrix(results[c("character",alpha_name), ])
    rownames(output_results)<-c("character","result")
        
    ret=do.call(rbind,
                apply(output_results,2,function(x) {
                    char_results = COYU_results_as_dataframe(x$result, alpha_name)
                    characters=data.frame(character_number=rep(x$character, nrow(char_results)))
                    return (cbind(characters,char_results))                                          
                }))

    class(ret) = append(class(ret), "COYUs9AllResultsDF")
    return (ret)                   
}

#'@export
COYU_results_as_dataframe.COYUs9CharacterResults <- function(results,
                                                             alpha_name=first_dataset(results)) {

    ref_target_columns=c("reference_varieties","reference_afp",
                         "reference_means","reference_actual_logSD","reference_adjusted_logSD")

    ref_rename_columns=c("variety","AFP",
                         "mean","actual_logSD","adjusted_logSD")

    cand_target_columns=c("candidate_varieties","candidate_afp",
                          "extrapolation","extrapolation_factor",
                          "candidate_means","candidate_actual_logSD","candidate_adjusted_logSD",
                          "candidate_COYU_pvalue",
                          "candidate_prediction_err",
                          "candidate_coyu_threshold",
                          "candidate_not_uniform")
    
    cand_rename_columns=c("variety","AFP",
                          "extrapolation","extrapolation_factor",
                          "mean","actual_logSD","adjusted_logSD",
                          "candidate_COYU_pvalue",
                          "candidate_prediction_err",
                          "candidate_coyu_threshold",
                          "candidate_not_uniform")
    
    overall_data <- function(variety_data, col_list, col_renaming) {     
        overall = variety_data[, col_list ]
        colnames(overall) = col_renaming
        return(overall)
    }
                             
    yearly_data <- function(extracted_result, col_list, output_col_template) {
        Reduce(
            function (a, b) {
                merge(a,b, by=c("AFP"), all=TRUE)
            },
            lapply(
               extracted_result,
               function (yrly) {
                   yrly_ret = yrly[, col_list]
                   colnames(yrly_ret)=sprintf(output_col_template, as.character(yrly$year[1]))
                   return (yrly_ret)
               })
        )
    }

    ref_overall = cbind(
        data.frame(is_candidate=rep(0, nrow(results$reference))),
        overall_data(results$reference,
                     ref_target_columns,
                     ref_rename_columns)
    )
    
    cand_overall = cbind(
        data.frame(is_candidate=rep(1, nrow(results$candidate))),
        overall_data(results$candidate,
                     cand_target_columns,
                     cand_rename_columns)
    )

    ## Note: if you change the column names here, remember to update
    ## the re-ordering patterns at the end of this function

    ref_yearly_patterns = c("Mean_%s", "Log(SD+1)_%s", "AdjLog(SD+1)_%s")    
    cand_yearly_patterns = c("Extrapolation_%s", "Mean_%s", "Log(SD+1)_%s", "AdjLog(SD+1)_%s")
    reordered_yearly = sapply(cand_yearly_patterns,
                              function (pattern) sprintf(pattern, get_trial_years(results)))
    
    ref_yearly = yearly_data(
        extract_yearly_result(results$yearly_results,"ref_results"),
        c("AFP","mn","logSD","adjusted_logSD"),
        c("AFP",ref_yearly_patterns))

    cand_yearly = yearly_data(
        extract_yearly_result(results$yearly_results,"cand_results"),
        c("AFP","extrapolation_factor", "mn","logSD","adjusted_logSD"),
        c("AFP",cand_yearly_patterns)
    )
    
    cand_flattened = merge(
        cand_overall,
        cand_yearly,
        by=c("AFP"),
        all=TRUE)
                    
    ref_flattened = merge(
        ref_overall,
        ref_yearly,
        by=c("AFP"),
        all=TRUE)
    
    ## Add extra columns to flattened reference data filled with NA
    extra_cols = setdiff(colnames(cand_flattened), colnames(ref_flattened))
    ref_flattened = cbind(
        ref_flattened,
        data.frame(
            matrix(nrow=nrow(ref_flattened),
                   ncol=length(extra_cols),
                   dimnames=list(NULL,extra_cols))
        )
    )
                        
    all_data=rbind(cand_flattened,
                   ref_flattened)      

    yearly_cols = get_yearly_col_names.COYUs9CharResultsDF(all_data)
    new_order = append(setdiff(names(all_data),
                               yearly_cols),
                       reordered_yearly)

    results_df = all_data[,new_order]
    class(results_df) = append(class(results_df), "COYUs9CharResultsDF")
    return (results_df)
}


