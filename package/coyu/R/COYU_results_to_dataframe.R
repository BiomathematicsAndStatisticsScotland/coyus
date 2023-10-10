
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
#' @param alpha_name Optional. Name of the dataset to transform (e.g. "2_year_reject"). If not provided first_dataset will be used to find a dataset.
#' @param what Optional. Whether to provide "candidate" or "reference" data. Default "candidate"
#' @return data frame containing the results of interest
#' @examples
#' ## an example using the test_2_year example included in the COYU package
#' 
#' data(test_2_year,package="coyu") 
#' 
#' results1<-COYU_all_results(test_2_year$trial_data,
#'                            test_2_year$coyu_parameters,
#'                            test_2_year$probability_sets)[[1]]
#' ## note [[1]] selects the results for the first probability set
#' 
#' COYU_print_results(results1,
#'                    test_2_year$coyu_parameters,
#'                    test_2_year$character_key,
#'                    test_2_year$probability_set[1,])
#' ## note test_2_year$probability_set[1,] gives the probabilities for this set
#' 
#' write.csv(COYU_results_as_dataframe(results1, "2_year_reject"),
#'           "tester.csv")
#' 
#' COYU_plot_results(results1,
#'                   character_key = test_2_year$character_key,
#'                   plot_file="MyPlots.pdf")
#' ## results sent to a pdf file.
#'
#' @export                                        
COYU_results_as_dataframe <- function(results,
                                      alpha_name=first_dataset(results),
                                      what = c("candidate")) {
  UseMethod("COYU_results_as_dataframe")
}

#'@export
COYU_results_as_dataframe.COYUs9AllResults<-function(results,
                                                     alpha_name=first_dataset(results),
                                                     what="candidate") {
  
  
  if (what=="candidate") {
      target_columns=c("candidate_varieties","candidate_afp",
                       "extrapolation","extrapolation_factor",
                       "candidate_means","candidate_actual_logSD","candidate_adjusted_logSD",
                       "candidate_COYU_pvalue","candidate_prediction_err","candidate_coyu_threshold",
                       "candidate_not_uniform")
      afp_col="candidate_afp"
      mean_col="cand_mean"
      logsd_col="cand_logsd"
      adj_logsd_col="cand_adjlogsd"
      variety_col="candidate_varieties"      
  } else if (what=="reference") {
      target_columns=c("reference_varieties","reference_afp",
                       "reference_means","reference_actual_logSD","reference_adjusted_logSD")
      afp_col="reference_afp"
      mean_col="ref_mean"
      logsd_col="ref_logsd"
      adj_logsd_col="ref_adjlogsd"
      variety_col="reference_varieties"
  } else {
      ## TODO: add "combined" option which will emit a single dataframe with a "is_candidate" column
      stop("Invalid value for parameter 'what'=%s. Valid values are c('candidate','reference')",
           what)
  }

  ## Select results we're interested in
  output_results<-as.matrix(results[c("character",alpha_name), ])
  rownames(output_results)<-c("character","result")    
  
  ret=do.call(rbind,
              apply(output_results,2,function(x) {

                  if (what=="candidate") {                
                      varieties =x$result$candidates
                  } else {
                      varieties = x$result$reference
                  }

                  mean_data = mean_sd_cols_as_dataframe(x$result,
                                                        varieties[,afp_col],
                                                        mean_col,
                                                        "Mean_%s")
                  sd_data = mean_sd_cols_as_dataframe(x$result,
                                                      varieties[,afp_col],
                                                      logsd_col,
                                                      "Log(SD+1)_%s")
                  adj_logsd_data = mean_sd_cols_as_dataframe(x$result,
                                                             varieties[,afp_col],
                                                             adj_logsd_col,
                                                             "AdjLog(SD+1)_%s")
                  
                  characters<-data.frame(character_number=rep(x$character,nrow(varieties)))

                  ret2 = cbind(characters, varieties[,target_columns])

                  merge(
                      ret2,
                      merge(mean_data,
                            merge(sd_data,
                                  adj_logsd_data,
                                  by=c("AFP")),
                            by=c("AFP")),
                      by.x=afp_col,
                      by.y=c("AFP"))
              }))

    return(ret)
}


