
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

#' COYU_all_results
#'
#' Wrapper function to carry out the COYU analysis. The function processes 
#' trial data and parameters to produce a raw results set. The results object 
#' can be printed using `COYU_print_results`.
#'
#' @param trial_data `COYUs9TrialData` object
#' @param coyu_parameters `COYUs9Parameters` object
#' @param probability_sets `COYUs9ProbabilitySet` object
#' @return `COYUs9AllResults` object
#'
#' @seealso COYU_sanity_check COYU_parameters_from_df COYU_probability_sets COYU_plot_results COYU_print_results COYU_results_as_dataframe
#'
#' @examples
#' ## an example using the test_2_year example included in the COYU package
#' 
#' data(test_2_year,package="coyu") 
#' 
#' results1<-COYU_all_results(test_2_year$trial_data,test_2_year$coyu_parameters,test_2_year$probability_sets)[[1]]
#' ## note [[1]] selects the results for the first probability set
#' 
#' COYU_print_results(results1, test_2_year$coyu_parameters, test_2_year$character_key, test_2_year$probability_set[1,])
#' ## note test_2_year$probability_set[1,] gives the probabilities for this set
#' 
#' write.csv(COYU_results_as_dataframe(results1, "2_year_reject"), "tester.csv")
#' 
#' COYU_plot_results(results1, character_key = test_2_year$character_key, plot_file="MyPlots.pdf")
#' ## results sent to a pdf file.
#'
#' @export

COYU_all_results<-function(trial_data,coyu_parameters,probability_sets) UseMethod("COYU_all_results")

#'@export
COYU_all_results.COYUs9TrialData<-function(trial_data,coyu_parameters,probability_sets) {
  
  if (!COYU_sanity_check(trial_data,coyu_parameters)) {
    stop("Sanity check of data set and parameters failed. Aborting COYU run")
  }

  if (! "COYUs9ProbabilitySet" %in% class(probability_sets)) {
    stop("probability_sets does not have class COYUs9ProbabilitySet. Aborting")
  }
  
  results<-apply(probability_sets,1,function(prob_set) {
    intermediate_results <- sapply(coyu_parameters$characters, run_COYU, trial_data, coyu_parameters, prob_set)
    class(intermediate_results) <- "COYUs9AllResults"
    return(intermediate_results)
  })

  class(results)<-list("COYUs9ResultsList","list")
  return(results)
}
