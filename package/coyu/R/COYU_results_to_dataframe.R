
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
#' @return data frame containing the results of interest
#'
#' @export                                        
COYU_results_as_dataframe <- function(results,alpha_name=first_dataset(results)) {
  UseMethod("COYU_results_as_dataframe")
}

#'@export
COYU_results_as_dataframe.COYUs9AllResults<-function(results,alpha_name=first_dataset(results)) {

  #Select results we're interested in
  output_results<-as.matrix(results[c("character",alpha_name), ])
  rownames(output_results)<-c("character","result")

  target_columns=c("candidate_varieties","candidate_afp",
    "extrapolation","regression_factor","extrapolation_factor",
    "candidate_means","candidate_actual_logSD","candidate_adjusted_logSD",
    "candidate_COYU_pvalue","candidate_prediction_err","candidate_coyu_threshold",
    "candidate_not_uniform")
  
  do.call(rbind,
          apply(output_results,2,function(x) {
            candidates<-x$result$candidates
            characters<-data.frame(character_number=rep(x$character,nrow(candidates)))
            
            cbind(characters,candidates[,target_columns])
          })) 
}


