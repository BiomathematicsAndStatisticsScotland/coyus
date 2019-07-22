#' COYU_predictions
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

#'
#' Create predictions about the results for a particular character.
#' 
#' Note: this relies on carefully ordered and balanced data set (as provided by sim prog)
#'
#' TODO: User warning required in case of non-ordered/balanced data
#' TODO: testthat testcases required for this function and others in this file
#' TODO: document the layout of the COYUs9Predictions object
#'
#' @param coyu_results is the results object for a single character, returned from COYU.using.spline.4df
#' @param alpha is probability level for COYU test
#' @return COYUs9Predictions object
#
#' @export
#' @importFrom stats qt 
COYU_predictions<-function(coyu_results,alpha=0.05) {

  candidate_coyu_threshold<-
    (coyu_results$reference_mean_logSD+
     qt(
        p=(1-alpha),
        df=coyu_results$spline_df
        )* coyu_results$candidates$candidate_prediction_err
     )

  candidate_not_uniform<-(coyu_results$candidates$candidate_COYU_pvalue < alpha )

  predictions<-cbind(coyu_results$candidates,
                     data.frame(alpha,candidate_coyu_threshold,candidate_not_uniform))

  coyu_results$candidates<-predictions

  current_classes<-c("COYUs9Predictions",class(coyu_results))
  class(coyu_results)<-current_classes
  return(coyu_results)
}
