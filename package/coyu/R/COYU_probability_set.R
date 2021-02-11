
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

#' COYU_probability_set
#'
#' Convenience function to create sets of probability levels for use by COYU_all_results and other functions.
#' All arguments are required and should be numeric vectors of identical length in the range 0-0.5. Multiple sets of probability levels are permitted, but in general only one set will be need. See https://www.upov.int/edocs/tgpdocs/en/tgp_8.pdf for further guidance.
#' 
#' @param reject_3_year probability level for 3rd year of 3 year trials. This value should be set even for 2 year trials (but have no effect). A value of 0.003 is recommended 
#' @param reject_2_year probability level for rejection in 2nd year. For 2 year trials, reject_2_year should equal accept_2_year. For 3 year trials, reject_2_year is usually set the same as reject_3_year. A value of 0.003 is recommended 
#' @param accept_2_year probability level for rejection in 2nd year. For 2 year trials, accept_2_year should equal reject_2_year. For 3 year trials, accept_2_year is usually larger than reject_2_year and reject_3_year. A value of 0.02 is recommended  
#' 
#'
#' @return COYUs9ProbabilitySet object
#'
#' @examples
#'
#' COYU_probability_set(reject_3_year = c(0.003), reject_2_year = c(0.003), accept_2_year = c(0.02))
#' @export
COYU_probability_set<-function(reject_3_year=NULL,reject_2_year=NULL,accept_2_year=NULL) {
  expected_size=max(length(reject_2_year),length(reject_3_year),length(reject_3_year))
  
  if (length(reject_2_year)!=length(accept_2_year)) {
    stop("Different number of reject/accept values for 2 year trials")
  }
  
  if (is.null(reject_3_year)) {
    reject_3_year=rep(NA,expected_size)
  }
  
  if (is.null(reject_2_year)) {
    reject_2_year=rep(NA,expected_size)
  }
  
  if (is.null(accept_2_year)) {
    accept_2_year=rep(NA,expected_size)
  }
  
  if (min(length(reject_2_year),length(reject_3_year),length(reject_3_year)) !=
      expected_size) {
    stop("Arguments to function COYU_probability_set are of different lengths")
  }
  contents<-c(reject_3_year,reject_2_year,accept_2_year)
  
  MAX_PROBABILITY = 0.5
  if (any(contents < 0 | contents > MAX_PROBABILITY)) {
    stop(sprintf("Arguments to function COYU_probability_set are probabilities and should be in the range 0 to %s",MAX_PROBABILITY))
  }
  
  ret<-matrix(contents,
              nrow=expected_size,
              ncol=3,
              byrow=FALSE,                     
              dimnames=list(
                NULL,
                c("3_year_reject","2_year_reject","2_year_accept")
              ))
  
  class(ret)<-c("COYUs9ProbabilitySet","matrix")
  
  return(ret)
  #TODO: consider returning data frame here.
}
