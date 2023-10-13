
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

#' is_3_year
#'
#' @param coyu_obj COYU object to test
#' @return TRUE if object is related to 3 year trials.
#'@export                                        
is_3_year<-function(coyu_obj) {
  UseMethod("is_3_year")
}

#' is_2_year
#'
#' @param coyu_obj COYU object to test
#' @return TRUE if object is related to 2 year trials.
#'@export
is_2_year<-function(coyu_obj) {
  UseMethod("is_2_year")
}

#'@export
is_2_year.COYUs9Results<-function(coyu_obj) {
  return(length(coyu_obj$yearly_results) == 2)
}

#'@export
is_3_year.COYUs9Results<-function(coyu_obj) {
  return(length(coyu_obj$yearly_results) == 3)
}

#'@export
is_2_year.COYUs9AllResults<-function(coyu_obj) {
  return(!is.null(coyu_obj[["2_year_reject",1]]))
}

#'@export
is_3_year.COYUs9AllResults<-function(coyu_obj) {
  return(!is.null(coyu_obj[["3_year_reject",1]]))
}

#'@export
is_2_year.COYUs9Parameters<-function(coyu_obj) {
  return(coyu_obj$num_trial_years == 2)
}

#'@export
is_3_year.COYUs9Parameters<-function(coyu_obj) {
  return(coyu_obj$num_trial_years == 3)
}

#'@export
is_2_year.COYUs9TrialData<-function(coyu_obj) {
  return(nlevels(coyu_obj$year) == 2)
}

#'@export
is_3_year.COYUs9TrialData<-function(coyu_obj) {
  return(nlevels(coyu_obj$year) == 3)
}

