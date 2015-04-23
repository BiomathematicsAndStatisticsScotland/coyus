
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

#' get_trial_years
#'
#' Convenience method to retrieve the trial years from various COYU object
#' 
#' @param coyu_obj A COYU-related object to test, e.g. trial data, parameters etc
#' @return Trial years in the object as a vector, e.g. c(2011,2012)
#'
#' @export                                        
get_trial_years<-function(coyu_obj) UseMethod("get_trial_years")

#' get_trial_years
#'
#' Convenience method to retrieve the number of trial years from various COYU object
#' 
#' @param coyu_obj A COYU-related object to test, e.g. trial data, parameters etc
#' @return Number of trial years in the object, either 2 or 3
#'
#' @export
get_num_trial_years<-function(coyu_obj) UseMethod("get_num_trial_years")

#' @export
get_trial_years.COYUs9TrialData<-function(coyu_obj) {
  levels(coyu_obj$year)
}

#' @export
get_num_trial_years.COYUs9TrialData<-function(coyu_obj) {
  nlevels(coyu_obj$year)
}

#' @export
get_num_trial_years.COYUs9Parameters<-function(coyu_obj) {
  return(coyu_obj$num_trial_years)
}
