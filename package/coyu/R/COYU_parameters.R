#' COYU_parameters
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
#' Construct a COYU parameters object.
#'
#' @param candidates List of candidate AFP numbers
#' @param references List of reference AFP numbers
#' @param characters List of character numbers
#' @param num_years Number of years in trial
#' @return parameters object
#' @seealso COYU_parameters_from_df
#' @export
COYU_parameters <- function(candidates,
                            references,
                            characters,
                            num_years) {

  if (!all(is.numeric(candidates),
           is.numeric(references),
           is.numeric(characters),
           is.numeric(num_years))) {
    stop("All arguments to this function should be numeric")
  }

  if (length(candidates) < 1) {
    stop("No candidate varieties provided")
  }

  if (length(references) < 1) {
    stop("No reference varieties provided")
  }
  
  
  if (any(candidates %in% references)) { 
    stop("Some variety AFPs present in both references and candidates")
  }
  
  parms=list()
  parms$candidates=candidates
  parms$references=references
  parms$characters=characters
  parms$num_trial_years=num_years
  
  class(parms)<-c("COYUs9Parameters","list")
  return(parms)
}

#' COYU_parameters_from_df
#'
#' Create a parameter set directly from a COYU dataframe
#'
#' @param trial_data COYUs9TrialData object
#' @param candidates List of candidate variety AFP numbers. Numeric vector 
#' @return COYUs9Parameters parameter set appropriate to the data frame 
#' 
#' Other parameters (characters,references etc) are set to include all relevant
#' data in the data frame - i.e. references is all the varieties less the ones designated as candidates
#'
#' Example: COYU_parameters_from_df(my_trial_data,c(51,7))
#' 
#' @export
COYU_parameters_from_df <- function(trial_data,candidates) UseMethod("COYU_parameters_from_df")

#' @export
COYU_parameters_from_df.COYUs9TrialData <- function(trial_data,candidates) {
  variety_afp <- extract_varieties(trial_data)

  if (missing(candidates)) {
    stop("Required parameter 'candidates' missing")
  }
  
  if (!is.numeric(candidates)) {
    stop("Candidates variable must be numeric")
  }

  if (!all(candidates %in% variety_afp)) {
    stop("One or more of the specified candidates are not present in trial data. Candidate varieties: (",paste(candidates,collapse=","),")")
  }

  if (length(candidates) >= length(variety_afp)) {
    stop("Number of candidates >= number of varieties in trial")
  }
  
  if (length(candidates) > (length(variety_afp) / 2)) {
    warning("You have specified ",
            length(candidates),
            " candidates. This is a large number for a trial with ",
            length(variety_afp),
            " varieties")
  }
  

  references <- setdiff(variety_afp,candidates)
  characters <- extract_characters(trial_data)
  num_years <- nlevels(trial_data$year)

  return(COYU_parameters(candidates,references,characters,num_years))
}
