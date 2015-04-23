#' @include COYU_utilities.R
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


extract_characters <-function(trial_data) UseMethod("extract_characters")

extract_characters.COYUs9TrialData <-function(trial_data) {
  measurement_cols <- colnames(trial_data)[!colnames(trial_data) %in% c("year","AFP","variety")]

  return(
         as.numeric(unique(sapply(measurement_cols,function(x) { gsub("[^0-9]","",x) })))
         )  
}

extract_characters.COYUs9AllResults <-function(trial_data) {
  return(unlist(trial_data["character",]))
}

extract_varieties <-function(trial_data) UseMethod("extract_varieties")

extract_varieties.COYUs9TrialData <-function(trial_data) {
  return(unique(strip_numeric_factor(trial_data$AFP)))
}

extract_varieties.COYUs9Results <-function(trial_data) {
  return(unique(c(strip_numeric_factor(trial_data$reference[["reference_afp"]]),
                  strip_numeric_factor(trial_data$candidates[["candidate_afp"]])))
         )
}

extract_varieties.COYUs9AllResults <-function(trial_data) {
  return(extract_varieties(trial_data[[first_dataset(trial_data),1]]))
}

extract_candidate_afp <- function(trial_data) UseMethod("extract_candidate_afp")

extract_reference_afp <- function(trial_data) UseMethod("extract_reference_afp")

extract_candidate_names <- function(trial_data) UseMethod("extract_candidate_names")

extract_reference_names <- function(trial_data) UseMethod("extract_reference_names")

extract_candidate_afp.COYUs9AllResults <- function(trial_data) {
  return(strip_numeric_factor(trial_data[[first_dataset(trial_data),1]]$candidates[["candidate_afp"]]))
}

extract_candidate_afp.COYUs9Results <- function(trial_data) {
  return(strip_numeric_factor(trial_data$candidates[["candidate_afp"]]))
}

extract_reference_afp.COYUs9AllResults <- function(trial_data) {
  return(strip_numeric_factor(trial_data[[first_dataset(trial_data),1]]$reference[["reference_afp"]]))         
}

extract_reference_afp.COYUs9Results <- function(trial_data) {
  return(strip_numeric_factor(trial_data$reference[["reference_afp"]]))
}

extract_candidate_names.COYUs9AllResults<- function(trial_data) {
  return(strip_string_factor(trial_data[[first_dataset(trial_data),1]]$candidates[["candidate_varieties"]]))
}

extract_candidate_names.COYUs9Results<- function(trial_data) {
  return(strip_string_factor(trial_data$candidates[["candidate_varieties"]]))
}

extract_reference_names.COYUs9AllResults<- function(trial_data) {
  return(strip_string_factor(trial_data[[first_dataset(trial_data),1]]$reference[["reference_varieties"]]))
}

extract_reference_names.COYUs9Results<- function(trial_data) {
  return(strip_string_factor(trial_data$reference[["reference_varieties"]]))
}


#' extract_field
#'
#' Iterate over a set of results character-by-character to extract values
#' 
#' @param results COYUs9AllResults object
#' @param extractor Function that is iterated over the results object to extract some values
#' @param dataset_name Optional. Name of the dataset (e.g. 2_year_reject) If not supplied the results of \code{\link{first_dataset}} as applied to the "results" parameter will be used
#' @param row_names Optional. If supplied set the row names on the returned object 
#' @param col_names Optional. If supplied set the colummn names on the returned object
#'
#' @return Result of running the function "extractor" on all characters in "results". Usually a data.frame
#'
#' TODO: example:
#'
#' @export
extract_field <- function(results,extractor,dataset_name,row_names,col_names) UseMethod("extract_field")

#' @export
extract_field.COYUs9AllResults <- function(results,extractor,dataset_name,row_names,col_names) {
  if (missing(dataset_name)) {
    dataset_name<-first_dataset(results)
  }
  extracted<-matrix(sapply(results[dataset_name,],extractor),
                    nrow=length(extract_candidate_afp(results)),
                    ncol=length(extract_characters(results)))

  if (!missing(row_names)) {
    rownames(extracted)<-row_names
  }
  if (!missing(col_names)) {
    colnames(extracted)<-col_names
  }
  return(extracted)
}
