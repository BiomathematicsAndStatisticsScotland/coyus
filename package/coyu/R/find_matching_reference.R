
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

#' find_matching_reference
#'
#' Create a data frame containing reference and characters that match a function
#'
#' @param results COYUs9AllResults object
#' @param characters List of characters to apply the match against. 
#' @param alpha_names Names of datasets to match against. If not supplied, dataset_names will be used to determine the valid datasets
#' @param match_func Match func, which should ideally be an S3 method on the COYUs9Predictions class. This should take a single predictions object, determine the "matching" reference records and return them for consumption by the find_matching_reference function.
#' @param ... Extra parameters to pass to match_func
#'
#' @return COYUs9ReferenceMatches object containing all the matches
#'
#' @seealso find_matching_reference has_matching_variety
#'@export
find_matching_reference.COYUs9AllResults<-function(results,
                                   characters,
                                   alpha_names=dataset_names(results),
                                   match_func=reference_sd_exceed_threshold,
                                   ...) UseMethod("find_matching_reference")

#'@export  
find_matching_reference.COYUs9AllResults<-function(results,
                                   characters,
                                   alpha_names=dataset_names(results),
                                   match_func=reference_sd_exceed_threshold,
                                   ...) {
  matching_reference<-matrix(sapply(characters,function(character_number) {
    sapply(alpha_names,function(alpha_name) {
      ref_results<-results[[alpha_name,which(results["character",]==character_number)]]
      local_matches<-match_func(ref_results,...)      
      if (nrow(local_matches) > 0) {
        return(list(local_matches))
      } else {
        return(NA)
      }
    })
  }),
                              nrow=length(alpha_names),
                              ncol=length(characters)
                             )

  colnames(matching_reference)<-sprintf("CHR%02d",characters)
  rownames(matching_reference)<-alpha_names
  class(matching_reference)<-c("COYUs9ReferenceMatches",class(matching_reference))
  return(matching_reference)
}

#' reference_sd_exceed_threshold
#'
#' @param char_results COYUs9Results object to analyse
#' @param ... Extra parameter, which should be a single numeric threshold usually taken from a  decision ruleset.
#' @return All reference varieties in a particular results set where the adjusted logSD exceeds the threshold
#' @export
reference_sd_exceed_threshold <- function(char_results, ...) UseMethod("reference_sd_exceed_threshold")

#' @export
reference_sd_exceed_threshold.COYUs9Results<-function(char_results, ...) {
  if (missing(...)) {
    stop("This function should be passed a threshold via the additional arguments to find_matching_reference")
  }
  
  threshold <- as.numeric(...)   
  return(char_results$reference[which(char_results$reference[,"reference_adjusted_logSD"] > threshold),])  
}
