
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

#' find_matching_candidates
#'
#' Create a data frame containing candidates and characters that match a function
#'
#' @param results COYUs9AllResults object
#' @param characters List of characters to apply the match against. 
#' @param alpha_names Names of datasets to match against. If not supplied, dataset_names will be used to determine the valid datasets
#' @param match_func Match func, which should ideally be an S3 method on the COYUs9Predictions class. This should take a single predictions object, determine the "matching" candidate records and return them for consumption by the find_matching_candidates function.
#' @param ... Extra parameters to pass to match_func
#'
#' @return COYUs9CandidateMatches object containing all the matches
#'
#' @seealso find_matching_reference
#'@export
find_matching_candidates<-function(results,
                                   characters,
                                   alpha_names=dataset_names(results),
                                   match_func=find_nonuniform_candidates,
                                   ...) UseMethod("find_matching_candidates")

#' @export
find_matching_candidates.COYUs9AllResults<-function(results,
                                   characters,
                                   alpha_names=dataset_names(results),
                                   match_func=find_nonuniform_candidates,
                                   ...) {
  matching_candidates<-matrix(sapply(characters,function(character_number) {
    sapply(alpha_names,function(alpha_name) {
      predictions<-results[[alpha_name,which(results["character",]==character_number)]]
      local_matches<-match_func(predictions,...)      
      if (nrow(local_matches) > 0) {
        return(list(local_matches))
      } else {
        return(NA)
      }
    })
  }),
                              nrow=length(alpha_names),
                              ncol=length(characters))

  colnames(matching_candidates)<-sprintf("CHR%02d",characters)
  rownames(matching_candidates)<-alpha_names
  class(matching_candidates)<-c("COYUs9CandidateMatches",class(matching_candidates))
  return(matching_candidates)
}

#' find_extrapolation
#'
#' Find candidates that have been extrapolated
#'
#' @param predictions COYUs9Predictions object
#' @param ... Placeholder for extra parameters (not used)
#' 
#' @return Data frame of candidates which have had extrapolation applied
#' @seealso find_matching_candidates
#' @export
find_extrapolation<-function(predictions,...) UseMethod("find_extrapolation")

#' find_nonuniform_candidates
#'
#' Find candidates where the coyu_candidate_pvalue is below the
#' threshold set by the decision rule in use. This will depend on the
#' dataset you are looking at.
#'
#' @param predictions COYUs9Predictions object
#' @param ... placeholder for extra parameters (not used)
#'
#' @return Data frame of candidates which have not met the threshold set by the decision rule
#' @seealso find_matching_candidates
#' @export
find_nonuniform_candidates<-function(predictions,...) UseMethod("find_nonuniform_candidates")

#' @export
find_extrapolation.COYUs9Predictions<-function(predictions,...) {
  return(predictions$candidates[which(predictions$candidates[,"extrapolation"]==1),])
}

#' @export
find_nonuniform_candidates.COYUs9Predictions<-function(predictions,...) {
  return(predictions$candidates[which(predictions$candidates[,"candidate_not_uniform"]==TRUE),])
}
