
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


#' has_matching_variety
#'
#' Method that operates on match data from find_matching_candidates and find_matching_varieties and returns true if the specified variety/character combination is a match at any point
#'
#' @param match_matrix Results from functions listed in the See Also section
#' @param character_numbers Character number(s) of interest. Numeric vector
#' @param alpha_names Datasets of interest. Character vector
#' @param variety_afp Variety AFP number(s) of interest. Numeric vector. 
#'
#' @seealso find_matching_candidates find_matching_varieties
#' @export                                        
has_matching_variety<-function(match_matrix,character_numbers,alpha_names,variety_afp) UseMethod("has_matching_variety")

has_matching_variety_base<-function(match_matrix,character_numbers,alpha_names,variety_afp,afp_variable) {
  character_names=sprintf("CHR%02d",character_numbers)

  for (alpha_name in alpha_names) {
    for (character_name in character_names) {
      if (alpha_name %in% rownames(match_matrix) &&
          character_name %in% colnames(match_matrix)) {
        matches<-match_matrix[alpha_name,character_name]
        if (!is.na(matches)) {

          #Condense down multiple match response to single TRUE/FALSE value to suppress warnings
          return(TRUE %in% (matches[[1]][,afp_variable]==variety_afp) )
        }
      }
    }  }

  return(FALSE)
}

#TODO: consider renaming variables in results so we don't need to have separate methods here
#' @export
has_matching_variety.COYUs9CandidateMatches<-function(match_matrix,character_numbers,alpha_names,variety_afp) {
  has_matching_variety_base(match_matrix,character_numbers,alpha_names,variety_afp,"candidate_afp")
}

#' @export
has_matching_variety.COYUs9ReferenceMatches<-function(match_matrix,character_numbers,alpha_names,variety_afp) {
  has_matching_variety_base(match_matrix,character_numbers,alpha_names,variety_afp,"reference_afp")
}

