
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


#' get_varieties
#'
#' Convenience method to retrieve variety data from a COYUs9TrialData object
#'
#' @param trial_data COYUs9TrialData object
#' @param variety_afp List of AFP numbers that appear in trial_data
#' @param character_number Optional. By default data for all characters is returned
#' @return subset of trial_data containing only the selected varieties
#'
#' @export                                        
get_varieties<-function(trial_data,variety_afp,character_number=NA) UseMethod("get_varieties")

#' @export
get_varieties.COYUs9TrialData<-function(trial_data,variety_afp,character_number=NA) {
  
  if (is.na(character_number[[1]])) {
    varieties<- trial_data[which(trial_data[,"AFP"] %in% variety_afp), ]
  } else {
    character_names<-c(name_stddev(character_number),name_mean(character_number))
    
    if (all(character_names %in% colnames(trial_data))) {
      varieties<- trial_data[which(trial_data[,"AFP"] %in% variety_afp),
                             c("year","variety","AFP",character_names)]
    } else {
      stop("The mean, stddev or both of characters ",paste(character_number,collapse=",")," is missing from the trial data")
    }
  }

  varieties$AFP <- factor(varieties$AFP)

  return(varieties)
}


