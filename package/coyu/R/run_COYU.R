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

run_COYU<-function(character_number,trial_data,coyu_parameters,probability_set) {
  #prepare the data. This could be moved into the COYU_single_character function itself
  character_stddev=name_stddev(character_number)
  character_mean=name_mean(character_number)
  
  data_candidates<-get_varieties(trial_data,coyu_parameters$candidates,character_number)
  data_references<-get_varieties(trial_data,coyu_parameters$references,character_number)
  
  data_candidates$logSD<-log(data_candidates[,character_stddev]+1)
  data_references$logSD<-log(data_references[,character_stddev]+1)

  #TODO: this rename is only required by Adrian's code
  names(data_candidates)[names(data_candidates)==character_mean] <- "mn"
  names(data_references)[names(data_references)==character_mean] <- "mn"
  
  #Run probability tests - TODO: this is a very DUST-like way of looking at how to do this; maybe move this bit to DUST code 
  alpha_crit3=as.numeric(probability_set["3_year_reject"])
  alpha_crit2=as.numeric(probability_set["2_year_reject"])
  alpha_crit2a=as.numeric(probability_set["2_year_accept"]) 

  coyu_results<-COYU_single_character(character_number=character_number,
                                      dat.ref=data_references,
                                      dat.cand=data_candidates)

  #Reorder results to input order as described in parameters object. Otherwise the
  #lexical order of candidates/references in the first year is used. 
  ref_order<-match(coyu_parameters$reference, coyu_results$reference$reference_afp)
  cand_order<-match(coyu_parameters$candidates, coyu_results$candidates$candidate_afp)
  
  coyu_results$reference<-coyu_results$reference[ref_order,]
  coyu_results$candidates<-coyu_results$candidates[cand_order,]

  
  if (is_3_year(coyu_parameters)) {
    crit3_results=COYU_predictions(coyu_results,alpha=alpha_crit3)
    crit2_results=NULL
    crit2a_results=NULL
  } else {
    crit3_results=NULL
    crit2_results=COYU_predictions(coyu_results,alpha=alpha_crit2)    
    crit2a_results=COYU_predictions(coyu_results,alpha=alpha_crit2a)    
  }
  
  return(list(character=character_number,
              "3_year_reject"=crit3_results,
              "2_year_reject"=crit2_results,
              "2_year_accept"=crit2a_results)) 
}
