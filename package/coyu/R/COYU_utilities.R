
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

#' name_stddev
#'
#' Convenience function to provide a character-based named for stddev measurements
#'
#' @param character_number Number of a character to name. 
#' @return standard deviation character name, produced from character_number
#'
#' @export
name_stddev<-function(character_number) {
  if (is.numeric(character_number)) {
    return(sprintf("sUP%02d",character_number))
  }
  stop(paste("character_number",character_number," is not numeric"))
}

#' name_stddev
#'
#' Convenience function to provide a character-based named for plot-mean measurements
#'
#' @param character_number Number of a character to name. 
#' @return standard deviation character name, produced from character_number
#'
#' @export
name_mean<-function(character_number) {
  if (is.numeric(character_number)) {
    return(sprintf("UP%02d",character_number))
  }
  stop(paste("character_number",character_number," is not numeric"))
}



#' strip_numeric_factor
#'
#' @param x Variable to convert
#' @return x without its levels
#'
#' Convenience function to convert a variable with levels to its non-level equivalent. Numeric form
#' @export
strip_numeric_factor<-function(x) {
  if (is.factor(x)) {    
    return(as.numeric(levels(x)[x]))
  }
  return(x)
}

#' strip_string_factor
#'
#' @param x Variable to convert
#' @return x without its levels
#'
#' Convenience function to convert a variable with levels to its non-level equivalent. String form
#' @export
strip_string_factor<-function(x) {
 if (is.factor(x)) {    
    return(as.character(levels(x)[x]))
  }
  return(x)
}

#' first_dataset
#'
#' For a variety of COYU objects, return the name of the first active dataset (e.g. 2_year_reject in the case of 2 year trials)
#' @param coyu_obj The COYU object to examine 
#' @return name of 1st active dataset
#'
#'@export
first_dataset<-function(coyu_obj) {
  UseMethod("first_dataset")
}

#'@export
first_dataset.COYUs9Parameters<-function(coyu_obj) {
  if (is_3_year(coyu_obj)) {
    return("3_year_reject")
  }
  return("2_year_reject")
}

#'@export
first_dataset.COYUs9CharacterResults<-first_dataset.COYUs9Parameters

#'@export
first_dataset.COYUs9AllResults<-first_dataset.COYUs9Parameters

#' dataset_names
#'
#' For a variety of COYU objects, return the name of all active datasets (e.g. "3_year_reject" only in the case of 3 year trials)
#' 
#' @param coyu_obj The COYU object to examine
#' @return names of all active dataset
#'@export
dataset_names<-function(coyu_obj) {
  UseMethod("dataset_names")
}

#'@export
dataset_names.COYUs9Parameters<-function(coyu_obj) {
  if (is_3_year(coyu_obj)) {
    return(c("3_year_reject"))
  }
  return(c("2_year_reject","2_year_accept"))
}

#'@export
dataset_names.COYUs9CharacterResults<-dataset_names.COYUs9Parameters

#'@export
dataset_names.COYUs9AllResults<-dataset_names.COYUs9Parameters

