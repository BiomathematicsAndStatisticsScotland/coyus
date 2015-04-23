
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

#' COYU_character_key
#'
#' Generate a character key with the appropriate type to be used in COYU_print_results
#'
#' @param character_codes Vector of numeric character codes. These should be coercable to integer
#' @param character_names Optional. If provided this should be a vector of strings. This vector must be the same length as character_codes. If not provided the character names will be manufactured from the character codes. 
#' @return COYUs9CharacterKey object
#'
#' @export
COYU_character_key <- function(character_codes,character_names) {
  if (!is.numeric(character_codes)) {
    stop("character_codes must be a numeric vector")
  }
  
  if (missing(character_names)) {
    character_names <- sprintf("CHARACTER %02d",character_codes)
  }

  if (length(character_codes) != length(character_names)) {
    stop("character_codes and character_names are of unequal length")
  }
  
  ret<-data.frame(CCode=as.integer(character_codes), CName=character_names, stringsAsFactors=FALSE)
  class(ret)<-c("COYUs9CharacterKey","data.frame")
  return(ret)
}

#' get_character_name
#'
#' Provide a string character name using a character key
#'
#' @param character_key COYUs9CharacterKey object
#' @param character_code Numeric character code
#' @return the name of character_code looked up in character_key, or a vector of character(0) if character_code is not named in character_key
#'
#' @export
get_character_name<-function(character_key,character_code) UseMethod("get_character_name")

#' @export
get_character_name.COYUs9CharacterKey<-function(character_key,character_code) {
  name <- character_key[which(character_key[,"CCode"]==character_code),"CName"]
  return(name)
}

