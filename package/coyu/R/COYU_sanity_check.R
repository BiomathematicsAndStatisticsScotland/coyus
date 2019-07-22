#' COYU_sanity_check
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
#' This function checks a dataset and set of COYU parameters for consistency. It is used by the function
#' \code{\link{COYU_all_results}} to check input datasets prior to computing COYU predictions
#'
#' If you do not have trial data, you can create a valid data "skeleton"
#' with the function
#' \code{\link{COYU_data_skeleton}}. You'll then need to populate the dat
#'
#' @param trial_data        Data frame containing trial data (should have class COYUs9TrialData)
#' @param coyu_parameters   Parameter set object with type COYUs9Parameters
#' @return TRUE if the trial data and parameters pass the sanity check, false otherwise
#'
#' @seealso COYU_data_skeleton
#' @export
#' @importFrom stats IQR
#' @importFrom utils combn
COYU_sanity_check<-function(trial_data,coyu_parameters) UseMethod("COYU_sanity_check")


#Constants
COYU_MIN_REFERENCE_VARIETIES <- 10
COYU_MIN_COMMON_REFERENCE_VARIETIES <- 8

#' @export
COYU_sanity_check.COYUs9TrialData<-function(trial_data,coyu_parameters) {

  if (missing(trial_data) || missing(coyu_parameters)) {
    warning("Missing arguments. Aborting check.")
    return(FALSE)
  }

  if (! "COYUs9Parameters" %in% class(coyu_parameters)) {
    warning("coyu_parameters does not have type \"COYUs9Parameters\". Aborting check")
    return(FALSE)
  }
  
  #N.B: this function is rather large and could be usefully split up
  
  success <- TRUE

  # check that basic colnames exist; if not abort immediately
  required_names <- c("year","AFP","variety")
  missing_cols <- setdiff(required_names,colnames(trial_data))
  if (length(missing_cols) > 0) {
    warning("Required columns are missing! Missing columns: ",
            paste(missing_cols,collapse=","),
            ". All COYU datasets must have these columns. Aborting check now")
    return(FALSE)
  }
  
  # check column names agree with characters
  
  expected_names <-c(name_mean(coyu_parameters$characters),name_stddev(coyu_parameters$characters)) 
  missing_measurements <- setdiff(expected_names,colnames(trial_data))
  if (length(missing_measurements) > 0) {    
    warning("The following character mean or stddev measurements are missing from the dataset and are expected by the parameter set: ",
            paste(missing_measurements,collapse=","))
    success <- FALSE            
  }
  
  if (!is.factor(trial_data$year)) {
    warning("trial_data$year is not a factor")
    success <- FALSE
  }

  if (!is.factor(trial_data$AFP)) {
    warning("trial_data$AFP is not a factor")
    success <- FALSE
  }

  if ( !(is.character(trial_data$variety) || is.factor(trial_data$variety)) ) {
    warning("trial_data$variety is not a character variable or factor")
    success <- FALSE
  }

  #Check data types in columns
  numeric_columns <- sapply(expected_names,function(x) { is.numeric(trial_data[,x]) })
  if (!all(numeric_columns)) {
    warning("Certain measurements in trial_data are not numeric and should be. Columns containing non-numeric values: ",
            paste(names(numeric_columns)[numeric_columns==FALSE],collapse=","))
    success <- FALSE                  
  }
  
  #TODO: this check may be too strict - e.g. case where analysing 4 years of data but only using 2 for uniformity
  #Easy to fix by setting up a different trial file though.
  num_trial_years <- nlevels(trial_data$year)
  
  if ( num_trial_years < 1 ||
       num_trial_years > 3) {
    warning(paste("COYUs9 requires that the number of trial years must be in range 1-3. Actual number in dataset is ",
                  num_trial_years))
    success <- FALSE
  }

  if (coyu_parameters$num_trial_years < 1 || coyu_parameters$num_trial_years > 3) {
     warning(paste("COYUs9 requires that the number of trial years must be in range 1-3. The parameters specify ",
                  coyu_parameters$num_trial_years))
    success <- FALSE
  }
  
  if (coyu_parameters$num_trial_years != num_trial_years) {
    warning("Number of trial years in parameters is different from number of trial years in dataset")
    success <- FALSE
  }

  data_cand<-get_varieties(trial_data,coyu_parameters$candidates)
  data_ref<-get_varieties(trial_data,coyu_parameters$references) 
  
  #Check number of candidates is the same using 2 different techniques.
  
  n_cand_byyear <- nrow(data_cand)/num_trial_years
  n_cand_bylevel <- length(unique(data_cand$AFP))
  
  if (n_cand_byyear != n_cand_bylevel) {
    warning("Number of candidates is not the same. Value by year=",n_cand_byyear," by level=",n_cand_bylevel)
    success <- FALSE
  }

  
  #FOREACH year ensure 10 reference varieties without missing plot mean data, negative values or anything else horrible
  for (year in levels(trial_data$year)) {
    
    check_iqr <- function(name,dataset) {
      character_iqr<-apply(as.data.frame(dataset[,c(character_means,character_stddevs)]),2,function(x) { IQR(x,na.rm=TRUE) })
      
      if (any(character_iqr==0)) {
        warning(name," measurements ",paste(names(character_iqr)[character_iqr==0],collapse=",")," in year ",year," have an interquartile range of 0. This is not allowed")
        return(FALSE)
      }
      return(TRUE)
    }
    
    candidates_year <- get_varieties(trial_data[trial_data$year==year,], variety_afp=coyu_parameters$candidates)
    reference_year <- get_varieties(trial_data[trial_data$year==year,], variety_afp=coyu_parameters$references)

    character_means <- name_mean(coyu_parameters$characters)
    character_stddevs <- name_stddev(coyu_parameters$characters)
    
    missing_candidates<-setdiff(coyu_parameters$candidates,candidates_year$AFP)
    if (length(missing_candidates) > 0 ) {
      warning("Candidate varieties ",paste(missing_candidates,collapse=",")," are not present in data for year ",year)
      success <- FALSE
    }

    if (any(is.na(candidates_year))) {
      warning("Missing data for candidates in year ",year)
      success <- FALSE
    }

    if (any(candidates_year[,c(character_means,character_stddevs)] < 0, na.rm=TRUE)) {
      warning("Negative candidate measurement values in year ",year)
      success <- FALSE
    }

    if (any(reference_year[,c(character_means,character_stddevs)] < 0, na.rm=TRUE)) {
      warning("Negative reference measurement values in year ",year)
      success <- FALSE
    }

    #Check IQR for all characters, for both candidate and reference. A value of 0 will cause subsequent code to fail so we exit
    #For single-character trials, we don't perform the IQR check as it doesn't make sense
    if (nrow(candidates_year) > 1) {
      check_cand_iqr <- check_iqr("Candidate",candidates_year)
    } else {
      check_cand_iqr <- TRUE
    }
    
    check_ref_iqr <- check_iqr("Reference",reference_year)
    
    success <- success && check_ref_iqr && check_cand_iqr
    
    valid_reference_means<- reference_year[!apply(as.data.frame(reference_year[,character_means]),1,
                                                  function(x) { any(is.na(x)) } ), ]
    valid_reference_stddevs<- reference_year[!apply(as.data.frame(reference_year[,character_stddevs]),1,
                                                    function(x) { any(is.na(x)) } ), ]
    
    if (nrow(valid_reference_means) < COYU_MIN_REFERENCE_VARIETIES) {
      warning("Only ",nrow(valid_reference_means)," reference varieties with plot mean data in year ",year,". ",COYU_MIN_REFERENCE_VARIETIES," are required")
      success <- FALSE
    }

    if (nrow(valid_reference_stddevs) < COYU_MIN_REFERENCE_VARIETIES) {
      warning("Only ",nrow(valid_reference_stddevs)," reference varieties with stddev data in year ",year,". ",COYU_MIN_REFERENCE_VARIETIES," are required")
      success <- FALSE
    }
    
  }

  if ( num_trial_years > 1) {

   #Generate pairs of years and check at least 8 reference varieties without missing data in common between pairs of years
    
    year_pair_check<-
      combn(levels(trial_data$year), 2,
            simplify=FALSE,
            FUN=function(year_pair) {
              year_1_data<-get_varieties(trial_data[trial_data$year==year_pair[1], ], variety_afp=coyu_parameters$references)
              year_2_data<-get_varieties(trial_data[trial_data$year==year_pair[2], ], variety_afp=coyu_parameters$references)
              
              valid_year_1_data<-year_1_data[!apply(year_1_data, 1, function(x) { any(is.na(x)) }),]
              valid_year_2_data<-year_2_data[!apply(year_2_data, 1, function(x) { any(is.na(x)) }),]
              
              common_varieties<-intersect(unique(valid_year_1_data$AFP),unique(valid_year_2_data$AFP))
              
              if (length(common_varieties) <  COYU_MIN_COMMON_REFERENCE_VARIETIES) {
                warning("In this dataset the year-pairing ",
                        paste(year_pair, collapse=","),
                        " has fewer than ",COYU_MIN_COMMON_REFERENCE_VARIETIES," common reference varieties between them")
                success<<-FALSE
              }
              
       
              return(length(common_varieties))    
            })
  } else {
    warning("Skipping check for 8 reference varieties in each trial year as this is a single-year trial");
  }
  
  return(success)
}

