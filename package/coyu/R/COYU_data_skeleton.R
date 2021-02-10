#' COYU_data_skeleton
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
#' Creates a "skeleton" data frame in the COYU data format, optionally 
#' adding measurement data as well.
#'
#' @param years Vector of 4-digit year values. The vector length is the number of years.
#' @param characters Vector of numeric character identities. These may be numbers of any length but the printed output will only allow characters of up to 4 digits in length. The vector length is the number of years.
#' @param variety_afp Vector of numeric variety AFP numbers. These are unique numerical identifiers for the varieties. The vector length is the number of varieties.
#' @param variety_name Optional. If not provided variety names will be created from variety_afp. The vector length is the number of varieties.
#' @param mean_data Optional. Matrix of mean data with row by col dimensions as specified below. If not provided, NA will be substituted for the plot mean data
#' @param stddev_data Optional. Matrix of standard deviation data with row by col dimensions as specified below. If not provided NA will be substituted for the stddev data
#' @return Data frame with the correct structure for use with the COYU routines. The data class is COYUs9TrialData
#'
#' Rows = length(years) * length(variety_afp)
#' Cols = length(characters)
#'
#' Thus a trial with the following attributes:
#'
#' years=c(2011,2012)
#' characters=c(1,2,3,4,5,6)
#' varieties=c(900,901,902,903,904,905,906,907,908,909)
#'
#' would have dimensions as follows:
#'
#' Rows= 20
#' Cols= 6
#'
#'
#' The mean and standard deviations matrices should contain data for each combination of year,
#' variety and character in the order specified in the other arguments
#' to this function. The numeric values need not be in their natural
#' order, for example characters=c(1,9,4,10,51,25)
#'
#'
#' Thus with the attributes above, the first row of mean_data would contain the plot_mean
#' measures for characters 1-6 for variety 900 in year 2011. The 2nd row would contain
#' the measures for variety 901 in 2011 and so on.
#' 
#' Missing data is allowed for reference/control varieties but candidate varieties are expected to have data in all years
#' 
#' @seealso COYU_all_results COYU_sanity_check COYU_probability_set
#' @examples 
#' years=c(2011,2012)
#' characters=c(1,2,3)
#' varieties=c(10,11,12)
#' COYU_data_skeleton(years, characters, varieties)
#' fake_mean_data<-rbind(matrix(3,3,3), matrix(8,3,3))
#' fake_stddev_data<-rbind(matrix(0.1,3,3), matrix(0.8,3,3))
#' trial_data<-COYU_data_skeleton(years, characters, varieties, mean_data = fake_mean_data, stddev_data = fake_stddev_data)
#' COYU_parameters_from_df(trial_data,c(10))
#' y=COYU_probability_set(reject_3_year = c(0.05,0.02,0.01), reject_2_year = c(0.05,0.02,0.01), accept_2_year = c(0.1,0.05,0.02))
#' COYU_sanity_check(trial_data = trial_data, coyu_parameters = COYU_parameters_from_df(trial_data,c(10)))
#' @export
COYU_data_skeleton <- function(years,characters,variety_afp,variety_name,mean_data,stddev_data) {
  missing_args = c(years=missing(years), characters=missing(characters),variety_afp=missing(variety_afp))
  
  if (any(missing_args)) {
    stop(sprintf("Required parameters missing: %s",
                 paste(names(missing_args[missing_args==TRUE]),
                       collapse=",")))
  }

  characters<-unique(characters)
  variety_afp<-unique(variety_afp)
  
  if (missing(variety_name)) {
    variety_name <- paste("AFP_",variety_afp,sep="")
  }

  if (length(variety_afp) != length(variety_name)) {
    stop("Variety name and variety AFP variables are different lengths")
  }

  if (!is.numeric(variety_afp)) {
    stop("'variety_afp' argument should be numeric")
  }

  if (!is.numeric(characters)) {
    stop("'characters' argument should be numeric")
  }
  
  mean_names <- name_mean(characters)
  stddev_names <- name_stddev(characters)
  
  check_dims<-function(target,nrow,ncol) {
    dims<-dim(target)

    return(length(dims)==2 && dims[1]==nrow && dims[2]==ncol)
  }
  
  target_nrow= length(variety_afp)*length(years)
  target_ncol= length(characters)
  
  if (!missing(mean_data)) {
    if(!check_dims(mean_data, target_nrow, target_ncol )) {
      stop("mean_data is not ",target_nrow,"x",target_ncol," array or matrix")
    }
  } else {
    mean_data <- matrix(NA,nrow=target_nrow,ncol=target_ncol)
  }
    

  if (!missing(stddev_data)) {
    if(!check_dims(stddev_data, target_nrow, target_ncol )) {
      stop("stddev_data is not ",target_nrow,"x",target_ncol," array or matrix")
    }
  } else {
    stddev_data <- matrix(NA,nrow=target_nrow, ncol=target_ncol)    
  }

  colnames(mean_data)<-mean_names
  colnames(stddev_data)<-stddev_names
  
  skel<-cbind(do.call("rbind",
                      sapply(years,function(year) {
                        data.frame(year=rep(year,length(variety_afp)),
                                   AFP=variety_afp,
                                   variety=variety_name)
                      },
                             simplify=FALSE)
                      ),
              mean_data,
              stddev_data)
              
  skel$year=as.factor(skel$year)
  skel$AFP=as.factor(skel$AFP)
  class(skel)<-c("COYUs9TrialData", "data.frame")
  return(skel)
}

