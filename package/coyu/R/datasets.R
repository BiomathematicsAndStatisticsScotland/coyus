#' 2 year trial, no missing data
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
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 18 reference varieties
#' \item 2 candidate varieties
#' \item 17 characters
#' \item 2 years of trial data
#' }
#'
#' There are no missing values
#'
#' @docType data
#' @keywords datasets
#' @name test_2_year
#' @usage data(test_2_year)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 2 year trial, no missing data but some bad values
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 33 reference varieties
#' \item 3 candidate varieties
#' \item 24 characters
#' \item 2 years of trial data
#' }
#'
#' There are no missing values but character 25 is bad. This dataset is used for testing validation code.
#'
#' @docType data
#' @keywords datasets
#' @name test_2_year_dirty
#' @usage data(test_2_year_dirty)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 2 year trial with missing data
#'
#' This is an example dataset containing data as follows:
#' \itemize{
#' \item 61 reference varieties
#' \item 2 candidate varieties
#' \item 14 characters
#' \item 2 years of trial data
#' }
#'
#' There are missing values
#'
#' @docType data
#' @keywords datasets
#' @name test_2_year_withmissing
#' @usage data(test_2_year_withmissing)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 3 year trial with missing data
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 87 reference varieties
#' \item 6 candidate varieties
#' \item 35 characters
#' \item 3 years of trial data
#' }
#'
#' There are missing values
#'
#' @docType data
#' @keywords datasets
#' @name test_3_year_withmissing
#' @usage data(test_3_year_withmissing)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 3 year trial exhibiting cyclic planting
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 74 reference varieties
#' \item 16 candidate varieties
#' \item 30 characters
#' \item 3 years of trial data
#' }
#'
#' There are missing values. The dataset is used to test that variety ordering doesn't cause problesm
#'
#' @docType data
#' @keywords datasets
#' @name test_cyclic_planting
#' @usage data(test_cyclic_planting)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 2 year trial with missing values - contributed data
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 61 reference varieties
#' \item 2 candidate varieties
#' \item 14 characters
#' \item 2 years of trial data
#' }
#'
#' There are missing values. 
#'
#' @docType data
#' @keywords datasets
#' @name test_missing_values_1
#' @usage data(test_missing_values_1)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 3 year trial with missing values - contributed data
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 87 reference varieties
#' \item 6 candidate varieties
#' \item 35 characters
#' \item 3 years of trial data
#' }
#'
#' There are missing values. 
#'
#' @docType data
#' @keywords datasets
#' @name test_missing_values_2
#' @usage data(test_missing_values_2)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 3 year trial with unbalanced data - contributed
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 63 reference varieties
#' \item 11 candidate varieties
#' \item 34 characters
#' \item 3 years of trial data
#' }
#'
#' There are missing values. 
#'
#' @docType data
#' @keywords datasets
#' @name test_unbalanced_1
#' @usage data(test_unbalanced_1)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL

#' 2 year trial with unbalanced data and missing means - contributed
#'
#' This is an example dataset containing data as follows:
#'
#' \itemize{
#' \item 444 reference varieties
#' \item 64 candidate varieties
#' \item 12 characters
#' \item 2 years of trial data
#' }
#'
#' There are missing values. This dataset is quite large so is useful
#' for testing optimisation of the stats routines
#'
#' @docType data
#' @keywords datasets
#' @name test_missing_means
#' @usage data(test_missing_means)
#' @seealso COYU_dataset_format
#' @format A named list with names \code{trial_data, coyu_parameters, probability_sets} and \code{character_key}
NULL


#' COYU Dataset Format
#'
#' The example datasets each contain a name \code{trial_data} which consists of a \code{COYUs9TrialData} object. This has the following format:
#'
#' \itemize{
#' \item A data frame with the following columns:
#' \itemize{
#' \item{year} Contains a numeric 4-digit year. Factor
#' \item{AFP} Contains numeric variety identifiers, known in the UK as "AFP numbers". Factor
#' \item{variety} Contains a textual variety name, for display purposes only. May be factor or character
#' \item{UPNN} One column per character. UP prefix followed by numeric identifier of character. Containing mean values
#' \item{sUPNN} One column per character. sUP prefix by numeric identifier of character. Containing Standard Deviations (mean over plots)
#' }
#' \item S3 class \code{COYUs9TrialData}
#' }
#'
#' This format may be constructed by the \code{\link{COYU_data_skeleton}} function
#'
#' In addition the example datasets also contain the following names:
#'
#' \itemize{
#' \item{character_key} Map of character numbers to descriptive names. Used when displaying output (e.g. \code{\link{COYU_print_results}}
#' \item{coyu_parameters} Parameter set for COYU run. Describes candidate and reference varieties. May be created from \code{trial_data} by function \code{\link{COYU_parameters_from_df}}
#' \item{probability_sets} Set of decision rules used in the COYU run. 
#' }
#'
#' @name COYU_dataset_format
NULL
