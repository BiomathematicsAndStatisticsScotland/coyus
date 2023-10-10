#' extra_cols_as_df
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

#' Return a data frame with columns for data from all the years in the trial.
#' Usually used to combine means and logSD values for all years into a single data frame
#'
#' @param char_results Character Results, of type COYUs9Results
#' @param trial_years Number of years in the trial. Can be derived
#'     with the get_num_trial_years(coyu_parameters) function
#' @param varieties List of variety names to extract from the
#'     character results.
#' @param field_name The field name to extract from the character
#'     result for each year of the trial
#' @param col_name_template A template in printf() format for
#'     producing the output column name. For example "Mean_%s". The %s
#'     will be replaced with the year
#' @return A data frame keyed by AFP number with all the values of
#'     field_name for each year of the trial for this variety
extra_cols_as_df<-function(char_results,
                           trial_years,
                           varieties,
                           field_name,
                           col_name_template)
    UseMethod("extra_cols_as_df")

extra_cols_as_df.COYUs9Results<-function(char_results,
                                         trial_years,
                                         varieties,
                                         field_name,
                                         col_name_template) {
    column_names= c("AFP",
                    sapply(char_results$mean_sd_data,
                           function(x) {
                               sprintf(col_name_template, x$year)
                           }))

    ret=as.data.frame(
        cbind(sort(varieties),
              matrix(sapply(char_results$mean_sd_data,
                            function(x) {
                                pad_values_by_name(sort(varieties),x[[field_name]]) }
                            ),
                     nrow=length(varieties),
                     ncol=trial_years))
    )
    
    colnames(ret)=column_names
    return(ret)
}
