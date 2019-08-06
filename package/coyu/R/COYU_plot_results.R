
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

#' COYU_plot_results
#'
#' Produce diagnostic plots, either to file or current graphics device
#' 
#' @param results COYUs9Results object containing the data to plot
#' @param character_key Mapping of character number to name
#' @param plot_options Control plot options. For future use.
#' @param plot_file File name to plot to. If NULL, current graphics device is used. 
#'
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics plot lines points plot.new title par
COYU_plot_results <- function(results,character_key,plot_options=1,plot_file=NULL) UseMethod("COYU_plot_results")


#' @export
COYU_plot_results.COYUs9AllResults<-function(results,character_key,plot_options=1,plot_file=NULL) {
    
  if (!is.null(plot_file)) {
    pdf(file=plot_file,onefile=TRUE)
  }

  
  if (is_3_year(results)) {
    plottable<-"3_year_reject"
    #3 plots, in 2x2 square
    plot_rows=c(2,2)
  } else {
    plottable<-"2_year_reject"
    #2 plots, portrait format
    plot_rows=c(2,1)
  }
  
  #Set our rows for the number of plots we are doing and allocate space for outer top margin
  par(mfrow=plot_rows,oma=c(0,0,2,0))

  #TODO: could have single character plot function to use here
  plot_results<-sapply(results[plottable,],function(char_result) {
    character_name<-character_key[which(character_key[,"CCode"]==char_result$character_number),"CName"]
    COYU_plot_single_character(char_result,character_name)
  })
  
  dev.off()
}

#' COYU_plot_single_character
#'
#' Plot a single results object to the current graphics device
#'
#' @param char_result COYUs9Results object to plot
#' @param character_name Optional name for the plot title
#' 
#'@export
COYU_plot_single_character <- function(char_result,character_name) UseMethod("COYU_plot_single_character")

#'@export
COYU_plot_single_character.COYUs9Results <- function(char_result,character_name) {

    if (missing(character_name)) {
      character_name <- sprintf("%d",char_result$character_number)
    }
    
    #Find maxima and minima across x and y scales for each character, ignoring missing values
    minmax_x<-sapply(char_result$mean_sd_data,function(year_result) {
      return(c(min(c(year_result$ref_mean,year_result$cand_mean), na.rm=TRUE),
               max(c(year_result$ref_mean,year_result$cand_mean), na.rm=TRUE)))
    })

    minmax_y<-sapply(char_result$mean_sd_data,function(year_result) {
      return(c(min(c(year_result$ref_logsd,year_result$cand_logsd), na.rm=TRUE),
               max(c(year_result$ref_logsd,year_result$cand_logsd), na.rm=TRUE)))      
    })
    
    #Add a bit of extra space to each limit
    character_xlim=c(min(minmax_x)*0.95,max(minmax_x)*1.05)
    character_ylim=c(min(minmax_y)*0.95,max(minmax_y)*1.05)    
    
    year_plot_result<-sapply(char_result$mean_sd_data,function(year_result) {
      #Filter out any missing values
      filtered_sd=year_result$ref_logsd[!is.na(year_result$ref_logsd)]
      filtered_mean=year_result$ref_mean[names(filtered_sd)]
      
      plot(filtered_mean,
           filtered_sd,
           xlim=character_xlim,
           ylim=character_ylim,
           xlab="Mean",
           ylab="log (SD+1)",
           main=paste("Year ",year_result$year),
           pch=4)
      lines(year_result$x_line,
            year_result$y_line)
      points(year_result$cand_mean, year_result$cand_logsd,pch="c",col="red") # new
    })

    #Force a page break in 3 year mode
    #TODO: obey plot_options variable here - if plot_options==2, format plots 1 per page
    if (is_3_year(char_result)) {
      plot.new()
    }
    
    title(sprintf("Character '%s'  (%d)",gsub("^\\s+|\\s+$", "", character_name),char_result$character_number), outer=TRUE)
}
