
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
#' Produce diagnostic plots, either to file or current graphics device. The plots show the uniformity levels (log(SD+1))
#' for candidates (C in red) and reference varieties (X) plotted against the expression level (mean). Lines represent the spline curve fitted.  
#' There are plots for each year and character. The plots can be used to assist decisions when there is extrapolation 
#' (i.e. candidate outside range of expression of reference varieties).
#' 
#' @param results COYUs9Results object containing the data to plot
#' @param character_key Mapping of character number to name. This can be the character_key in the COYUs9TrialData object and can be subsetted as required
#' @param plot_options Control plot options. If plot_options==2, a new page/device will be started for each plot. Other values will lead to the default behaviour. 
#' @param plot_file File name to plot to. If NULL, current graphics device is used.
#' @param candidates The candidate varieties to plot, by AFP number. If value is "ALL" (default), all candidates are plotted. Any candidates not included here will be omitted from the plots 
#' @examples
#' ## an example using the test_2_year example included in the COYU package
#' 
#' data(test_2_year,package="coyu") 
#' 
#' results1<-COYU_all_results(test_2_year$trial_data,
#'                            test_2_year$coyu_parameters,
#'                            test_2_year$probability_sets)[[1]]
#' ## note [[1]] selects the results for the first probability set
#' 
#' COYU_print_results(results1,
#'                    test_2_year$coyu_parameters,
#'                    test_2_year$character_key,
#'                    test_2_year$probability_set[1,])
#' ## note test_2_year$probability_set[1,] gives the probabilities for this set
#' 
#' write.csv(COYU_results_as_dataframe(results1, "2_year_reject"),
#'           "tester.csv")
#' 
#' COYU_plot_results(results1,
#'                   character_key = test_2_year$character_key,
#'                   plot_file="MyPlots.pdf")
#' ## results sent to a pdf file. 
#'
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics plot lines points plot.new title par
COYU_plot_results <- function(results,
                              character_key,
                              plot_options=1,
                              plot_file=NULL,
                              candidates="ALL") UseMethod("COYU_plot_results")


#' @export
COYU_plot_results.COYUs9AllResults<-function(results,
                                             character_key,
                                             plot_options=1,
                                             plot_file=NULL,
                                             candidates="ALL") {
  
  if (!is.null(plot_file)) {
    pdf(file=plot_file,onefile=TRUE)
  }
  
  
  if (is_3_year(results)) {
    plottable<-"3_year_reject"
    ## 3 plots, in 2x2 square
    plot_rows=c(2,2)
  } else {
    plottable<-"2_year_reject"
    ## 2 plots, portrait format
    plot_rows=c(2,1)
  }
  
  ## Set our rows for the number of plots we are doing and allocate space for outer top margin
  par(mfrow=plot_rows,oma=c(0,0,2,0))
  
  plot_results<-sapply(results[plottable,],function(char_result) {
    character_name<-character_key[which(character_key[,"CCode"]==char_result$character_number),"CName"]
    COYU_plot_single_character(char_result, character_name, candidates, plot_options)
  })
  
  dev.off()
}

#' COYU_plot_single_character
#'
#' Plot a single results object to the current graphics device
#'
#' @param char_result COYUs9Results object to plot
#' @param character_name Optional name for the plot title
#' @param candidates The candidate varieties to plot, by AFP number. If value is "ALL" (default), all candidates are plotted. Any candidates not included here will be omitted from the plots 
#' @param plot_options Controls plotting. If plot_options==2, a new page will be started for each plot
#'@export
COYU_plot_single_character <- function(char_result,
                                       character_name,
                                       candidates="ALL",
                                       plot_options=1) UseMethod("COYU_plot_single_character")

#'@export
COYU_plot_single_character.COYUs9Results <- function(char_result,
                                                     character_name,
                                                     candidates="ALL",
                                                     plot_options=1) {
  
  if (missing(character_name)) {
    character_name <- sprintf("%d",char_result$character_number)
  }

  ##Check our specified candidates are valid
  valid_candidates = as.numeric(as.character(char_result$candidates$candidate_afp))
  if (candidates=="ALL") {
      active_candidates = valid_candidates
  } else if (!is.numeric(candidates)) {         
      stop(sprintf("Argument 'candidates' must be \"ALL\" or numeric. Value is %s",
                   paste(candidates, collapse=",")))
      
  } else if (! candidates %in% valid_candidates) {
      stop(sprintf("Argument candidates must all be present in the results data. Value is %s, valid candidates are %s",
                   paste(candidates, collapse=","),
                   paste(valid_candidates, collapse=",")))
  } else {
      active_candidates = candidates
  }

  year_plot_data = lapply(char_result$yearly_results,COYU_yearly_plot_data)
    
  ## Find maxima and minima across x and y scales for each character, ignoring missing values
  minmax_x<-sapply(year_plot_data,function(year_result) {
    return(c(min(c(year_result$ref_mean,
                   year_result$cand_mean[ names(year_result$cand_mean)==active_candidates ]),
                 na.rm=TRUE),
             max(c(year_result$ref_mean,
                   year_result$cand_mean[ names(year_result$cand_mean)==active_candidates ]),
                 na.rm=TRUE)))
  })
  
  minmax_y<-sapply(year_plot_data,function(year_result) {
    return(c(min(c(year_result$ref_logsd,
                   year_result$cand_logsd[ names(year_result$cand_logsd)==active_candidates ]),
                 na.rm=TRUE),
             max(c(year_result$ref_logsd,
                   year_result$cand_logsd[ names(year_result$cand_logsd)==active_candidates ]),
                 na.rm=TRUE)))
  })
  
  ## Add a bit of extra space to each limit
  character_xlim=c(min(minmax_x)*0.95,max(minmax_x)*1.05)
  character_ylim=c(min(minmax_y)*0.95,max(minmax_y)*1.05)    
  
  year_plot_result<-sapply(year_plot_data, function(year_result) {
    ## Filter out any missing values
    filtered_sd=year_result$ref_logsd[ !is.na(year_result$ref_logsd) ]
    filtered_mean=year_result$ref_mean[ names(filtered_sd) ]

    ## Plot reference values as "X"
    plot(filtered_mean,
         filtered_sd,
         xlim=character_xlim,
         ylim=character_ylim,
         xlab="Mean",
         ylab="log (SD+1)",
         main=paste("Year ",year_result$year),
         pch=4)

    ## Calculate the "extension lines" for our fit line by taking the
    ## last N points at each end and fitting a straight line to them
    ##
    ## There are other things we could do here, e.g. provide an
    ## extend_line() functional argument to this function that would
    ## allow user-controlled line extensions (and provide a default
    ## implementation)
    ##
    ## See also comments in COYU_single_year about what we might
    ## like to return to the end user of this code
    points_to_fit = 100
    data_below = data.frame(x_line= head(year_result$x_line, points_to_fit),
                            y_line= head(year_result$y_line, points_to_fit))
    data_above = data.frame(x_line= tail(year_result$x_line, points_to_fit),
                            y_line= tail(year_result$y_line, points_to_fit))

    lm_below = lm(y_line ~ x_line, data=data_below)
    lm_above = lm(y_line ~ x_line, data=data_above)    
    
    ## Calculate the extension line with y=mx + b from our linear model
    slope_func = function(x_point, model) {
        intercept = model$coefficients[1]
        slope = model$coefficients[2]
        return (slope * x_point + intercept)
    }

    x_points_below = seq(character_xlim[1], min(data_below$x_line), length.out=20)
    y_points_below = sapply( x_points_below, slope_func, lm_below )

    x_points_above = seq(max(data_above$x_line), character_xlim[2], length.out=20)
    y_points_above = sapply( x_points_above, slope_func, lm_above )
      
    lines(x=x_points_below,
          y=y_points_below,
          lty=2,
          col="blue")

    lines(x=x_points_above,
          y=y_points_above,
          lty=2,
          col="blue")
      
    ## Plot the actual fit spline returned from COYU_single_year
    lines(year_result$x_line,
          year_result$y_line,
          col="blue")

    ## Plot candidates as "C"
    active_cand_mean = year_result$cand_mean[ names(year_result$cand_mean)==active_candidates ]
    active_cand_logsd = year_result$cand_logsd[ names(year_result$cand_logsd)==active_candidates ]
      
    points(active_cand_mean, active_cand_logsd, pch="c", col="red")


    ## Plot candidate labels
    text(active_cand_mean,
         active_cand_logsd,
         adj=c(-0.5,1.2),
         labels=names(active_cand_logsd),
         cex=0.5,
         col="red")
    
  })
  
  ## Force a page break in 3 year, and if plot_options==2
  if (is_3_year(char_result) || plot_options==2) {
    plot.new()
  }
  
  title(sprintf("Character '%s'  (%d)",
                gsub("^\\s+|\\s+$", "", character_name),
                char_result$character_number),
        outer=TRUE)
}
