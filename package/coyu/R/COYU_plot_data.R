#' COYU_yearly_plot_data
#'
#' Generate data for plotting each year using the reference variety
#' spline and the reference and candidate data.
#'
#' @seealso COYU_plot_results calls this method
#'
#' @param single_year_result An object of class COYUs9SingleYearResult
#' 
#' @return a named list of class COYUs9SingleYearPlotData containing
#'     mean and SD data, fit lines. Format as follows:
#' 
#'       year          Name of the year
#'       x_line        Diagnostic plot X values (mean, predicted) 
#'       y_line        Diagnostic plot Y values (sd, predicted)
#'       ref_mean      Reference means for this year, in AFP order 
#'       ref_logsd     Reference SD values for this year, in AFP order
#'       ref_adjlogsd  Adjusted reference SD values for this year, in AFP order
#'       cand_mean     Candidate mean values for this year, in AFP order
#'       cand_logsd    Candidate SD values for this year, in AFP order
#'       cand_adjlogsd Adjusted reference SD values for this year, in AFP order
#' @export
COYU_yearly_plot_data<-function(single_year_result) UseMethod("COYU_yearly_plot_data")

COYU_yearly_plot_data.COYUs9SingleYearResult<-function(single_year_result) {    
    ref_mean <- single_year_result$ref_results$mn
    ref_sd <- single_year_result$ref_results$logSD
    cand_mean <- single_year_result$cand_results$mn
    cand_sd <- single_year_result$cand_results$logSD
    
    names(ref_mean)<-single_year_result$ref_results$AFP
    names(ref_sd)<-single_year_result$ref_results$AFP
    names(cand_mean)<-single_year_result$cand_results$AFP
    names(cand_sd)<-single_year_result$cand_results$AFP

    ref_mean_plot_x<-seq(min(ref_mean),
                         max(ref_mean),
                         length.out=1000)

    ## Should only be one year value in this object - this is just a
    ## convenient way of getting it
    year = as.character(single_year_result$ref_results$year[1])
    
    mean_sd_plot_data = list(
        year=year,
        x_line=ref_mean_plot_x,
        y_line=predict(single_year_result$ref_variety_spline, ref_mean_plot_x)$y,
        ref_mean=ref_mean,
        ref_logsd=ref_sd,
        ref_adjlogsd=single_year_result$ref_results$adjusted_logSD,
        cand_mean=cand_mean,
        cand_logsd=cand_sd,
        cand_adjlogsd=single_year_result$cand_results$adjusted_logSD
    )

    class(mean_sd_plot_data) = c("COYUs9SingleYearPlotData", "list")
    return (mean_sd_plot_data)
}
