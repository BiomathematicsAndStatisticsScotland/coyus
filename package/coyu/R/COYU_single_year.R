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



## Required to suppress a stray WARNING
globalVariables(c("AFP"))

#' COYU_single_year
#'
#' Compute COYU values for a single year of single-character data and return results as a dataframe
#'
#' @param yr.i Number of year in trial
#' @param dat.ref Reference variety data for a single character
#' @param dat.cand Candidate variety for a single  character
#' @return named list with the following format:
#'
#' grand_mean gives the grand mean of all reference varieties (single
#'       value)
#' 
#' sum_sqrs gives the error sum of squares (within year) for the
#'       spline method (single value)
#' 
#' eff_df_spline gives the effective degrees of freedom for the spline
#'       method (incl intercept) candidates based on the linear
#'       regression method (single value)
#'
#' ref_variety_spline gives the spline fitted to reference variety
#'       data used for uniformity predictions and plotting. We may in
#'       the future provide methods to interact directly with the
#'       spline a la lm() and friends
#' 
#' cand_results contains the candidate results dataframe (dataframe,
#'       as dat.cand with extra columns in AFP order)
#' 
#' ref_results contains the reference results dataframe (dataframe, as
#'       dat.ref with extra columns in AFP order)
#'
#' @seealso COYU_plot_results
#' @import MASS
#' @import splines
#' @importFrom stats smooth.spline predict 
COYU_single_year<-function(yr.i, dat.ref, dat.cand){
    ## TODO: the nature of this function means that there is a lot of
    ## recalculation (e.g. grand mean and so on) Work out what does not
    ## change across runs and calculate that elsewhere    
    yr<-levels(dat.ref$year)
    yearly_cand_data<-dat.cand[dat.cand$year==yr[yr.i],]
    
    yearly_ref_data_withmissing<-dat.ref[dat.ref$year==yr[yr.i],]
    
    yearly_ref_data_nomissing<-yearly_ref_data_withmissing[!(
        is.na(yearly_ref_data_withmissing$logSD)|is.na(yearly_ref_data_withmissing$mn)),]
    
    ## Possible fix for issue identified by Haidee on 2021-12-20 where
    ## some varieties have means, or SDs but not both    
    ## yearly_ref_data_nomissing<-yearly_ref_data_nomissing[!(is.na(yearly_ref_data_nomissing$logSD) | is.na(yearly_ref_data_nomissing$mn)),]
    
    grand_mean<-mean(yearly_ref_data_nomissing$logSD) 
    
    ## reordering to help spline work - does this return results in the right order for use later?
    order_ref_by_mean<-order(yearly_ref_data_nomissing$mn)
    yearly_ref_data_nomissing<-yearly_ref_data_nomissing[order_ref_by_mean,]
    order_cand_by_mean<-order(yearly_cand_data$mn)
    yearly_cand_data<-yearly_cand_data[order_cand_by_mean,]
    
    ## fit natural spline with fixed df using smooth.spline
    ref_variety_spline <- smooth.spline(yearly_ref_data_nomissing$mn,
                                        yearly_ref_data_nomissing$logSD,
                                        all.knots = TRUE,
                                        df = 4)
    Y.fit<-predict(ref_variety_spline,
                   yearly_ref_data_nomissing$mn)$y 
    Ynew.fit<-predict(ref_variety_spline,
                      yearly_cand_data$mn)$y
    cand_adj_logSD<-grand_mean+yearly_cand_data$logSD-Ynew.fit
    names(cand_adj_logSD) = yearly_cand_data$AFP
    
    ## Fix for data containing missing values for means - 21.04.15
    Y.fit.full<-numeric(length=length(yearly_ref_data_withmissing$mn)) 
    Y.fit.full[!is.na(yearly_ref_data_withmissing$mn)]<-predict(ref_variety_spline,
                                                   yearly_ref_data_withmissing[!is.na(yearly_ref_data_withmissing$mn),]$mn)$y 
    is.na(Y.fit.full)<-which(is.na(yearly_ref_data_withmissing$mn)) 
    
    ref_adj_logSD<-grand_mean+yearly_ref_data_withmissing$logSD-Y.fit.full 
    names(ref_adj_logSD) = yearly_ref_data_withmissing$AFP
    
    ## next bit get stuff for working out SEs -
    ## TODO: - separate function for this? Seems unrelated to most other stuff in COYU_single_year
    eff_df_spline<-ref_variety_spline$df
    spar1<-ref_variety_spline$spar
    knots <- ref_variety_spline$fit$knot * ref_variety_spline$fit$range + ref_variety_spline$fit$min
    k1<-unique(knots)
    k1<-k1[2:(length(k1)-1)] 
    design.out <- ns(x=yearly_ref_data_nomissing$mn,
                     knots=k1,
                     intercept=TRUE,
                     Boundary.knots=c(min(yearly_ref_data_nomissing$mn),max(yearly_ref_data_nomissing$mn))
                     )
    N<-matrix(design.out,
              nrow=attributes(design.out)$dim[1],
              ncol=attributes(design.out)$dim[2])
    design.out <- ns(x=yearly_cand_data$mn,
                     knots=k1,
                     intercept=TRUE,
                     Boundary.knots=c(min(yearly_ref_data_nomissing$mn),max(yearly_ref_data_nomissing$mn))
                     )
    
    Nn<-matrix(design.out,
               nrow=attributes(design.out)$dim[1],
               ncol=attributes(design.out)$dim[2])
    
    S <-  smooth_matrix(yearly_ref_data_nomissing$mn, spar = spar1)
    res.df<-length(yearly_ref_data_nomissing$logSD)-eff_df_spline # checked against mgcv (look at splinevariancefn_comparison_AR.R)
    sum_sqrs<-sum((yearly_ref_data_nomissing$logSD-Y.fit)^2)
    mse<-sum_sqrs/res.df
    Tred<-t(ginv(t(N))%*% t(Nn) ) 
    TT<- Tred%*% S %*% t(Tred)
    wahba_regression_factor  <- diag(TT) #for candidates
    
    Tredr<-t(ginv(t(N))%*% t(N) ) # added 26/04/18
    TTr<- Tredr%*% S %*% t(Tredr)  #added 26/04/18
    wahba_regression_factor_ref  <- diag(TTr) #for reference vars # added 26/04/18
    
    min_factor_ref<- wahba_regression_factor_ref[yearly_ref_data_nomissing$mn==min(yearly_ref_data_nomissing$mn)][1] #added 26/04/18
    max_factor_ref<- wahba_regression_factor_ref[yearly_ref_data_nomissing$mn==max(yearly_ref_data_nomissing$mn)][1] #added 26/04/18
    

    extrap_factor<-sqrt((wahba_regression_factor+1)/(min_factor_ref+1)*(yearly_cand_data$mn<min(yearly_ref_data_nomissing$mn)) +
                      (wahba_regression_factor+1)/(max_factor_ref+1)*(yearly_cand_data$mn>max(yearly_ref_data_nomissing$mn))) #added 26/04/18
    is.na(extrap_factor)<-which(extrap_factor==0) #added 26/04/18

    candidate_results <- cbind(yearly_cand_data[c("year","variety","AFP","mn","logSD")],
                               adjusted_logSD=cand_adj_logSD,
                               regression_factor=wahba_regression_factor,
                               extrapolation_factor=extrap_factor) #modified 26/04/18
    
    reference_results <- cbind(yearly_ref_data_withmissing[c("year","variety","AFP","mn","logSD")],
                               adjusted_logSD=ref_adj_logSD) 
    
    ## Return results with appropriate class
    ## N.B bareword "AFP" causes a warning; this is suppressed by globalVariables(AFP) above
    single_year_results = list(grand_mean=grand_mean,                   
                               sum_sqrs=sum_sqrs,                       
                               eff_df_spline=eff_df_spline,
                               ref_variety_spline=ref_variety_spline,
                               cand_results=candidate_results[with(candidate_results,order(AFP)),], 
                               ref_results=reference_results[with(reference_results,order(AFP)),])

    class(single_year_results) = c("COYUs9SingleYearResult", "list")
    return (single_year_results)
}

