#' @import splines
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



#Required to suppress a stray WARNING
globalVariables(c("AFP"))

#' COYU_single_year
#'
#' Compute COYU values for a single year of single-character data and return results as a dataframe
#'
#' @param yr.i Number of year in trial
#' @param dat.ref Reference variety data for a single character
#' @param dat.cand Candidate variety for a single  character
#' @return data frame with the following format:
#'
#'
#' row 1 gives the grand mean (single value)
#' row 2 gives the error sum of squares (within year) for the spline method (single value)
#' row 3 gives the effective degrees of freedom for the spline method (incl intercept) candidates
#'       based on the linear regression method (single value)
#' row 4 contains diagnostic plot data for each year. (dataframe ,in AFP order)
#' row 5 contains the candidate results dataframe (dataframe, as dat.cand with extra columns in AFP order)
#' row 6 contains the reference results dataframe (dataframe, as dat.ref with extra columns in AFP order)
#'
#' @import MASS
#' @importFrom stats smooth.spline predict 
COYU_single_year<-function(yr.i,dat.ref,dat.cand){
  
# TODO: the nature of this function means that there is a lot of
# recalculation (e.g. grand mean and so on) Work out what does not
# change across runs and calculate that elsewhere #

  yr<-levels(dat.ref$year)
  dat.cand.i<-dat.cand[dat.cand$year==yr[yr.i],]
  dat.ref.i<-dat.ref.i.full<-dat.ref[dat.ref$year==yr[yr.i],] # changed 04.09.14
  dat.ref.i<-dat.ref.i[!is.na(dat.ref.i$logSD),] #added 04.09.14
  grand_mean<-mean(dat.ref.i$logSD) 

  # reordering to help spline work - does this return results in the right order for use later?
  #TODO: Switch to using temp variables here, clearer
  order_ref_by_mean<-order(dat.ref.i$mn)
  dat.ref.i<-dat.ref.i[order_ref_by_mean,]
  order_cand_by_mean<-order(dat.cand.i$mn)
  dat.cand.i<-dat.cand.i[order_cand_by_mean,]
  
  # fit natural spline with fixed df using smooth.spline
  obj <- smooth.spline(dat.ref.i$mn,dat.ref.i$logSD, all.knots = TRUE,df = 4)
  Y.fit<-predict(obj,dat.ref.i$mn)$y 
  Ynew.fit<-predict(obj,dat.cand.i$mn)$y
  cand_adj_logSD<-grand_mean+dat.cand.i$logSD-Ynew.fit


  #Fix for data containing missing values for means - 21.04.15
  Y.fit.full<-numeric(length=length(dat.ref.i.full$mn)) 
  Y.fit.full[!is.na(dat.ref.i.full$mn)]<-predict(obj,dat.ref.i.full[!is.na(dat.ref.i.full$mn),]$mn)$y 
  is.na(Y.fit.full)<-which(is.na(dat.ref.i.full$mn)) 

  ref_adj_logSD<-grand_mean+dat.ref.i.full$logSD-Y.fit.full 
  
  # next bit get stuff for working out SEs -
  # TODO: - separate function for this? Seems unrelated to most other stuff in COYU_single_year
  eff_df_spline<-obj$df
  spar1<-obj$spar
  knots <- obj$fit$knot * obj$fit$range + obj$fit$min
  k1<-unique(knots)
  k1<-k1[2:(length(k1)-1)] 
  design.out <- ns(x=dat.ref.i$mn,
                   knots=k1,
                   intercept=TRUE,
                   Boundary.knots=c(min(dat.ref.i$mn),max(dat.ref.i$mn))
                   )
  N<-matrix(design.out,
            nrow=attributes(design.out)$dim[1],
            ncol=attributes(design.out)$dim[2])
  design.out <- ns(x=dat.cand.i$mn,
                   knots=k1,
                   intercept=TRUE,
                   Boundary.knots=c(min(dat.ref.i$mn),max(dat.ref.i$mn))
                   )
  
  Nn<-matrix(design.out,
             nrow=attributes(design.out)$dim[1],
             ncol=attributes(design.out)$dim[2])
  
  S <-  smooth_matrix(dat.ref.i$mn, spar = spar1)
  res.df<-length(dat.ref.i$logSD)-eff_df_spline # checked against mgcv (look at splinevariancefn_comparison_AR.R)
  sum_sqrs<-sum((dat.ref.i$logSD-Y.fit)^2)
  mse<-sum_sqrs/res.df
  Tred<-t(ginv(t(N))%*% t(Nn) ) 
  TT<- Tred%*% S %*% t(Tred)
  wahba_regression_factor  <- diag(TT) #for candidates
  
  Tredr<-t(ginv(t(N))%*% t(N) ) # added 26/04/18
  TTr<- Tredr%*% S %*% t(Tredr)  #added 26/04/18
  wahba_regression_factor_ref  <- diag(TTr) #for reference vars # added 26/04/18
  
  min_factor_ref<- wahba_regression_factor_ref[dat.ref.i$mn==min(dat.ref.i$mn)][1] #added 26/04/18
  max_factor_ref<- wahba_regression_factor_ref[dat.ref.i$mn==max(dat.ref.i$mn)][1] #added 26/04/18
  

  extrap_factor<-sqrt((wahba_regression_factor+1)/(min_factor_ref+1)*(dat.cand.i$mn<min(dat.ref.i$mn)) +
    (wahba_regression_factor+1)/(max_factor_ref+1)*(dat.cand.i$mn>max(dat.ref.i$mn))) #added 26/04/18
  is.na(extrap_factor)<-which(extrap_factor==0) #added 26/04/18
  
  #Store diagnostic plot data for later use
  #Scale limits are calculated where this data is plotted.
  #
  #Sort plot_data values into AFP order to match other results (sorted
  #in factor order)
  #
  #N.B the use of bareword "AFP" below generates a spurious NOTE in R
  #CMD CHECK. We suppress this by using globalVariables("AFP") at the
  #top of this file  
  ref_sorted <- dat.ref.i[with(dat.ref.i,order(AFP)),]
  cand_sorted <- dat.cand.i[with(dat.cand.i,order(AFP)),]
  
  ref_mean <- ref_sorted$mn
  ref_sd <- ref_sorted$logSD
  cand_mean <- cand_sorted$mn
  cand_sd <- cand_sorted$logSD
  
  names(ref_mean)<-ref_sorted$AFP
  names(ref_sd)<-ref_sorted$AFP
  names(cand_mean)<-cand_sorted$AFP
  names(cand_sd)<-cand_sorted$AFP
  
  Xnewplot<-seq(min(dat.ref.i$mn),
                max(dat.ref.i$mn),
                length.out=1000)
  
  sd_plot_data=list(
    year=yr[yr.i],
    x_line=Xnewplot,
    y_line=predict(obj,Xnewplot)$y,
    ref_mean=ref_mean,
    ref_logsd=ref_sd,
    cand_mean=cand_mean,
    cand_logsd=cand_sd
    )

  candidate_results <- cbind(dat.cand.i[c("year","variety","AFP","mn","logSD")],
                             adjusted_logSD=cand_adj_logSD,
                             regression_factor=wahba_regression_factor,
                             extrapolation_factor=extrap_factor) #modified 26/04/18
  
  reference_results <- cbind(dat.ref.i.full[c("year","variety","AFP","mn","logSD")],
                             adjusted_logSD=ref_adj_logSD) 
                                   
  #Return results 
  list(grand_mean=grand_mean,                   
       sum_sqrs=sum_sqrs,                       
       eff_df_spline=eff_df_spline,             
       plot_data=sd_plot_data,                  
       cand_results=candidate_results[with(candidate_results,order(AFP)),], 
       ref_results=reference_results[with(reference_results,order(AFP)),])  
}

