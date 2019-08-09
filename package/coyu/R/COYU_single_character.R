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

#' COYU_single_character
#'
#' Run the COYU algorithm for a single character, over all years. 
#'
#' @param character_number Number of the character we're working on
#' @param dat.ref  Reference data, in form documented in COYU_data_skeleton for COYUs9TrialData
#' @param dat.cand Candidate data, in form documented in COYU_data_skeleton for COYUs9TrialData
#' @return Named list with class "COYUs9Results". Format is not yet stable so is not documented
#' 
#' @import lme4
#' @import Matrix
#' @import methods
#' @importFrom stats aggregate na.pass predict pt sigma
COYU_single_character<-function(character_number,
                                dat.ref,
                                dat.cand){
  
# Version (18.07.14) modifies to allow different reference varieties in each year
# However must be sufficient in common between years 
# and all candidates must be present in all years
# Version (26.08.13) adds extrapolation detector to the code of 29.04.13
# an extra variable is output - extrapolation
# this is binary - 1 means that the candidate has a mean value outside that of the
# the reference varieties in at least one year


# Demo R function of COYU using a cubic smoothing spline adjustement within years
# Currently works for one character at a time - this could easility be modified
# required input:
# dat.ref contains data set for reference varieties
# dat.candidate contains data set for candidate varieties

# the two data sets must contain the following variables:
# year - the year of test - can be numeric 
# logSD - the natural log SD +1 for each year by variety combination
# mn - the mean measurement for each year by variety combination
# variety - variety name (not used for calculations but spooled out)

# this function does not check the format of data 
# - so it needs to be sensible, including:
# same varieties in each year in the same order
# consistency of naming
# no missing values

# outputs data.frame that can be spooled plus a jpg file - this will overwrite any jpeg
# with the same name - made unique to date

#TODO: generate warnings when calculating logSDs etc if missing values are present.

#TODO: Move some of the data preparation work elsewhere. Could also
#pass in trial_data directly and extract candidate and reference
#varieties here rather than doing it in run_coyu
  
  #data prep. Could add some stuff from run_COYU here
  
#  dat.ref$adj.logSD<-0*dat.ref$logSD
#  dat.cand$adj.logSD<-0*dat.cand$logSD
  
  # basic info extraction
  nobs<-nrow(dat.ref)
  n.yr<-nlevels(dat.ref$year)
  yr<-levels(dat.ref$year)
  n.cand<-nrow(dat.cand)/n.yr # only works with well formatted data sets
  n.cand2<-nlevels(dat.cand$AFP) #CHANGE FOR CHECK note AFP use for variety may change?
  n.ref<-nlevels(dat.ref$AFP) #addition 8.8.14 
  reference_afp<-levels(dat.ref$AFP)# addition 8.8.14
  candidate_afp<-levels(dat.cand$AFP)# addition 8.8.14 
  
  #There is a similar check to this in COYU_sanity_check
  if (n.cand!=n.cand2) {
    stop(paste("Number of candidates is not the same between years", n.cand, n.cand2))
  }   

  #have to create extrapolation_detect this way as aggregate returns an
  #annoying nested data structure when returning multiple values  
  extrapolation_detect <- merge(dat.cand,
                                data.frame(year=levels(dat.cand$year),
                                           aggregate(mn~year,
                                                     dat.ref[!is.na(dat.ref$logSD),],
                                                     function(x) {
                                                       c(MIN=min(x), MAX=max(x)) })$mn)
                                )

  #TODO: At some point we should return this information to the end user in this by-year form
  extrapolation_detect$extrapolation <- as.numeric(extrapolation_detect$mn < extrapolation_detect$MIN |
                                                   extrapolation_detect$mn > extrapolation_detect$MAX)
    
  # send data off to spline and linear fit  
  yearly_results<-sapply(1:n.yr, COYU_single_year, dat.ref=dat.ref, dat.cand=dat.cand)
  
  # output in yearly_results is a matrix 6 sets of info x n.yr years
  # Dataframe format is documented in COYU_single_year.R
  merged_ref_results<-merge(dat.ref, do.call("rbind", yearly_results["ref_results",1:n.yr]))
  merged_cand_results<-merge(dat.cand, do.call("rbind", yearly_results["cand_results",1:n.yr]))
  
  ## Note: problems with visibility of dat.ref in lmer() call, hence
  ## pulled out into separate variables

  ## CHANGE need to check function is stable with likely data.
  ## 2nd Change: add control function to remove warning when VC is 0
  vcanal<-lmer(adjusted_logSD~year+(1|AFP),
               REML=TRUE,
               data=merged_ref_results,
               control=lmerControl(
                   check.conv.singular =.makeCC(action = "ignore",
                                                tol = 1e-4))
               ) 
  vcanal.vc<-as.numeric(VarCorr(vcanal)[1]) #CHANGE
  vcanal.sig2<-(sigma(vcanal))^2 #CHANGE
  

  # calculate dfs, MSEs and SEs
  eff.df.spline<-as.numeric(as.data.frame((yearly_results["eff_df_spline", 1:n.yr])))
  res.df.spline<- nobs -sum(eff.df.spline)
  resid.var.spline<-(vcanal.vc+vcanal.sig2)*(nobs-n.yr)/res.df.spline #CHANGE   
  mn.adj.logSD.ref.spl<-sum(c(n.yr,rep(1,(n.yr-1)))*fixef(vcanal))/n.yr # CHANGE 

  # now to work out means for candidates - for now only using AFP as
  #aggregation variable as variety names are not consistent across
  #years. Could make them consistent if desired

  ##OLD VERSION
  ##cand_means <- aggregate(cbind(mn,logSD,adjusted_logSD,regression_factor,extrapolation_factor)~AFP, merged_cand_results, mean, na.action=na.pass)

  ##NEW VERSION  
  ##Extract candidate means and max extrapolation factors for each candicate
  cand_means1 = aggregate(cbind(mn,logSD,adjusted_logSD,regression_factor)~AFP,
                         merged_cand_results, mean, na.action=na.pass)
    
  merged_cand_resultsnoNA = merged_cand_results
  merged_cand_resultsnoNA$extrapolation_factor[
       is.na(merged_cand_resultsnoNA$extrapolation_factor)]=0
    
  cand_means2 = aggregate(extrapolation_factor~AFP,
                         merged_cand_resultsnoNA, max)
  cand_means = merge(cand_means1, cand_means2,all.x=T)
  is.na(cand_means$extrapolation_factor) = which(cand_means$extrapolation_factor==0)
  ##END NEW VERSION
    
  cand_means$candidate_varieties=dat.cand$variety[match(cand_means$AFP,dat.cand$AFP)]
  cand_means$SE_cand_one_spl_Wah = sqrt(resid.var.spline*(1+ cand_means$regression_factor)/n.yr)
  cand_means$pval_cand_spl = 1-pt(
    (cand_means$adjusted_logSD-mn.adj.logSD.ref.spl)/cand_means$SE_cand_one_spl_Wah,
    df=res.df.spline)
  
  anova_data<-as.data.frame(rbind(as.numeric(yearly_results["sum_sqrs", 1:n.yr]),
                                  as.numeric((yearly_results["eff_df_spline", 1:n.yr]))),
                                  row.names=c("Sum of squares", "Effective degrees of freedom"))
  colnames(anova_data)<-yr   

  #Dealing with unbalanced data and missing values
  ind1<-which(with(merged_ref_results, aggregate(!is.na(mn),list(AFP),sum)$x==0))
  ind2<-which(with(merged_ref_results, aggregate(!is.na(logSD),list(AFP),sum)$x==0))
  ind3<-which(with(merged_ref_results, aggregate(!is.na(adjusted_logSD),list(AFP),sum)$x==0))

  reference_predictions <- function(model,missing_data_index,col_name="predictions") {
    results<-numeric(length=n.ref)

    #Uses predict.merMod
    if (length(missing_data_index)==0) {
      results<-predict(lmer(model,
                            REML=TRUE,
                            data=merged_ref_results,
                            control=lmerControl(
                                check.conv.singular =.makeCC(action = "ignore",
                                                             tol = 1e-4))
                            ),
                       newdata=data.frame(AFP=as.factor(reference_afp)),
                       re.form=~0)

    } else {
     results[-missing_data_index]<-
       predict(lmer(model,
                    REML=TRUE,
                    data=merged_ref_results,
                    control=lmerControl(
                        check.conv.singular =.makeCC(action = "ignore",
                                                     tol = 1e-4))
                    ),
               newdata=data.frame(AFP=as.factor(reference_afp[-missing_data_index])),
               re.form=~0)

     is.na(results)<-missing_data_index
    }

    ret <- data.frame(reference_afp,results)
    names(ret) <-c("reference_afp",col_name)
    return(ret)
  }

  mn1<-reference_predictions(mn~(1|year)+AFP,ind1,"reference_means")
  mn2<-reference_predictions(logSD~(1|year)+AFP,ind2,"reference_actual_logSD")
  mn3<-reference_predictions(adjusted_logSD~(1|year)+AFP,ind3,"reference_adjusted_logSD")

  #Assemble reference results
  final_ref_results<-merge(merge(mn1,mn2,
                                 by=c("reference_afp")),
                           mn3,
                           by=c("reference_afp"))
  final_ref_results$reference_varieties=dat.ref$variety[match(final_ref_results$reference_afp,dat.ref$AFP)]
     
  results<-list()
  results$character_number=character_number
  results$grand_mean=rowMeans(as.data.frame(yearly_results["grand_mean",1:n.yr]))
  results$spline_df=res.df.spline
  results$reference_mean_logSD=mn.adj.logSD.ref.spl
  results$mean_sd_data=yearly_results["mean_sd_data",1:n.yr]
  results$anova=anova_data
  results$reference=final_ref_results

  
  #Assemble candidate results and set names to something "friendly", order columns sensibly
  final_cand_results <- merge(cand_means,aggregate(extrapolation~AFP,extrapolation_detect,max))
    
  names(final_cand_results)<-c("candidate_afp", "candidate_means", "candidate_actual_logSD",
                               "candidate_adjusted_logSD",
                               "regression_factor", "extrapolation_factor", "candidate_varieties",
                               "candidate_prediction_err", "candidate_COYU_pvalue", "extrapolation")
  
  results$candidates<-final_cand_results[,c("candidate_afp","candidate_varieties","candidate_means",
                                            "candidate_actual_logSD", "candidate_adjusted_logSD",
                                            "candidate_prediction_err", "candidate_COYU_pvalue",
                                            "extrapolation", "extrapolation_factor")]
  
  class(results)<-c("COYUs9Results",list)
  return(results)
}
