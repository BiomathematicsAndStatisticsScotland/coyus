
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

#TODO: Need generic function that can print table with symbols, column
#header, rownames etc wrapped at a particular width (a la FORTRAN)

calculate_candidate_uniformity <- function(all_results) {
  
  candidate_afp<-extract_candidate_afp(all_results)
  candidate_name<-extract_candidate_names(all_results)
  characters<-sprintf("%d",unlist(all_results["character",]))
  
  get_threshold<-function(character_results) { character_results$candidates$candidate_coyu_threshold }
  uniformity<-list()
  
  #Bind in variety names/AFPs, just like a normal summary 
  if (is_3_year(all_results)) {
    uniformity$`3_year_reject`=cbind(AFP=candidate_afp,
                                     Variety=candidate_name,
                                     extract_field(all_results,get_threshold,"3_year_reject",
                                                   col_names=characters))
  } else {
    uniformity$`2_year_reject`=cbind(AFP=candidate_afp,
                                     Variety=candidate_name,
                                     extract_field(all_results,get_threshold,"2_year_reject",
                                                   col_names=characters))
    uniformity$`2_year_accept`=cbind(AFP=candidate_afp,
                                     Variety=candidate_name,
                                     extract_field(all_results,get_threshold,"2_year_accept",
                                                   col_names=characters))
  }
  return(uniformity)
}

format_between_plant_summary<-function(coyu_parameters,results,probability_set) {
  
  if (is_3_year(coyu_parameters)) {
    first_dataset<-"3_year_reject"
    alpha_names<-c("3_year_reject")
  } else {
    first_dataset<-"2_year_reject"
    alpha_names<-c("2_year_reject","2_year_accept")
  }
  
  #UNIFORMITY ANALYSIS OF BETWEEN-PLANT STANDARD DEVIATIONS (SD)
  
  # Over year summary
  yearly_summary<-apply(results,2,function(results_col) {
    
    char_results<-results_col[[first_dataset]]

    summary_col_names<-c("AFP","Variety", "Extrapolation", "Char_Mean","Adj_LogSD","Unadj_Log_SD")    
    
    ## Ensure we always have the correct number of values
    pad_values_by_name<-function(target_names,x) {
      indexes=match(setdiff(target_names, names(x)), target_names) 
      padded=pad_at_index(x,indexes)
      names(padded)=target_names
      return(padded)
    }        

    extra_cols_as_df<-function(varieties,field_name, col_name_template) {
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
                         ncol=get_num_trial_years(coyu_parameters)))
            )

        colnames(ret)=column_names
        return(ret)
    }

    candidate_summary<-as.data.frame(
        cbind(char_results$candidates[,c("candidate_afp")],
              strip_string_factor(char_results$candidate$candidate_varieties),
              char_results$candidates[,c("extrapolation_factor",
                                         "candidate_means",
                                         "candidate_adjusted_logSD",
                                         "candidate_actual_logSD")]
              ))

    colnames(candidate_summary)=summary_col_names
    candidate_summary=merge(
        merge(candidate_summary,
              extra_cols_as_df(coyu_parameters$candidates, "cand_mean", "Mean_%s"),
              by=c("AFP"),
              sort=FALSE,
              all.x=TRUE),
        extra_cols_as_df(coyu_parameters$candidates, "cand_logsd", "Log(SD+1)_%s"),
        by=c("AFP"),
        sort=FALSE,
        all.x=TRUE)
      
    reference_summary<-as.data.frame(
        cbind(char_results$reference[,c("reference_afp")],
              strip_string_factor(char_results$reference$reference_varieties),
              rep(NA, nrow(char_results$reference)),
              char_results$reference[,c("reference_means",
                                        "reference_adjusted_logSD",
                                        "reference_actual_logSD")]     
              ))                             
        
    colnames(reference_summary)=summary_col_names

    reference_summary=merge(
        merge(reference_summary,
              extra_cols_as_df(coyu_parameters$reference, "ref_mean", "Mean_%s"),
              by=c("AFP"),
              sort=FALSE,
              all.x=TRUE),
        extra_cols_as_df(coyu_parameters$reference, "ref_logsd", "Log(SD+1)_%s"),
        by=c("AFP"),
        sort=FALSE,
        all.x=TRUE)

    #Remove irrelevant columns before computing means
    reference_means <- colMeans(reference_summary[,c(-1,-2,-3)], na.rm=TRUE)
    
    ## TODO: this could potentially be simplified by using results variable candidate_not_uniform
    sd_symbols<-mapply(function(value,thresholds) {
      if (is.na(value)) {
        return("");
      }                        
      
      if (is_3_year(coyu_parameters)) {
        if (value > thresholds["3_year_reject"]) {
          return("*")
        }
      } else {
        if (value > thresholds["2_year_reject"]) {
          return("+")
        } else if (value > thresholds["2_year_accept"]) { 
          return(":") #See Y2CRI2A in COYU9.for for use of this
        }
      }
      return("")
    },
    candidate_summary[,"Adj_LogSD"],
    apply(matrix(sapply(results_col[alpha_names], 
                        function(character_results) { character_results$candidates$candidate_coyu_threshold }),
                 nrow=length(coyu_parameters$candidates),
                 ncol=length(alpha_names),
                 dimnames=list(coyu_parameters$candidates,alpha_names)
    ),
    1,
    as.list)
    )             
      
    extrapolation_symbols = sapply(char_results$candidates$extrapolation_factor,
                                   function(extrp_val) {
                                       if (!missing(extrp_val) && !is.na(extrp_val)) {
                                           if (extrp_val > 1.2 && extrp_val <= 1.5) {
                                               return (c("!"))
                                           } else if (extrp_val > 1.5) {
                                               return (c("$"))
                                           }
                                       }
                                       return (c(""))
                                   })
      
    #TODO: ideally we'd indicate which years were extrapolated too but this data is not exposed yet
    candidate_symbols <- data.frame(AFP=candidate_summary$AFP,
                                    Variety=c(""),
                                    Extrapolation=c(""),
                                    Char_Mean=extrapolation_symbols,
                                    Adj_LogSD=sd_symbols,
                                    stringsAsFactors = FALSE, 
                                    matrix("",
                                           nrow=nrow(candidate_summary),
                                           ncol=ncol(candidate_summary)-5))    
    
    reference_symbols <- matrix("",nrow=nrow(reference_summary),ncol=ncol(reference_summary))
    
    dimnames(candidate_symbols)<-dimnames(candidate_summary)
    dimnames(reference_symbols)<-dimnames(reference_summary)
    
    return(list(character_num=results_col["character"],
                candidate_summary=candidate_summary,
                reference_summary=reference_summary,
                candidate_symbols=candidate_symbols,
                reference_symbols=reference_symbols,
                reference_means=reference_means,
                degrees_freedom=char_results$spline_df
    ))
  })
  
  return(yearly_summary)
}

print_between_plant_summary<-function(summary_data,summary_symbols, postprocess=function(x) { x }, connection="") {
  col_count<-ncol(summary_data)
  for (row in 1:nrow(summary_data)) {
    afp<-strip_numeric_factor(summary_data[row,"AFP"])
    variety<-strip_string_factor(summary_data[row,"Variety"])
    cat(sprintf("%8s %-12s",afp,variety),        
        postprocess(sprintf("%12.3g%-2s",
                            as.numeric(as.vector(summary_data[row,3:col_count])),
                            as.character(summary_symbols[row,3:col_count]))),
        "\n",
        file=connection
    )
  }
}

print_candidate_uniformity <- function(uniformity_data,connection="") {
  
  col_count<-ncol(uniformity_data)
  for (row in 1:nrow(uniformity_data)) {
    afp<-strip_numeric_factor(uniformity_data[row,"AFP"])
    variety<-strip_string_factor(uniformity_data[row,"Variety"])
    
    cat(sprintf(" %8s %-12s",
                afp,
                variety),        
        sprintf("%5.3g  ",
                as.numeric(as.vector(uniformity_data[row,3:col_count]))),
        "\n",
        file=connection
    )
  }
}

print_candidate_summary <- function(candidate_summary,connection="") {
  char_names<-colnames(candidate_summary)
  
  col_count<-ncol(candidate_summary)
  cat(sprintf(" %8s %-12s","AFP","VARIETY"),sprintf("%5s",char_names[c(-1,-2)]),"\n\n",file=connection)
  for (row in 1:nrow(candidate_summary)) {
    afp<-strip_numeric_factor(candidate_summary[row,"AFP"])
    variety<-strip_string_factor(candidate_summary[row,"Variety"])
    
    cat(sprintf(" %8s %-12s",
                afp,
                variety),        
        sprintf("%5s",
                as.vector(candidate_summary[row,3:col_count])),
        "\n",
        file=connection
    )
  }
  write("\n",file=connection)
}

print_symbol_key <- function(symbols_used,probability_set,connection="") {
  symbol_key<-c("*"=sprintf("SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION AFTER 3 YEARS WITH PROBABILITY %7.4f",
                            probability_set["3_year_reject"]),
                "+"=sprintf("SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION AFTER 2 YEARS WITH PROBABILITY %7.4f",
                            probability_set["2_year_reject"]),
                ":"=sprintf("SD NOT YET ACCEPTABLE ON OVER-YEARS CRITERION AFTER 2 YEARS WITH PROBABILITY %7.4f",
                            probability_set["2_year_accept"]),
                "_"="NO VERDICT.",
                "!"="There is an extrapolation issue but it is limited. The COYU verdicts should be reviewed alongside the COYU graphs to verify that the decisions are sensible.",
                "$"="There is serious extrapolation. The COYU decisions may be unreliable. In these cases, decisions must be made based on the data directly. The graphs in the PDF output may be of use. See TGP8")
  
  #Fortran also has these: # - WARNING - SD, CHARACTER MEAN, OR ADJUSTED SD IS UNUSUAL
  # X  - SD EXCEEDS 1.265 TIMES MEAN OF REFERENCE VARIETIES,
  
  if (length(symbols_used) > 1) {
    write("SYMBOLS\n",file=connection)
    ignore<-sapply(names(symbol_key),function(symbol) {
      if (symbol %in% symbols_used) {
        cat(sprintf("%5s    %s\n",symbol,symbol_key[symbol]),file=connection)
      }
    })
    write("\n",file=connection)
  }
}

#' COYU_print_results_to_string
#'
#' Utility method to call COYU_print_results and return the output as a string.
#'
#' @param results COYUs9AllResults object
#' @param coyu_parameters COYUs9Parameters object describing the parameter set used to generate the results in the first parameter
#' @param character_key COYUs9CharacterKey object containing two columns, the first (CCode) a numeric character code and the second (CName) a text description of the character
#' @param probability_set Numeric vector with named values 2_year_reject, 2_year_accept and 3_year_reject. Usually extracted from a COYUs9ProbabilitySet object
#' @param verbose Optional, default TRUE. Print per-character results in addition to the summary file.
#' @seealso COYU_print_results
#' @export
COYU_print_results_to_string<-function(results,
                                       coyu_parameters,
                                       character_key,
                                       probability_set,                         
                                       verbose=TRUE) {
    
  #Declare results variable so we don't get a warning
  printed_results=""
    
  #Anonymous file() might be a better way to do this
  con = textConnection("printed_results", open = "w", local = TRUE)
  
  COYU_print_results(results,
                     coyu_parameters,
                     character_key,
                     probability_set,
                     connection=con,
                     verbose)
  
  close(con)
  
  return(printed_results)
}


#' COYU_print_results
#'
#' Produce formatted output from a COYUs9AllResults object and
#' associated data. The output format is derived from the DUST software
#' package and is geared towards use in that package.
#'
#' Users with other requirements should investigate \code{\link{COYU_results_as_dataframe}} or use the raw results directly. 
#'
#' @param results COYUs9AllResults object
#' @param coyu_parameters COYUs9Parameters object describing the parameter set used to generate the results in the first parameter
#' @param character_key COYUs9CharacterKey object containing two columns, the first (CCode) a numeric character code and the second (CName) a text description of the character
#' @param probability_set Numeric vector with named values 2_year_reject, 2_year_accept and 3_year_reject. Usually extracted from a COYUs9ProbabilitySet object
#' @param connection Optional, default "". Pass a connection to which to write the results. By default the results will be written to STDOUT.
#' @param verbose Optional, default TRUE. Print per-character results in addition to the summary file. 
#'
#' @seealso COYU_plot_results COYU_character_key COYU_results_as_dataframe
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
#' write.csv(COYU_results_as_dataframe(results1,"2_year_reject"),
#'           "tester.csv")
#' 
#' COYU_plot_results(results1,
#'                   character_key = test_2_year$character_key,
#'                   plot_file="MyPlots.pdf")
#' ## results sent to a pdf file.
#'
#' @export
COYU_print_results<-function(results,
                             coyu_parameters,
                             character_key,
                             probability_set,
                             connection="",
                             verbose=TRUE) UseMethod("COYU_print_results")

#' @export
COYU_print_results.COYUs9AllResults<-function(results,
                                              coyu_parameters,
                                              character_key,
                                              probability_set,
                                              connection="",
                                              verbose=TRUE) {
  
  if (! "COYUs9Parameters" %in% class(coyu_parameters)) {
    stop("coyu_parameters does not have class COYUs9Parameters")
  }
  
  if (! "COYUs9CharacterKey" %in% class(character_key)) {
    stop("character_key does not have class COYUs9CharacterKey")
  }
  
  if ( ! all(c("2_year_reject","2_year_accept","3_year_reject") %in% names(probability_set))) {
    stop("probability_set is missing named values that are required. This shouldn't happen if you use a COYUs9ProbabilitySet object to provide a value to this function. Please see the docs.")
  }
  
  if (is_3_year(coyu_parameters)) {
    symbols_used<-c("$", "!","*")
  } else {
    symbols_used<-c("$", "!","+",":")
  }
  
  between_plant_summary<-format_between_plant_summary(coyu_parameters,results,probability_set)
  
  if (verbose) {  
    ignore<-sapply(between_plant_summary, function(char_summary) {
      cat(sprintf("%50s - %-20s\n\n", char_summary$character_num,
                  get_character_name(character_key, char_summary$character_num)),
          file=connection)
      
      write("      **** UNIFORMITY ANALYSIS OF BETWEEN-PLANT STANDARD DEVIATIONS (SD) ****\n\n",
            file=connection)
      
      cat(sprintf("%8s %-12s ","AFP","VARIETY"),
          sprintf("%14s",colnames(char_summary$reference_summary)[c(-1,-2)]),
          "\n\n",
          file=connection)
      
      write("CANDIDATE\n",file=connection)
      
      print_between_plant_summary(char_summary$candidate_summary,
                                  char_summary$candidate_symbols,
                                  postprocess=function(x) { gsub("NA", "- ",x) },
                                  connection)

      ## Extract character means and logSD means
      cat(sprintf("\n%-34s", "REFERENCE MEANS"),
          sprintf("%14.3g", as.numeric(char_summary$reference_means[c("Char_Mean","Adj_LogSD")])),
          "\n\n",
          file=connection)
      
      write("\nREFERENCE\n",file=connection)
      
      print_between_plant_summary(char_summary$reference_summary,
                                  char_summary$reference_symbols,
                                  postprocess=function(x) { gsub("NA", "  ",x) },
                                  connection)
      
      
      write("\n", file=connection)
      
      print_symbol_key(c(symbols_used,"_"),probability_set,connection)
    
      if (is_3_year(coyu_parameters)) {
          required_df = COYU_MIN_DF_3_YEAR
      } else {
          required_df = COYU_MIN_DF_2_YEAR
      }
      
      if (char_summary$degrees_freedom < required_df) {
          cat(sprintf("RESIDUAL DEGREES OF FREEDOM: %.3g\nWARNING - AT LEAST %.3g DF ARE NEEDED FOR ROBUST CONCLUSIONS\n\n",
                      char_summary$degrees_freedom,
                      required_df),
              file=connection) 
      } else {
          cat(sprintf("RESIDUAL DEGREES OF FREEDOM: %.3g\n\n",
                      char_summary$degrees_freedom),
              file=connection)
      }
      
    })
  }  
  
  write("\n\nCANDIDATE SUMMARY\n",file=connection)
  
  candidate_summary<-cbind(AFP=extract_candidate_afp(results),
                           Variety=extract_candidate_names(results),
                           matrix(sapply(between_plant_summary,
                                         function (char_results) {
                                             apply(char_results$candidate_symbols[,-1],1,
                                                   function (candidate_row) {
                                                       ret<-paste(unique(candidate_row),collapse="")
                                                       if (nchar(ret)==0) {
                                                           ret<-"_"
                                                       }
                                                       return(ret)
                                                   })
                                         }),
                                  nrow=length(coyu_parameters$candidates),
                                  ncol=length(coyu_parameters$characters)
                           ))
  
  colnames(candidate_summary)<-c("AFP", "Variety", sprintf("%02d",extract_characters(results)))
  
  print_candidate_summary(candidate_summary,connection)
  
  print_symbol_key(symbols_used,probability_set,connection)
  
  write("\n\nCANDIDATE UNIFORMITY CRITERIA\n",file=connection)
  cat(sprintf("%-21s ",""),sprintf("%5d  ",coyu_parameters$characters),"\n",file=connection)
  candidate_uniformity<-calculate_candidate_uniformity(results)
  
  if (is_3_year(coyu_parameters)) {
    cat("3 YEAR REJECT:\n\n",file=connection)
    print_candidate_uniformity(candidate_uniformity$`3_year_reject`,connection)
  } else {
    cat("2 YEAR REJECT:\n\n",file=connection)
    print_candidate_uniformity(candidate_uniformity$`2_year_reject`,connection)
    cat("\n2 YEAR ACCEPT:\n\n",file=connection)
    print_candidate_uniformity(candidate_uniformity$`2_year_accept`,connection)
  }
                                          
  write("\n\nCHARACTER KEY :\n",file=connection)
  write(apply(character_key,1, function(x) sprintf("%8s  %-8s",x[1],x[2])),file=connection,ncolumns=2)
}

