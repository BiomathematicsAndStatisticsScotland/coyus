
#Handy function for multiple assignment of variables
vassign <- function(..., values, envir=parent.frame()) {
  vars <- as.character(substitute(...()))
  values <- rep(values, length.out=length(vars))
  for(i in seq_along(vars)) {
    assign(vars[[i]], values[[i]], envir)
  }
}

# Plot options
# 0 - ONLY SUMMARY TABLE
# 1 - ALL TABLES (PLOTS 3 PER PAGE)
# 2 - ALL TABLES (PLOTS 1 PER PAGE)
# -1 ALL TABLES - NO PLOTS
#Output width
# 0 - 80 columns
# 1 - 120 columns
#
# NOTE: for complete compatibility with DUST (particularly the file view) these files should be written in CR/LF format. 
# This need will become more acute if we ever implement write{U,M,J}File routines
writeCoyu9File<-function(input_file,
                         probability_sets=list(),
                         name="COYUs9.DAT",
                         output_file="COYUSOP.DAT",
                         plot_options=1,
                         output_width=0) {
  probability_set_count<-length(probability_sets)

  con<-file(winVirtStoreLocateFileToWrite(name),open="w")
  write(input_file,file=con)
  write(output_file,file=con)
  write(sprintf("%2d %2d %2d",probability_set_count,plot_options,output_width),file=con)
  for (probset in probability_sets) {
    write(probset,file=con)
  }
  close(con)
}

closeIfOpen <- function(con) { 
  if (!missing(con) && is(con,"connection")) {
    if (isOpen(con)) {
      close(con)
    }
  }
}

isAbsolutePath<-function(input_path) {
  #Borrowed largely from R.utils...
  input_path=as.character(input_path)
  if (length(input_path) <= 0L) {
    input_path="."
  }

  nPathnames=length(input_path)
  if (nPathnames == 0L) 
        return(logical(0L))
    if (nPathnames > 1L) {
        res <- sapply(input_path, FUN = isAbsolutePath, ...)
        return(res)
    }
    if (is.na(input_path)) 
        return(FALSE)
    if (regexpr("^~", input_path) != -1L) 
        return(TRUE)
    if (regexpr("^.:(/|\\\\)", input_path) != -1L) 
        return(TRUE)
    components <- strsplit(input_path, split = "[/\\]")[[1L]]
    if (length(components) == 0L) 
        return(FALSE)
    (components[1L] == "")
}

pathRelativeToTargetDir<-function(input_path, target_dir=".") {
  if (isAbsolutePath(input_path)) {
    return(input_path)
  }

  return(file.path(target_dir, input_path))
}

readCoyu9File<-function(name) {
  #Problems reading decimal values in locales other than en_gb (due to use of commas as decimal separator)
  
  target_dir<-dirname(name)

  con <- NULL
  
  tryCatch({
    con<-file(winVirtStoreLocateFileToRead(name),open="r")

    data_file<-list()
    data_file$input_file<-pathRelativeToTargetDir(readLines(con,1), target_dir=target_dir)
    data_file$output_file<-winVirtStoreLocateFileToWrite(pathRelativeToTargetDir(readLines(con,1), target_dir=target_dir))

    
    output_dir = dirname(data_file$output_file)
    
    trial_id=gsub("^(\\w+).*\\.DAT$","\\1",basename(data_file$input_file),ignore.case=TRUE,perl=TRUE)
    
    data_file$plot_file=normalizePath(
      sprintf("%s/%s",output_dir,paste("COYUs_",trial_id,"_",Sys.Date(),".pdf",sep="")),
      mustWork=FALSE)
    data_file$csv_file=normalizePath(
      sprintf("%s/%s",output_dir,paste("COYUs_",trial_id,"_",Sys.Date(),".csv",sep="")),
      mustWork=FALSE)
    
    data_file$prob_set_count=scan(con,what=integer(),n=1,quiet=TRUE)
    data_file$plot_options=scan(con,what=integer(),n=1,quiet=TRUE)
    data_file$output_width=(scan(con,what=integer(),n=1,quiet=TRUE)+1)*80

                                        #Change when we revise the data format accepted by the COYU package
    data_file$probability_sets<-matrix(scan(con,n=data_file$prob_set_count*3,quiet=TRUE),
                                       nrow=data_file$prob_set_count,
                                       ncol=3,
                                       byrow=TRUE,                     
                                       dimnames=list(
                                         NULL,
                                         c("3_year_reject","2_year_reject","2_year_accept")
                                         ))

    class(data_file$probability_sets)<-c("COYUs9ProbabilitySet","matrix")

  }, error=function(e) {
    checkForInvalidEncoding(name,e)
  }, finally= closeIfOpen(con))

  data_file <- modifyList(data_file,readUFile(data_file$input_file,target_dir))
  
  class(data_file)<-c("DustData","list")
  return(data_file)
}

anonymiseDataset<-function(data_file,anonymise_afp=FALSE,anonymise_name=TRUE) UseMethod("anonymiseDataset")

anonymiseDataset.DustData <- function(data_file,anonymise_afp=FALSE,anonymise_name=TRUE,strip_dust_specific=TRUE) {
  if (anonymise_afp) {
    #Replace AFPs with sequential numbers, in trial data and elsewhere
    replacements<-seq(1:nlevels(data_file$trial_data$AFP))    
    afp_values<-unique(strip_numeric_factor(data_file$trial_data$AFP))
    
    data_file$trial_data$AFP <- as.factor(replacements[ match(data_file$trial_data$AFP, afp_values) ]) 
    data_file$coyu_parameters$references <- replacements[ match(data_file$coyu_parameters$references, afp_values )]
    data_file$coyu_parameters$candidates <- replacements[ match(data_file$coyu_parameters$candidates, afp_values)]
  }

  if (anonymise_name) {
    #Replace names with names based on AFP number
    data_file$trial_data$variety<- paste("AFP_",strip_numeric_factor(data_file$trial_data$AFP),sep="")
  }

  if (strip_dust_specific) {
    new_data_file<-data_file[c("trial_data","coyu_parameters","probability_sets","character_key")]
    return(new_data_file)
  }  
  return(data_file)
}

winVirtStoreLocateFileToRead<-function(target_path) {
  if (.Platform$OS.type!="windows") {
    return(target_path)
  }
  
  #Logic: if the equivalent path in the VirtualStore exists, use that, otherwise use the 
  #target path. This allows for files "overwritten" by other parts of the DUST system to be used
  remapped_path = winVirtStoreConvertPath(target_path)
  
  if (file.exists(remapped_path)) {
    return(remapped_path)
  }
  return(target_path)
}

winVirtStoreLocateFileToWrite<-function(target_path) {
  if (.Platform$OS.type!="windows") {
    return(target_path)
  }
  
  #Logic: if target_file/dir exists and is writable, use that. 
  #Otherwise use the equivalen path in VirtualStore
  target_dir=dirname(target_path)
  if (dir.exists(target_dir)) {
    if (file.exists(target_path)) {
      if (file.access(target_path, mode=2)==0) {
        return(target_path)
      }
    } else if (file.access(target_dir, mode=2)==0) {
      return(target_path)
    }
    
    remapped_path = winVirtStoreConvertPath(target_path)
    remapped_dir = dirname(remapped_path)
    dir.create(remapped_dir, showWarnings=FALSE, recursive=TRUE)
    return(remapped_path)
  } else {
    error(sprintf("Directory %s for file %s does not exist",target_dir, target_path))
  }  
}

winVirtStoreConvertPath=function(target_path) {
  virt_store_path=winVirtStoreGetRoot()
  
  remapped_path = file.path(virt_store_path,
                            gsub("^\\w+:/","",
                                normalizePath(target_path, 
                                              winslash=.Platform$file.sep,
                                              mustWork=FALSE)))
  
  return(remapped_path)
}

winVirtStoreGetRoot<-function() {
  #Get the root of the windows VirtualStore for this user
  return(normalizePath(file.path(gsub("\\\\","/",Sys.getenv("LOCALAPPDATA")),"VirtualStore"),
                       winslash=.Platform$file.sep,
                       mustWork=FALSE))
}

readUFile<-function(name,target_dir=".") {
      
    ## NOTE/TODO: There are 2 different UX formats. Original COYU9 fortran
    ## doesn't seem to handle the single-year case so we won't either
    
    ## TODO: strip whitespace from all string data read from this file
    ## Relativise path as best we can 

  con <- NULL
  
  if (!file.exists(name)) {
    name <- normalizePath(sprintf("%s/%s",target_dir,name), mustWork=FALSE)
  } else {
    name <- normalizePath(name, mustWork=FALSE)
  }
  
  tryCatch({
    
    con<-file(winVirtStoreLocateFileToRead(name),open="r")

    
    header<-as.list(read.fortran(con,
                                 c("I2","I1","I2","I2","A1","A72"),
                                 n=1,
                                 col.names=c("crop_type","site","trial_year","trial_type","unknown_field","trial_title")))
    
    vassign(num_trial_years,num_trial_varieties,num_trial_characters,
            values=as.integer(strsplit(gsub("^[[:space:]]*","",readLines(con,1)),"[[:space:]]+")[[1]]))
    
    
    varieties_in_trial<-scan(con,what=integer(),n=num_trial_varieties,quiet=TRUE)
    characters_in_trial<-scan(con,what=integer(),n=num_trial_characters,quiet=TRUE)
    
    mean_files<-readLines(con,num_trial_years)
    stddev_files<-readLines(con,num_trial_years)
    
    num_candidates<-scan(con,what=integer(),n=1,quiet=TRUE)
    
    candidate_varieties<-scan(con,what=integer(),n=num_candidates,quiet=TRUE)
    
  }, error=function(e) {
    checkForInvalidEncoding(name,e)   
  }, finally= closeIfOpen(con))
                                        #While reading M and J files, set to be directory containing this UX file
                                        #If these calls fail the WD may be changed permanently but this is unlikely to matter.
  means<-lapply(mean_files,readMFile,target_dir=dirname(name))
  stddevs<-lapply(stddev_files,readJFile,target_dir=dirname(name))
           
  #Get yearly means and standard deviations in 1 data frame.
  #N.B variety names can be inconsistent between years, AFP is expected to be unique so we check this later
  means_by_year<-do.call("rbind",lapply(means,filterMData,varieties_in_trial,characters_in_trial))
  stddevs_by_year<-do.call("rbind",lapply(stddevs,filterJData,varieties_in_trial,characters_in_trial))

  ## Catch "bad" data files where M and J files do not contain comparable data
  ## Note possibly "better" fix below using merge()
  if (nrow(means_by_year) != nrow(stddevs_by_year)) {
      stop(sprintf("means for year %s: %d rows. stddevs for year %s: %d rows. I will not combine these. Ensure missing data is explicitly recorded", year, nrow(means_by_year), year, nrow(stddevs_by_year)))
  }

    
  # Get canonical variety names from last year of mean values
  last_year <- max(means_by_year$year)
  variety_labels<-means_by_year[,c("AFP","variety")]
    
  ## Combine into a single dataset. Drop Year, AFP and variety columns
  ## from 2nd frame as we'll use the first frame to provide these -
  ## this does not work if means_by_year and stddevs_by_year have
  ## differing numbers of rows
  ##  
  all_data<-cbind(means_by_year,
                   subset(stddevs_by_year,
                          select=!names(stddevs_by_year) %in% c("year","AFP","variety")))
  ## 
  ## Set canonical variety names - fails when we have differing
  ## numbers of rows in means and stddevs and is not necessary anyway
  ##
  all_data$variety<-variety_labels[variety_labels$AFP==all_data$AFP,"variety"]
  
  ## Instead we could do something like this:
  ## all_data=merge(means_by_year, stddevs_by_year, by=c("year", "AFP", "variety"))

    
  
  
  coyu_parameters<-COYU_parameters(candidates=candidate_varieties,
                                   references=setdiff(varieties_in_trial,candidate_varieties),
                                   characters=characters_in_trial,
                                   num_years=num_trial_years)     

  dataset_trial_years<-sapply(means,function(x) { swingYear(attr(x,"header")$year) }) #TODO: Replace with nlevels call
  character_key<-attr(stddevs[[1]],"header")$character_key[[1]]
                                   
  all_data$year <- as.factor(all_data$year)
  all_data$AFP <- as.factor(all_data$AFP)
  
  ## Do some checking TODO: should also check that actual M and J file
  ## counts match trial years as mistakes in the header of such files
  ## (e.g. making 2 files load the same year of data) cause obscure
  ## errors.
    
  if (num_trial_years > 3) {
    stop("COYUs9 can only analyse data sets of up to 3 years")
  }

  if (num_candidates >= length(varieties_in_trial)) {
    stop(sprintf("Number of candidates in trial >= Number of varieties in trial (%d >= %d)",
                 num_candidates,
                 length(varieties_in_trial)))
  }

  missing_varieties<-setdiff(candidate_varieties,varieties_in_trial)

  if (length(missing_varieties) > 0) {
    stop("Specified candidate varieties [",paste(missing_varieties,collapse=","),"] are missing from this trial")
  }

  class(all_data)<-c("COYUs9TrialData","data.frame")

  return(modifyList(header,
                    list(trial_data=all_data,
                         coyu_parameters=coyu_parameters,
                         dataset_trial_years=dataset_trial_years,
                         character_key=character_key
                         )
                    )
         )
              
}

#Implement swing date to deal with 2-digit years in crop data files.
#1970 is chosen as the swing default as there doesn't seem to be any
#data before 1984.
swingYear<-function(year_2d,swing=70,century_pre=1900,century_post=2000) {
  if (year_2d < swing) {
    return(year_2d+century_post)
  } else {
    return(year_2d+century_pre)
  }
}

nameCharacters<-function(character_codes,character_prefix) {
  return(sprintf(paste(character_prefix,"%02d",sep=""),character_codes))
}

#Extract characters and varieties of interest, merge header into "year" column
filterJData<-function(stddev_data,varieties_AFP,characters,character_prefix="sUP") {
  character_names<-nameCharacters(characters,character_prefix)

  header<-attr(stddev_data,"header")
  stddev_data$year=swingYear(header$year)

##   #Check variety AFP OK - TODO: only require candidate varieties to be present
##   missing_varieties<-setdiff(varieties_AFP,stddev_data$AFP)
##   missing_characters<-setdiff(characters,header$character_key[[1]]$CCode)

##   if (length(missing_varieties) > 0) {
##     stop("Required AFP numbers missing from data file '",header$file_name,
##          "'. Missing AFPs: [",paste(missing_varieties,collapse=","),"]")
##   } 
##   if (length(missing_characters) > 0) {
##     stop("Required characters missing from data file '",header$file_name,
##          "'. Missing Characters: [",paste(missing_characters,collapse=","),"]")
##   }
  
  #Check characters OK.
  
  filtered_matrix<-stddev_data[ which(stddev_data[,'AFP'] %in% varieties_AFP), c("year","AFP","variety",character_names)]
  attr(filtered_matrix,"header")<-header
  return(filtered_matrix)
}

filterMData<-function(mean_data,varieties_AFP,characters,character_prefix="UP") {
  filterJData(mean_data,varieties_AFP,characters,character_prefix)
}

#The 'J' files contain the standard deviations for each character.
readJFile<-function(name,character_prefix="sUP",target_dir=".") {

  if (!file.exists(name)) {
    name <- normalizePath(sprintf("%s/%s",target_dir,name), mustWork=FALSE)
  }

  con <- NULL
  
  tryCatch({
    con<-file(winVirtStoreLocateFileToRead(name),open="r")

    header<-read.fortran(con,
                         c("I2","I1","I2","I2","A1","A72"),
                         n=1,
                         col.names=c("crop_type","site","year","trial_type","unknown_field","trial_title"))

                                        #Could scan() to a data frame then assign names via col.names
    vassign(num_varieties,num_characters,num_replicates,num_plants_per_plot,
            values=as.integer(strsplit(gsub("^[[:space:]]*","",readLines(con,1)),"[[:space:]]+")[[1]]))

    header$file_name=name
    header$num_varieties<-num_varieties
    header$num_characters<-num_characters
    header$num_replicates<-num_replicates
    header$num_plants_per_plot<-num_plants_per_plot

    varieties<-read.fortran(con,
                            c("4X","I6","A12"),
                            n=num_varieties,
                            col.names=c("AFP","variety"))
    varieties$variety=gsub("[[:space:]]*$","",varieties$variety)

    characters<-read.fortran(con,
                             c("I4","A8"),
                             n=num_characters,
                             col.names=c("CCode","CName"))

    class(characters)<-c("COYUs9CharacterKey","data.frame")
    
    header$character_key<-list(characters)
    
    ## Read actual data into a matrix

    ## Note: cannot rely at this stage on either AFP number OR variety
    ## name being unique so we simply have these as columns in the dateset
    ## rather than rownames  
    character_data<-matrix(scan(con,n=num_varieties*num_characters,quiet=TRUE),
                           nrow=num_varieties,
                           ncol=num_characters,
                           byrow=TRUE,                     
                           dimnames=list(NULL,nameCharacters(characters$CCode,character_prefix))
                           )
  }, error=function(e) {
    checkForInvalidEncoding(name,e)
  }, finally= {
    if (isOpen(con)) {
      close(con)
    }
  })

  ## -1 (or less) is apparently the code for missing values; this is not documented in the DUST manual
  character_data[character_data<=-1]=NA
  
  file_data<-data.frame(cbind(varieties,character_data))
  attr(file_data,"header")<-header

  if (any(duplicated(varieties$AFP))) {
    warning(paste("Duplicate AFP numbers in data file '",name,
                  "'. Duplicate values: [",
                  paste(unique(varieties$AFP[duplicated(varieties$AFP)]),collapse=","),"]"))
  }
  
  return(file_data)  
}

#The 'M' files contain means for each character. The format is the same as the 'J' files
readMFile<-function(name,character_prefix="UP",target_dir=".") {
  readJFile(name,character_prefix,target_dir)
}

checkForInvalidEncoding <- function(name,error_obj) {
  if(length(grep("invalid multibyte string",error_obj$message)) > 0) {
    errmsg<-paste("Error reading file '",name,"'\n",
                  "Consider using a different value for 'options(encoding)' with this dataset. (e.g. 'ISO-8859-1')\n",
                  "Current encoding is '",options("encoding"),"'\n",
                  "Base error was >>",
                  sub("[[:space:]]+$","",error_obj),
                  "<<",sep="")
    stop(errmsg)
  }
  stop(error_obj)
}
