context("Test control files with UTF-8 chars")

## Note - combine this with a test of the DUST stub routines to ensure
## we're as UTF-8 compliant as possible. Unfortunately DUST itself
## probably can't cope with UTF-8 format control files (and paths
## containing UTF-8 in the interface)
#source("IORoutines.R")

## read in some example U files and test we can write them out again in a usable form.

format_and_wrap <- function(format_str, data_elements, width=options()$width) {
    wrap_pattern=sprintf('(.{1,%d})(\\s|$)', width)
        
    return(trimws(
        gsub(wrap_pattern, '\\1\n ',
             paste(
                 sprintf(format_str, data_elements),
                 collapse="")
             ),
                 
        which="right"
    ))
}

getFileHeaderByYear<-function(file_info, year) {
    year_char = as.character(year)
    if (nchar(year_char)==4) {
        year_2_digit = substr(year_char, 3, 4)
    } else if (nchar(year_char)==2) {
        year_2_digit = year_char
    } else {
        stop("year should be either 2 digit (19) or 4 digit (2019)")
    }

    for (info in file_info) {
        if (info$year==year_2_digit) {
            return(info)
        }
    }
    return (NULL)
}

writeDataFile<-function(dust_data, year, character_name_func, file_info, connection="") {
    ## This approach might not be able to faithfully reconstruct data
    ## files in various cases
    ## 
    ## Firstly, where there are multiple M or J files for the
    ## same year (technically allowed, but not common, as the same
    ## characters/varieties occuring multiple times in the same year will
    ## conflict)
    ##
    ## We'd have to modify our parameters object to store the source
    ## of each individual data value (or just the parsed structure of
    ## the M and J files   
    ##
    ## We can't reproduce the input exactly as the first column
    ## in the varieties list (some random number) is ignored on
    ## read. So we just use the AFP number (2nd column) see readJFile
    ## for details. Also we only show the characters loaded in the
    ## trial originally, not what was originally in the data file.    
    ##   
    ## This can be corrected by preserving the first column as
    ## within_trial_id or something similar, but as it doesn't appear
    ## to have a use I haven't bothered (yet). We'd also need to
    ## preserve all the trial data to solve the latter issue
    ##
    ## But all that is a pain. So we won't do it yet.

    header = getFileHeaderByYear(file_info, year)
   
    
    if (is.null(header)) {
        stop(sprintf("No data for year %s present", year))
    }
    
    ## Pull out data for year that was originally present in the data file.
    ## Get the intersection of characters in this file and characters present in the trial
    ## as not all data is preserved.
    character_key = header$character_key[[1]]
    characters_in_this_file = intersect(dust_data$coyu_parameters$characters,
                                        character_key$CCode)

    
    data_names = append(c("AFP", "variety"), sapply(characters_in_this_file, character_name_func))

    year_data = dust_data$trial_data[ dust_data$trial_data$year == year,
                                      data_names, 
                                      drop=FALSE ]

    ## DUST represents missing data with -1
    year_data[is.na(year_data)]=-1
    
    header_text=sprintf("%02d%1d%02d%02d%s%s",
                        header$crop_type,
                        header$site,
                        header$year,
                        header$trial_type,
                        header$unknown_field,
                        header$trial_title)

    item_counts = sprintf("%4d %4d %4d %4d",
                          header$num_varieties,
                          header$num_characters,
                          header$num_replicates,
                          header$num_plants_per_plot)
    
    varieties = as.character(apply(year_data,
                                   1,
                                   function(x) {
                                       sprintf("%4s%6s%s",
                                               x["AFP"], x["AFP"], x["variety"])
                                   }))

    characters = as.character(sapply(characters_in_this_file,
                                    function(x) {                                        
                                        sprintf("%4s%s", x, character_key[character_key$CCode==x,2])
                                    }))

    variety_data = as.character(
        lapply(
            split(year_data[,c(-1,-2)],
                  seq(nrow(year_data)),
                  drop=T),
            function(x) {
                format_and_wrap("%10.3f", x)
            }))
    
    write(header_text, file=connection)
    write(item_counts, file=connection)
    write(varieties, file=connection)
    write(characters, file=connection)
    write(variety_data, file=connection)          
}

writeJFile<-function(dust_data, year, connection="") {
    return(writeDataFile(dust_data,
                         year,
                         character_name_func=name_stddev,
                         file_info=dust_data$j_file_info,
                         connection=""))
}

writeMFile<-function(dust_data, year, connection="") {
    return(writeDataFile(dust_data, year,
                         character_name_func=name_mean,
                         file_info=dust_data$m_file_info,
                         connection=""))
}

writeUFile<-function(dust_data, connection="") {

    ## Format the header
    header=sprintf("%2d%1d%2d%2d%s%s",
                   dust_data$crop_type,
                   dust_data$site,
                   dust_data$trial_year,
                   dust_data$trial_type,
                   dust_data$unknown_field,
                   dust_data$trial_title)
    

    
    item_counts = sprintf("%2d %3d %3d",
                          dust_data$coyu_parameters$num_trial_years,
                          length(dust_data$coyu_parameters$references) +
                          length(dust_data$coyu_parameters$candidates),
                          length(dust_data$coyu_parameters$characters))

    varieties = format_and_wrap(" %d ", append(dust_data$coyu_parameters$candidates,
                                              dust_data$coyu_parameters$references),
                                width=50)

    characters = format_and_wrap(" %d ", dust_data$coyu_parameters$characters)
    
    m_files = paste(lapply(dust_data$m_file_info,
                           function(x) { x$file_name }))
    j_files = paste(lapply(dust_data$j_file_info,
                           function(x) { x$file_name }))

    candidate_count = sprintf("%2d", length(dust_data$coyu_parameters$candidates))
    candidates = format_and_wrap(" %d ", dust_data$coyu_parameters$candidates)

    write(header, file=connection)
    write(item_counts, file=connection)
    write(varieties, file=connection)
    write(characters, file=connection)
    write(m_files, file=connection)
    write(j_files, file=connection)
    write(candidate_count, file=connection)
    write(candidates, file=connection)                            
}

#writeJFile(test_2_year, 1992)

## Grab our test_2_year data

## Write it out as UX, MX and JX files

## Create a control file

## Now run it via the R COYU stub

## Optionally run it via COYUS9.exe

## Do all of the above again, but this time with diacritics
