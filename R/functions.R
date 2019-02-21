#' Clean sema data
#'
#' This function collates and performs initial cleaning of raw data from the SEMA smartphone app. 
#' @param input The file path to the folder containing your sema data files. Important that this is the folder, not a specific file.
#' @param rt.trim Whether to remove responses that are below the rt.min threshold on reaction time. If this is TRUE, first, surveys with 
#' more responses below rt.min (default 500ms) than the allowed rt.threshold (default .5 or 50%) will be replaced with missing values (NA). 
#' Then, individual responses below the reaction time threshold from remaining surveys will be replaced with NA values. 
#' @param rt.min The reaction time threshold below which response times are considered invalid (if rt.trim = TRUE).
#' @param rt.threshold The proportion of too-fast responses (faster than rt.min) before a survey is replaced with missing values (only active if rt.trim
#' is set to TRUE)
#' @param na.value By default this is NULL, which means that missing data will be saved as NA, which is the default in R (and is read nicely by SPPS, I believe). 
#' If you wish to set missing values to something particular like -99, for instance, you would set na.value = -99
#' @export
clean_sema <- function(input, rt.trim = FALSE, rt.min = 500, rt.threshold = .5, na.value = NULL) {
  
  # Note - I have updated the package to return a dataframe rather than save a csv. More flexibility and the number of lines of code to save the output
  # is roughly the same anyway. 
  if (grepl('.csv', input)) {
    stop("input must be the folder containing sema data files, not a particular data file. For example '/Users/Sean/Data/sema_study_1_files'.")
  }
  
  print('Beginning cleaning process')
  
  # Stitch together all the different csvs for versions of the program (survey). Variables that only appear
  # in some surveys will be filled with NA in other survey sections.
  files = lapply(list.files(input, pattern = '*.csv', full.names = TRUE), read.csv, stringsAsFactors = FALSE)
  
  if (length(files) == 0) {
    stop("No files found. Are you sure you put in the right folder location? It should look like '/Users/Sean/Data/sema_study_1_files'.")
    }
  files <- plyr::rbind.fill(files)
  
  # Lower all the column names
  colnames(files) <- tolower(colnames(files))
  
  # Pete renames the participant id column, so we do that. 
  colnames(files) <- gsub('participant_id', 'sema_id', colnames(files))
  
  # Recode has_answers to 0 and 1. 
  files$has_answers <- as.numeric(as.factor(files$has_answers)) - 1
  
  ### Dealing with RTs early on so the multichoice RTs are before the meta variables. 
  
  # So here we're recoding all the zeroes in an RT variable to NA, since that's what they mean. 
  files[, grep('_rt$', colnames(files))][files[, grep('_rt$', colnames(files))] == 0] <- NA
  
  
  # then for the multi-choice items, taking the mean since they're the same across all choices within
  # a single item. 
  
  if (length(grep('\\.[0-9]*$', colnames(files)) > 0)) {
    multi <- unique(str_match(grep('\\.[0-9]*$', colnames(files), value = TRUE), '([a-z]*)\\.')[,2])
    
    for (x in 1:length(multi)) {
      files[[paste0(multi[x], '_rt')]] <- rowMeans(files[, grep(paste0(multi[x], '\\.[0-9]*_rt$'), colnames(files))], na.rm = TRUE)
    }
    
    # Then remove the original Rts
    files <- files[, -grep('\\.[0-9]+_rt$', colnames(files))]
    
  } else {
    multi <- NULL
  }
  

  
  # Now I'm getting a discrepancy such that the response_time_ms variable thinks it took longer than the sum
  # says (whenever there is an error). This only happens rarely, and I can't figure out why. To be 
  # continued. 
  
  # (rowSums(files[, grep('_rt', colnames(files))], na.rm = TRUE) - files$response_time_ms)
  # sum(files[78, grep('_rt', colnames(files))])
  # files[78, 'response_time_ms']
  
  
  ### Update: Have learned that the row number in the raw data does -not- correspond to survey delivery, as I had been led to believe. 
  ### Pete meant the row number variable he had calculated in later data. So we need to first sort by delivery date, and only then can 
  ### we accurately create this variable. For this reason, we calculate delivery time earlier than before. 
  
  
  # Date and time cleaning. 
  delivtime <- lubridate::dmy_hms(files$delivered)
  
  # Extract the date delivered
  files$datedlv <- lubridate::date(delivtime)
  
  # A bit hacky, and maybe there's a direct function for this, but just extracting the time of day in 24 hour. 
  files$timedlv <- lubridate::hour(delivtime) + 
    lubridate::minute(delivtime)/60 + 
    lubridate::second(delivtime)/3600
  
  rm(delivtime)
  
  # Create rownr, the number of the survey received
  files <- files %>% dplyr::arrange(sema_id, datedlv, timedlv) %>%
  # We've added an arrange call here to sort things by the delivery time, so row number will be accurate. 
    dplyr::group_by(sema_id) %>% 
    dplyr::mutate('rownr' = 1:n()) %>% 
    as.data.frame()
  
  
  print(paste0('The median number of surveys received in your data was ', median(as.numeric(table(files$sema_id))),
               ' and the median number of responses was ', median(as.numeric(table(files$sema_id[files$has_answers == 1])))))
  
  
  print(paste0('The greatest number of surveys received was ', max(as.numeric(table(files$sema_id))),
               ' and the greatest number of responses was ', max(as.numeric(table(files$sema_id[files$has_answers == 1])))))
  
  
  print(paste0('The fewest number of surveys received was ', min(as.numeric(table(files$sema_id))),
               ' and the fewest number of responses was ', min(as.numeric(table(files$sema_id[files$has_answers == 1])))))
  
  # We have to include the response time filtering before we calculate completenr, because we don't 
  # want to include missing surveys there. But we want to calculate surveys responded first so we know
  # how many were removed for RT.
  
  # Create the aggregate variables of surveys received and complete for each P. 
  files <- dplyr::group_by(files, sema_id) %>% 
    dplyr::mutate('surveys_received' = n()) %>% 
    dplyr::mutate('surveys_responded' = sum(has_answers == 1)) %>% 
    as.data.frame()
  
  # Now we count the number of too-fast responses for each survey.
  files$fastrtcount <- apply(files[, grep('_rt', colnames(files))], 1, function(x) sum(x < rt.min, na.rm = TRUE))
  
  # And the number of actual responses made in each survey. This is actually somewhat useful in itself - you
  # could use this to filter out surveys which have too much missing data. 
  files$responsecount <- apply(files[, grep('_rt', colnames(files))], 1, function(x) sum(!is.na(x)))
  
  # Create a vector of the actual survey content to use for removing responses
  survey_responses <- (grep('response_time_ms', colnames(files)) + 1):(grep('rownr', colnames(files)) - 1)
  
  # First we optionally remove surveys with too many too-fast responses
  if (rt.trim) {
    toofast <- files$fastrtcount/files$responsecount
    # Correct for division by zero in the case of missing surveys. 
    toofast[!is.finite(toofast)] <- 0
    toofast <- toofast > rt.threshold
    
    files[toofast, survey_responses] <- NA
    files[toofast, 'has_answers'] <- 0
    
    print( paste0(sum(toofast), ' surveys replaced with missing data for containing more than ', rt.threshold*100, ' percent too-fast responses'))
    rm(toofast)
  }
  
  # Then we optionally remove those too-fast responses that remain in other surveys. Unfortunately we can't
  # find the matching responses too easily because of the multiple choice ones messing it up. 
  
  if (rt.trim) {
    
    # First do the easy part - removing the non multi-choice responses that match too-fast responding. 
    rts <- grep('_rt', colnames(files), value = TRUE)
    names <- gsub('_rt', '', rts)
    
    if (length(multi) > 0) {
      rts <- rts[!names %in% multi]
      names <- names[!names %in% multi]
    }
    
    if (!(all(names %in% colnames(files)))) {
      stop('Error: All reaction time variables do not have a matching data variable')
    }
    
    files[, names][files[, rts] < rt.min & !is.na(files[, rts])] <- NA
    
    # Now to do the multiple choice responses - removing these one at a time due to the more complicated
    # structure. The !is.na calls aren't strictly neccesary but help to actually see what you're removing
    # if you use the code by hand. 
    
    if (length(multi) > 0) {
      for (i in 1:length(multi)) {
        rts <- paste0(multi[i], '_rt')
        names <- grep(paste0(multi[i], '\\.[0-9]'), colnames(files), value = TRUE)
        files[, names][files[rts] < rt.min & !is.na(files[rts]), ] <- NA
      }
    }
    
    print (paste0(sum(files[, grep('_rt', colnames(files))] < rt.min & !is.na(files[, grep('_rt', colnames(files))])), 
           ' additional answers removed across all surveys for having reaction times below ', rt.min, 'ms.'))
    # Now to remove the RTs themselves - all at once because this is easier. 
    files[, grep('_rt', colnames(files))][files[, grep('_rt', colnames(files))] < rt.min & !is.na(files[, grep('_rt', colnames(files))])] <- NA
    
    
  }
  
  # Count the number of entire surveys removed because of RT speed. We include this regardless of 
  # whether rt.trim is on so the output always has the same number of columns. 
  files <- dplyr::group_by(files, sema_id) %>% 
    dplyr::mutate('surveys_removed' = surveys_responded - sum(has_answers == 1)) %>% 
    as.data.frame()
  
  # Create datanr, the number of survey with data (in case people want to lag on that instead)
  # Wrote a function to do this (see bottom of file).
  files <- data_nr_calc(files)
  

  

  
  # We still need:
  
  # Interval in hours from previous delivered, and day number (the number of days for the participant)
  files <- get_interval(files)
  
  # Can't think of a more efficient way to do this atm. Slice each partiicpant to have only one day, 
  # arrange by day, create day number, then join it to the main file. There might be a filter technique
  # you could use instead of slicing and ungrouping, not sure. 
  days <- group_by(files, sema_id, datedlv) %>%
    slice(1) %>% 
    ungroup() %>%
    group_by(sema_id) %>%
    arrange(sema_id, datedlv) %>%
    mutate('daynr' = 1:n()) %>%
    select(sema_id, datedlv, daynr) %>%
    as.data.frame()
  
  files <- join(files, days, by = c('sema_id', 'datedlv'))
  
  rm(days)
  
  files$day_of_week <- lubridate::wday(files$delivered, label = TRUE)
  files$weekend <- files$day_of_week %in% c('Sat', 'Sun')
  
  ### Final steps that have yet to be added. These would need to slice the first row of each participant 
  ### to get the start of the study for them, or the first row of each participant-day to get the start
  ### of the day for them. 
  start <- group_by(files, sema_id) %>%
    slice(1) %>%
    mutate('survey_start' = delivered) %>%
    select(sema_id, survey_start) %>%
    as.data.frame()
  
  files <- join(files, start,  by = c('sema_id'))
  
  start <- group_by(files, sema_id, datedlv) %>%
    slice(1) %>%
    mutate('day_start' = delivered) %>%
    select(datedlv, sema_id, day_start) %>%
    as.data.frame()
  
  files <- join(files, start, by = c('sema_id', 'datedlv'))
  
  rm(start)
  
  # Calculate a continuous time variable like minutes since the start of the study. To look at time trends. 
  files$minutes_since_survey_start <- as.numeric((lubridate::dmy_hms(files$delivered) - lubridate::dmy_hms(files$survey_start))/60)
  
  # Do minutes since first survey of day as well. To look at diurnal trends. To look at whether responding changes over the 
  # course of day regardless of what time it is - might be survey fatigue. 
  files$minutes_since_day_start <- as.numeric((lubridate::dmy_hms(files$delivered) - lubridate::dmy_hms(files$day_start))/60)
  
  
  # Check the timezones of the participants for errors. 
  timezonecheck <- dplyr::group_by(files, sema_id) %>% 
    dplyr::summarize('timezones' = length(table(timezone))) %>%
    dplyr::filter(timezones > 1)
  
  print(paste0('Warning: The following participants have more than one timezone listed - this may indicate errors or travel on their part and could effect the order of the data: ', 
               paste(timezonecheck$sema_id, collapse = ', ')))
  rm(timezonecheck)
  
  if (!(is.null(na.value))) {
    print(paste0('Saving missing values as ', na.value))
    # If people want to enter an arbritrary missing value, we need to convert the 'Date' columns back to character, because they do 
    # -not- want random entries being included that don't fit. We only do this is there is missing data in a Date column. Which should 
    # really only be datedlv but let's be as flexible as possible. 
    
    if (sum(is.na(files[sapply(files, function(x) class(x)) == 'Date'])) > 0) {
      print('Warning: Converting date variables back to character in order to accept arbritrary values')
      files[sapply(files, function(x) class(x)) == 'Date'] <- sapply(files[sapply(files, function(x) class(x)) == 'Date'], as.character)
    }
    
    files[is.na(files)] <- na.value
  }
  
  return(files)
  
}

#' Calculate data number
#'
#' This function takes a sema dataset (must contain a variable sema_id and rownr) and calculates 'datanr', a survey number variable
#' tracking only surveys with data for each participant. This is useful if you want to time-lag using all available
#' data (though of course this can mean that you are lagging across larger time gaps for some participants than others or on some days).
#' This variable is calculated when the clean_sema function is initially called, but you can also call it directly afterwards if you 
#' remove additional data (for example, removing surveys where responsecount is too low)
#' 
#' Note that this will overwrite the previous version of datanr, so if you want to keep that variable for some reason, (for instance, you 
#' want to keep absolute data number separate from data number after surveys with too few responses are removed) then simple copy datanr into
#' a new variable before using this function (e.g. mydata$old_data_nr <- mydata$datanr), then use this function. 
#' 
#' @param semadata An R dataframe containing both sema_id and rownr variables (most likely, a sema dataset -after- processing with clean_sema)
#' @export
data_nr_calc <- function(semadata) {
  nonmissing <- dplyr::group_by(semadata, sema_id) %>% 
    dplyr::filter(has_answers == 1) %>%
    dplyr::mutate('datanr' = 1:n()) %>%
    dplyr::select(sema_id, rownr, datanr) %>%
    as.data.frame()
  
  semadata <- join(semadata, nonmissing, by = c('sema_id', 'rownr'))
  semadata
}

#' Get interval
#'
#' This function takes a sema dataset (must contain variables sema_id, timedlv and datedlv) and calculates the time interval
#' between successive rows (in minutes as this is more useful). It will overwrite the interval variable created by clean_sema, but may be useful for
#' re-calculating intervals on datasets you have trimmed to only included surveys with responses.
#' @param semadata An R dataframe containing both sema_id and datedlv variables (most likely, a sema dataset -after- processing with clean_sema)
#' @export
get_interval <- function(semadata) {
  semadata <- group_by(semadata, sema_id, datedlv) %>%
    arrange(sema_id, datedlv, timedlv) %>% 
    mutate('interval' = 60*(timedlv - lag(timedlv))) %>%
    as.data.frame()
  
  semadata
}
