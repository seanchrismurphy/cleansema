### For testing purposes
input <- '/Users/Sean/Dropbox/Post-Doc/Code/SEMA/sema_export_Objectifying_Experiences_2016_2017_04_25_22_29_08'
output <- '/Users/Sean/Desktop/clean data.csv'
rt.min <- 500
rt.trim <- TRUE
cleansema <- function(input, output, rt.min = 500) {
  
  # Stitch together all the different csvs for versions of the program (survey). Variables that only appear
  # in some surveys will be filled with NA in other survey sections.
  files = lapply(list.files(input, pattern = '*.csv'), read.csv, stringsAsFactors = FALSE)
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
  
  multi <- unique(str_match(grep('\\.[0-9]*$', colnames(files), value = TRUE), '([a-z]*)\\.')[,2])
  
  for (x in 1:length(multi)) {
    files[[paste0(multi[x], '_rt')]] <- rowMeans(files[, grep(paste0(multi[x], '\\.[0-9]*_rt$'), colnames(files))], na.rm = TRUE)
  }
  
  # Then remove the original Rts
  files <- files[, -grep('[0-9]+_rt$', colnames(files))]
  
  # Now I'm getting a discrepancy such that the response_time_ms variable thinks it took longer than the sum
  # says (whenever there is an error). This only happens rarely, and I can't figure out why. To be 
  # continued. 
  
  # (rowSums(files[, grep('_rt', colnames(files))], na.rm = TRUE) - files$response_time_ms)
  # sum(files[78, grep('_rt', colnames(files))])
  # files[78, 'response_time_ms']
  
  # Create rownr, the number of the survey received
  files <- dplyr::group_by(files, sema_id) %>% 
    dplyr::mutate('rownr' = 1:n()) %>% 
    as.data.frame()
  
  # We have to include the response time filtering before we calculate completenr, because we don't 
  # want to include missing surveys there. But we want to calculate surveys completed first so we know
  # how many were removed for RT.
  
  # Create the aggregate variables of surveys received and complete for each P. 
  files <- dplyr::group_by(files, sema_id) %>% 
    dplyr::mutate('surveys_received' = n()) %>% 
    dplyr::mutate('surveys_completed' = sum(has_answers == 1)) %>% 
    as.data.frame()
  
  # Now we count the number of too-fast responses for each survey.
  files$fastrtcount <- apply(files[, grep('_rt', colnames(files))], 1, function(x) sum(x < rt.min, na.rm = TRUE))
  
  # And the number of actual responses made in each survey. This is actually somewhat useful in itself - you
  # could use this to filter out surveys which have too much missing data. 
  files$rtcount <- apply(files[, grep('_rt', colnames(files))], 1, function(x) sum(!is.na(x)))
  
  # Create a vector of the actual survey content to use for removing responses
  survey_responses <- (grep('response_time_ms', colnames(files)) + 1):(grep('rownr', colnames(files)) - 1)
  
  # First we optionally remove surveys with too many too-fast responses
  if (rt.trim) {
    toofast <- files$fastrtcount/files$rtcount
    # Correct for division by zero in the case of missing surveys. 
    toofast[!is.finite(toofast)] <- 0
    toofast <- toofast > .5
    
    files[toofast, survey_responses] <- NA
    files[toofast, 'has_answers'] <- 0
    rm(toofast)
  }
  
  # Then we optionally remove those too-fast responses that remain in other surveys
  if (rt.trim) {
    files[, grep('_rt', colnames(files))][files[, grep('_rt', colnames(files))] < rt.min] <- NA
  }
  

  
  # Create completenr, the number of survey completed (in case people want to lag on that instead)
  # Since using filter to get the completenr removes the others, have to do it separately and join
  nonmissing <- dplyr::group_by(files, sema_id) %>% 
    dplyr::filter(has_answers == 1) %>%
    dplyr::mutate('completednr' = 1:n()) %>%
    dplyr::select(sema_id, rownr, completednr) %>%
    as.data.frame()
  
  files <- join(files, nonmissing)
  
  rm(nonmissing)
  
  # Count the number of entire surveys removed because of RT speed. We include this regardless of 
  # whether rt.trim is on so the output always has the same number of columns. 
  files <- dplyr::group_by(files, sema_id) %>% 
    dplyr::mutate('surveys_removed' = surveys_completed - sum(has_answers == 1)) %>% 
    as.data.frame()
  
  # Date and time cleaning. 
  delivtime <- lubridate::dmy_hms(files$delivered)
  
  # Extract the date delivered
  files$datedlv <- lubridate::date(delivtime)
  
  # A bit hacky, and maybe there's a direct function for this, but just extracting the time of day in 24 hour. 
  files$timedlv <- lubridate::hour(delivtime) + 
    lubridate::minute(delivtime)/60 + 
    lubridate::second(delivtime)/3600
  
  rm(delivtime)
  
  # We still need:
  
  # Interval in minutes from previous delivered, and day number (the number of days for the participant)
  files <- group_by(files, sema_id, datedlv) %>%
    arrange(sema_id, datedlv, timedlv) %>% 
    mutate('interval' = timedlv - lag(timedlv)) %>%
    as.data.frame()
  
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
  
  files <- join(files, days)
  
  rm(days)
  
  
  ### Final steps that have yet to be added. These would need to slice the first row of each participant 
  ### to get the start of the study for them, or the first row of each participant-day to get the start
  ### of the day for them. 
  
  # Calculate a continuous time variable like minutes since the start of the study. To look at time trends. 
  
  # Could do minutes since first survey of day as well. To look at diurnal trends. To look at whether responding changes over the 
  # course of day regardless of what time it is - might be survey fatigue. 
  
  
  
}