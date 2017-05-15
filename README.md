# cleansema

## Overview
The cleansema package is designed to perform the initial cleaning of data collected using the SEMA smartphone application. All you need to do is use the `cleansema` function in R by pointing it at the directory containing your raw data, and it will save clean, processed data as output. 

## Use
You'll first want to install R and Rstudio (see [here](https://www.researchgate.net/publication/316678011_A_Psychologist's_Guide_to_R]) for a quick walkthrough of that process, which is quite simple.)

One you have RStudio installed and open, using the cleansema package is simple. The first time you want to use it, you'll need to run the following code to install it. Paste this code into the command line of RStudio and hit run:

```
install.packages('devtools')
library(devtools)
install_github("seanchrismurphy/cleansema")
```

After that, you're ready to use the cleansema function on your data. This function requires two pieces of information - the input folder that contains your raw data, and the path to the .csv file you'd like to create with the processed data in it.


For example, running the code like this will take any files in the 'Raw Sema Data' folder, process them, and save them on the desktop as a csv file called 'clean_data.csv'. Adjust these two paramaters as needed (note that Windows users will need to use forward slashes, not the default back slashes that Windows uses)

```
require(cleansema)

cleansema(input = 'Users/Sean/Raw Sema Data/', output = 'Users/Sean/Desktop/clean_data.csv')
```

That's all there is to it!

## How it works and what you get

The cleansema function works in several steps, outlined below.
1. The individual .csv files (that sema creates for each version of the study) are joined together.
2. column names are changed to lowercase, and the `participant_id` column is renamed to `sema_id`
3. The `has_answers` column, which indicates if a survey was responded to, is recoded to 0s (for no) and 1s (for yes)
4. Reaction time variables have 0s (which indicate missing data) recoded to explicitly missing (NA in R).
5. The reaction time fields for multiple choice variables, which were spread out and duplicated across columns, are neatened up into a single column for each. 
6. The `rownr` variable is created, which indicates what number the survey is for each participant (i.e. first, second). This variable indexes all surveys the participant received, regardless of whether/how they responded. 
7. The `surveys_received` and `surveys_responded` variables are created for each participant, indicating the total number of surveys each received and responded to.
8. The `responsecount` and `fastrtcount` variables are created. These index, for each survey, how many questions were answered, and how many response times were below the rt.min threshold, respectively. 
9. If `rt.trim` is set to TRUE, surveys containing more than `rt.threshold` (.5, or 50%, by default) responses that are below the `rt.min` threshold (500ms by default) are replaced with missing values, and `has_answers` is set to 0 for these surveys. The number of surveys removed is printed when the function is run, and is also saved in the `surveys_removed` variable for each participant, so that problematic participants can be traced. 
10. Additionally, if `rt.trim` is set to TRUE, remaining responses below the `rt.min` threshold are replaced with missing data, as are the corresponding reaction times. The number of responses removed is printed when the function is run. 
11. The `datanr` variable is calculated. This is similar to `rownr`, but indexes only surveys where has_answers is 1. This can be used to create lagged variables such that all available data is used, though note this may cause the time interval between lagged responses to vary considerably. 
12. Various date and time variables are calculated from `delivered`, the timestamp from the participant's phone indicating when the survey was received. 
    1. First, `datedlv` and `timedlv` are calculated - these index the date and the time (in 24 hour time) that the participant began responding.
    2. The `interval` variable is calculated. This represents the time (in hours) since the previous survey. It may prove useful both in ensuring that prompts were delivered at the correct time intervals, but also as a moderator of time-lagged effects.
    3. The `daynr` variable is calculated. This represents, for each participant, which 'day' of the survey this is for them, beginning from 1 for ease of interpretation (though you will often want to subtract 1 from this to ensure the baseline is 0 for analytic purposes)
    4. The `day_of_week` and `weekend` variables are created. These label the day of the week (e.g. Mon, Sun) and whether or not it was a weekend, respectively. 
    5. The `survey_start` and `day_start` variables are calculated. These are simply the date and time the participant received their first survey of the study, or of the date, respectively. These are then used to calculate the `minutes_since_survey_start` and `minutes_since_day_start` variables for each response (these are both specific to each participant, and may be useful for measuring diurnal trends or fatigue effects separate to the actual date or time). 
