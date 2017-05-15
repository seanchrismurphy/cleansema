#' Clean sema data
#' 
#' The cleansema package was built to rapidly take folders of raw data from the SEMA smartphone application and turn
#' them into a single, clean dataframe, while also calculating variables that will commonly prove useful for analysis.
#'
#' @author Sean C Murphy, \email{seanchrismurphy@gmail.com}
#'
#' The key function you'll want to use is the \code{\link{clean_sema}} function.
#' @importFrom plyr join
#' @importFrom stringr str_match
#' @import dplyr
#' @importFrom lubridate dmy_hms date hour wday minute second
#' @import reshape2
"_PACKAGE"