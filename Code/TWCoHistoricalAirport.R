# **********************************Header*************************************
# *********GENERAL*********
# OBJECT NAME: TWCoHistoricalAirport 
# VERSION: 2.50
# OBJECT TYPE: R Custom Node
# CATEGORY: Utility
# SUBCATEGORY: Weather
# CREATED BY:   YU WENPEI	
# DATE:         11/15/2016
# MODIFIED BY:  GRANT CASE
# DATE:         01/07/2017
# DESCRIPTION:
# C:\Users\IBM_ADMIN\Box Sync\My Work Folder\IBM SPSS Predictive Extensions\TWCo_Historical_Airport\
# 
# 
# 
# 
# *********VARIABLES*********
# NAME	      		TYPE		LOCAL/PASSED	DEFAULT	
# DESCRIPTION		
# ---------------------------------------------------------------
#
# 
# 
# *********DEPENDENCIES*********
# PACKAGES:
# httr
# plyr
# 
# 
# 
# *********OUTPUT*********
# 
# 
# *********MODIFICATION LOG*********
# DATE        INITIALS MODIFICATION
# 11/15/2016  YW
# Created  
# 01/07/2017  GSC      
# Updated to ensure correct results by aligning measures datatypes also
# add comments, sections, and headers for easier understanding
# 
# *********TODO LOG*********
# TODO(Grant Case): Add logic to ensure correct format passed to dates.
# TODO(Grant Case): Add logic to check for spaces in column names (or at least
# point it out to the user)
# TODO(Grant Case): Add parallelism code

#
#
#
# *********HEADER CONVENTIONS*********
# DO NOT GO PAST 80 CHARACTERS
# TO DEBUG, REPLACE ALL "# DEBUG "
# TO TURN DEBUG OFF, FIND ALL LINES WITH "  # DB" AND "# DEBUG " AT BEGINNING
# **********************************Header*************************************


# -----------------------------------------------------------------------------
# -- PACKAGE DECLARATION SECTION
# -----------------------------------------------------------------------------
# ********************Create test for package existence************************
packages <- function(x) {
  x <- as.character(match.call()[[2]])
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}
# ****************************Packages to Use**********************************
packages(httr)
packages(plyr)
packages(lubridate)


# DEBUG save.image(file="C:/Users/IBM_ADMIN/Box Sync/My Work Folder/IBM SPSS Predictive Extensions/TWCo_Historical_Airport/Code/Debug/TWCOHistoricalAirport.RData", safe=FALSE) # DB
# DEBUG print("End Install Package") # DB


# -----------------------------------------------------------------------------
# -- CUSTOM DIALOG VARIABLE DECLARATION SECTION
# -----------------------------------------------------------------------------
input_apikey         <- "%%item_apikey%%"
input_requestType    <- %%item_locationtype%%  # requestType has two value c("geocode","stationId", "postalcode")
input_latitude       <- "%%item_lat%%"
input_longitude      <- "%%item_lon%%"
input_postalcode     <- "%%item_postalcode%%"
input_startDate      <- "%%item_startdate%%"
input_endDate        <- "%%item_enddate%%" 
input_units          <- "%%item_unit%%"
  # e = English units
  # m = Metric units
  # h = Hybrid units (UK)
  # s = Metric SI units (not available for all APIs)

input_date_inputtype <- %%item_date_group%%
  # is_variable
  # is_textinput

input_startDate_text <- "%%item_startdate_input%%"
input_endDate_text   <- "%%item_enddate_input%%"


dialog.option.locationtype    <- input_requestType
dialog.option.dateinput       <- input_date_inputtype

dialog.column.apikey          <- input_apikey
dialog.column.units           <- input_units
dialog.column.latitude.name   <- make.names(input_latitude)
dialog.column.longitude.name  <- make.names(input_longitude)
dialog.column.postalcode.name <- make.names(input_postalcode)
dialog.column.startdate.name  <- make.names(input_startDate)
dialog.column.enddate.name    <- make.names(input_endDate)
dialog.column.startdate.value <- input_startDate_text
dialog.column.enddate.value   <- input_endDate_text


date.input.type <- dialog.option.dateinput 
location.type <- dialog.option.locationtype




# -----------------------------------------------------------------------------
# -- CONSTANT SET SECTION
# -----------------------------------------------------------------------------

kHistoricalUrl              <- "/observations/historical.json?language=en-US"
kErrorOutputPrefix          <- "TWCoHistoricalAirport Node Error: "
kDebugImageLocation         <- "C:/Users/IBM_ADMIN/Box Sync/My Work Folder/IBM SPSS Predictive Extensions/TWCo_Historical_Airport/Code/Debug/TWCOHistoricalAirport.RData"
kDebugImageLocation2        <- "C:/Users/IBM_ADMIN/Box Sync/My Work Folder/IBM SPSS Predictive Extensions/TWCo_Historical_Airport/Code/Debug/modelerDebug.RData"
kmodDLatitudeColNameConstant   <- "latitude"
kmodDLongitudeColNameConstant  <- "longitude"
kmodDPostalCodeColNameConstant <- "postal.code"
kmodDStationIDColNameConstant  <- "station.id"
kmodDStartDateColNameConstant  <- "start.date"
kmodDEndDateColNameConstant    <- "end.date"
kmodDLocationURLColNameConstant   <- "location.type"
kAPIParameter <- dialog.option.locationtype 
kAPI <- "History-Site"
kBaseURLColNameConstant <- "Base.URL"


kmodDLocationURLConstant    <- if (dialog.option.locationtype == "geocode") "geocode" else "location"                                   

save.image(file=kDebugImageLocation, safe=FALSE) # DB
print("End Custom Dialog") # DB


URL.Data <- data.frame(kmodDLocationURLConstant)
names(URL.Data)[ncol(URL.Data)] <- kmodDLocationURLColNameConstant


RetrieveTWCoBaseURL <- function(API.Parameter, API) {
  if (API.Parameter == "geocode" && API == "History-Site") {
    BaseURL = "https://api.weather.com/v1/geocode/<latitude>/<longitude>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>"
  } else if (API.Parameter == "postalcode" && API == "History-Site") {
    BaseURL = "https://api.weather.com/v1/location/<postal.code>:4:<country>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>"
  } else if (API.Parameter == "stationid" && API == "History-Site") {
    BaseURL = "https://api.weather.com/v1/location/<station.id>:4:<country>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>"
  } else {
    BaseURL = ""
  }
  
  return(BaseURL)
}

# Psudo-Code
# Bring in Inputs
# Set Constants
# Set the Base URL
# Create a URL data frame



CreateAppendDFColumnData <- function(column, new.column.name, targetDF, sourceDF) {

  print ("Step 1")
  quotedcolumn = quote(column)

  print ("Step 2")

  if (missing(targetDF)) {
    targetDF <- data.frame(column)
  } else if (missing(sourceDF)) {
    targetDF <- data.frame(targetDF, column)
  } else {
    targetDF <- data.frame(targetDF, sourceDF[,column])
  }
  
  
  names(targetDF)[ncol(targetDF)] <- new.column.name
  return(targetDF)

}

    sLocationUrl = paste("geocode/", 
                         sLatURL, "/", 
                         sLongURL, 
                         sep="")
  } else if (input_requestType == "postalcode")


# -----------------------------------------------------------------------------
# -- BUILD URL DATA FRAME
# -----------------------------------------------------------------------------
URL.Data <- CreateAppendDFColumnData(kBaseURL,kBaseURLColNameConstant)

if (location.type == "geocode") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.latitude.name, kmodDLatitudeColNameConstant, URL.Data, modelerData)
  URL.Data <- CreateAppendDFColumnData(dialog.column.longitude.name, kmodDLongitudeColNameConstant, URL.Data, modelerData)
} else if (location.type == "postalcode") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.postalcode.name, kmodPostalCodeColNameConstant, URL.Data, modelerData)
} else if (location.type == "stationID") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.postalcode.name, kmodPostalCodeColNameConstant, URL.Data, modelerData)  
} else {
  URL.Data <- URL.Data
}

if (date.input.type == "is_variable") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.startdate.name, kmodDStartDateColNameConstant, URL.Data, modelerData)
  URL.Data <- CreateAppendDFColumnData(dialog.column.enddate.name, kmodDEndDateColNameConstant, URL.Data, modelerData)
} else if (date.input.type == "is_textinput") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.startdate.value, kmodDStartDateColNameConstant, URL.Data)
  URL.Data <- CreateAppendDFColumnData(dialog.column.enddate.value, kmodDEndDateColNameConstant, URL.Data)
} else {
  URL.Data <- URL.Data
}

URL.Data <- unique(URL.Data)


# DEBUG modelerData  # DB
# DEBUG print("End Constants and Variables")  # DB


# -----------------------------------------------------------------------------
# -- ERROR HANDLING FUNCTIONS SECTION
# -----------------------------------------------------------------------------
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

# Defines a custom stop function so that issues can be raised to the end user
CustomStop <- function(subclass, message, call = sys.call(-1), ...) {
  c <- condition(c(subclass, "error"), message, call = call, ...)

# DEBUG   save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG   print("DEBUG Image Save End") # DB

  paste(kErrorOutputPrefix, c)

  stop(c)
}

# _____________________________________________________________________________

is.TWCoDate  <- function(check.start, check.end, check.historyonly = TRUE) {
  # **********************************Header***********************************
  # FUNCTION NAME: is.TWCoDate
  # DESCRIPTION: This function accepts a latitude and longitude and will return
  # whether the values contained are legal latitude and longitudes
  # 
  #
  # Args:
  #   check.start: Start Date
  #   check.end: End Date
  #   check.historyonly: (Boolean: TRUE) - if TRUE will only check for historic 
  #   dates. For weather forecast dates in the future you would choose FALSE.
  #
  # Returns:
  #   ValidTWCoDate: Boolean value - 
  #     True = Legal latitude and longitude
  #     False = Not a valid date for The Weather Company data
  # **********************************Header***********************************
  
  # -----------------------------------------------------------------------------
  # -- VARIABLE SET SECTION
  # -----------------------------------------------------------------------------
  # Earliest date of The Weather Company's observations - January 1931
  earliest.date <- ymd(19310101)
  
  # If check.historyonly is true it will do the latest day + 1 otherwise + 7 days
  if (check.historyonly) {
    latest.date <- today() + 1
  } else {
    latest.date <- today() + 7
  }
  
  # Convert Start and End dates to date elements. If they are not legal elements
  # ymd will return NA
  check.start <- ymd(check.start)
  check.end <- ymd(check.end)

  if (is.na(check.start) || is.na(check.end))
    CustomStop("Invalid_Date", "Either the Start or End Dates are not a legal date")
    
  if (check.start > check.end) 
    CustomStop("Invalid_Start_End", "Input of Start Date must Be the Same or less than End Date")
    
  if (check.start < earliest.date || check.end < earliest.date)
    CustomStop("Invalid_Too_Early", "Input of Start or End Date before weather records began (Jan 1931)")
    
  if (check.start > latest.date || check.end > latest.date)
    CustomStop("Invalid_Future_Date", "Input of Future Date, No weather records yet")
  return(check.start, check.end)
}

# _____________________________________________________________________________

is.LatitudeLongitude <- function(check.latitude, check.longitude) {
  # **********************************Header***********************************
  # FUNCTION NAME: is.latitudelongitude
  # DESCRIPTION: This function accepts a latitude and longitude and will return
  # whether the values contained are legal latitude and longitudes
  # 
  #
  # Args:
  #   check.latitude: Latitude column
  #   check.longitude: Longitude column 
  # Returns:
  #   ValueLatLong: Boolean value - 
  #     True = Legal latitude and longitude
  #     False = Not a valid latitude and longitude
  # **********************************Header***********************************
  
  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------

  # Regular Expression patterns for both Latitude and Longitude
  # slightly modified version found at this link. 
  # http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinate  
  kLatitudePattern  <- "^(\\+|-)?(?:90(?:(?:\\.0{1,9})?)|(?:[0-9]|[1-8][0-9])(?:(?:\\.[0-9]{1,9})?))$"
  kLongitudePattern <- "^(\\+|-)?(?:180(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-9][0-9]|1[0-7][0-9])(?:(?:\\.[0-9]{1,6})?))$"
  
  
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  
  # Check both Latitude and Longitude against the patterns and if both are 
  # legal Latitude and Longitudes return ValidLatLong = TRUE otherwise its FALSE
  # to either TRUE or FALSE based on 
  if (grepl(kLatitudePattern, check.latitude, perl = TRUE) &&
      grepl(kLongitudePattern, check.longitude, perl = TRUE)) {
    ValidLatLong = TRUE
  } else {
    ValidLatLong = FALSE
  }
  return(ValidLatLong)
}



# ModelerDataCheck <- function(x) {
#   if (!is.numeric(x))
#     CustomStop("invalid_class", "my_log() needs numeric input")
#   if (any(x < 0))
#     CustomStop("invalid_value", "my_log() needs positive inputs")
#   log(x)
# }

# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG print("End Error Handling Functions") # DB




modelerDebug <- modelerData
save.image(file=kDebugImageLocation2, safe=FALSE) # DB


# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
print("End Node-Specific Functions") # DB

# -----------------------------------------------------------------------------
# -- modelerData EXECUTION SECTION
# -----------------------------------------------------------------------------
# Retrieve each row from modelerData, run the retrieveDataFromTWC, and 
# return the data frame 



tryCatch(
  is.TWCoDate (input_startDate_text, input_endDate_text),
    Invalid_Date = function(c) paste(kErrorOutputPrefix, "Either the Start or End Dates are not legal"),
    Invalid_Start_End = function(c) paste(kErrorOutputPrefix, "Input of Start Date must be the same or less than End Date"),
    Invalid_Too_Early = function(c) paste(kErrorOutputPrefix, "Input of Start or End Date before weather records began - Jan 1931"),
    Invalid_Future_Date = function(c) paste(kErrorOutputPrefix, "Input of Future Date, No weather records yet")
)

tryCatch(
  modelerData <- ldply(apply(modelerData, 1, FUN = retrieveDataFromTWC))

)


# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG print("End modelerData Execution Section") # DB





# -----------------------------------------------------------------------------
# -- modelerDataModel UPDATE SECTION
# -----------------------------------------------------------------------------
# TODO(Grant Case): Function to build all at once
# TODO(Grant Case): Function to check for names that don't match due to <spaces>

MakeLegalColumnNamesModelerDataModel <- function(modDM) {
  # **********************************Header***********************************
  # FUNCTION NAME: MakeLegalColumnNamesModelerDataModel
  # DESCRIPTION: This function accepts a data frame in the style of
  # modelerDataModel and will return a data frame will legal R field names that
  # in theory should match modelerData.
  #
  # Args:
  #   modDM: (data frame) A data frame that represents modelerDataModel
  #
  # Returns:
  # An updated data frame with field names that are legal which in theory
  # should match modelerData.

  # **********************************Header***********************************

  # Transpose the modelerDataModel so that we can update fieldName
  modDM.transpose                <- as.data.frame(t(modDM))

  #Split the fieldName column and others from the transposed set modelerDataModel
  modDM.transpose.cols.others    <- as.data.frame(data.frame(modDM.transpose[2:6]))
  modDM.transpose.cols.fieldName <- as.data.frame(data.frame(modDM.transpose[1:1]))

  # Run the make.names function to make the column names legal (in theory) this 
  # should match what occurred when the original column names were past to 
  # modelerData. Doing this ensures that modelerData names and modelerDataModel names
  # are the same.
  modDM.transpose.cols.fieldName <- data.frame(make.names(modDM.transpose.cols.fieldName$fieldName))
  names(modDM.transpose.cols.fieldName)[1] <- "fieldName"

  modDM.transpose                <- data.frame(modDM.transpose.cols.fieldName, modDM.transpose.cols.others)
  modDM <- as.data.frame(t(modDM.transpose))
  
  return(modDM)

}


modelerDataModel <- MakeLegalColumnNamesModelerDataModel(modelerDataModel)



valLatitude                  <- c(fieldName="latitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valLongitude                 <- c(fieldName="longitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valLocationId                <- c(fieldName="locationId", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="", fieldRole="")
valKey                       <- c(fieldName="key", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valClass                     <- c(fieldName="class", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valExpire_time_gmt           <- c(fieldName="expire_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
valObs_id                    <- c(fieldName="obs_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valObs_name                  <- c(fieldName="obs_name", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valValid_time_gmt            <- c(fieldName="valid_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
valDay_ind                   <- c(fieldName="day_ind", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valTemp                      <- c(fieldName="temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWx_icon                   <- c(fieldName="wx_icon", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valIcon_extd                 <- c(fieldName="icon_extd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWx_phrase                 <- c(fieldName="wx_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valPressure_tend             <- c(fieldName="pressure_tend", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valPressure_desc             <- c(fieldName="pressure_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valDewPt                     <- c(fieldName="dewPt", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valHeat_index                <- c(fieldName="heat_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valRh                        <- c(fieldName="rh", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valPressure                  <- c(fieldName="pressure", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valVis                       <- c(fieldName="vis", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWc                        <- c(fieldName="wc", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWdir                      <- c(fieldName="wdir", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWdir_cardinal             <- c(fieldName="wdir_cardinal", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valGust                      <- c(fieldName="gust", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valWspd                      <- c(fieldName="wspd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valMax_temp                  <- c(fieldName="max_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valMin_temp                  <- c(fieldName="min_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valPrecip_total              <- c(fieldName="precip_total", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valPrecip_hrly               <- c(fieldName="precip_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valSnow_hrly                 <- c(fieldName="snow_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valUv_desc                   <- c(fieldName="uv_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valFeels_like                <- c(fieldName="feels_like", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valUv_index                  <- c(fieldName="uv_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
valQualifier                 <- c(fieldName="qualifier", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valQualifier_svrty           <- c(fieldName="qualifier_svrty", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valBlunt_phrase              <- c(fieldName="blunt_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valTerse_phrase              <- c(fieldName="terse_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
valClds                      <- c(fieldName="clds", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="", fieldRole="")
valWater_temp                <- c(fieldName="water_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valPrimary_wave_period       <- c(fieldName="primary_wave_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valPrimary_wave_height       <- c(fieldName="primary_wave_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valPrimary_swell_period      <- c(fieldName="primary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valPrimary_swell_height      <- c(fieldName="primary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valPrimary_swell_direction   <- c(fieldName="primary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valSecondary_swell_period    <- c(fieldName="secondary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valSecondary_swell_height    <- c(fieldName="secondary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
valSecondary_swell_direction <- c(fieldName="secondary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
modelerDataModel <- data.frame(valKey, valClass, valExpire_time_gmt, valObs_id, valObs_name, 
                               valValid_time_gmt, valDay_ind, valTemp, valWx_icon, valIcon_extd, 
                               valWx_phrase, valPressure_tend, valPressure_desc, valDewPt, valHeat_index, 
                               valRh, valPressure, valVis, valWc, valWdir, valWdir_cardinal, valGust, 
                               valWspd, valMax_temp, valMin_temp, valPrecip_total, valPrecip_hrly, 
                               valSnow_hrly, valUv_desc, valFeels_like, valUv_index, valQualifier, 
                               valQualifier_svrty, valBlunt_phrase, valTerse_phrase, valClds, 
                               valWater_temp, valPrimary_wave_period, valPrimary_wave_height, 
                               valPrimary_swell_period, valPrimary_swell_height, valPrimary_swell_direction, 
                               valSecondary_swell_period, valSecondary_swell_height, valSecondary_swell_direction)
if (input_requestType == "postalcode") {
  modelerDataModel <- data.frame(valLocationId, modelerDataModel)
} else {
  modelerDataModel <- data.frame(valLatitude, valLongitude, modelerDataModel)
}

# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG print("End modelerDataModel Update Section") # DB


# -----------------------------------------------------------------------------
# -- TESTING SECTION
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# -- CLEAN UP SECTION
# -----------------------------------------------------------------------------


