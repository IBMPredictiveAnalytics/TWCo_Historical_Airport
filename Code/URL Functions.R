
# -----------------------------------------------------------------------------
# -- NODE-SPECIFIC FUNCTION DEFINITION SECTION
# -----------------------------------------------------------------------------
retrieveDataFromTWC_OneMonth <- function(url) {
  resultContext <- content(GET(url))
  processOneRecord <- function(observations){
    return(lapply(observations,processEachCell))
  }
  
  processEachCell <- function(cell){
    if(is.null(cell)) return(NA)
    else return(cell)
  }
  if(is.null(resultContext$observations)) {
    print(toString(resultContext))
  }  
  resultData <- ldply (lapply(resultContext$observations,processOneRecord), data.frame)
  if(input_requestType == "postalcode") {
    return(data.frame(latitude=resultContext$metadata$location_id,
                      resultData))
  } else {
    return(data.frame(latitude=resultContext$metadata$latitude, 
                      longitude=resultContext$metadata$longitude,
                      resultData))
  }
}

# _____________________________________________________________________________

retrieveDataFromTWC <- function(modelerDataRecord) {
  generateUrl <- function(sLocationUrl, sDateRange) {
    sUrl <- paste(kBaseURL, 
                  sLocationUrl, 
                  kHistoricalUrl,
                  "&units=", input_units,
                  "&apiKey=", input_apikey,
                  sDateRange,
                  sep="")
    return(sUrl)
  }
  
  sLatURL <- as.character(modelerDataRecord[input_latitude])
  sLongURL <- as.character(modelerDataRecord[input_longitude])
# DEBUG   print(sLatURL)  # DB
# DEBUG   print(sLongURL)  # DB
  
  if (input_requestType == "geocode") {
    sLocationUrl = paste("geocode/", 
                         sLatURL, "/", 
                         sLongURL, 
                         sep="")
  } else if (input_requestType == "postalcode") {
    sLocationUrl = paste("location/", 
                         modelerDataRecord[input_postalcode], sep="")
  }
  
  if (input_date_inputtype == "is_variable") {
    sInputStartDate <- modelerDataRecord[input_startDate]
    if (input_endDate == "") {
      sInputEndDate <- modelerDataRecord[input_startDate]
    } else {
      sInputEndDate <- modelerDataRecord[input_endDate]
    }
  } else {
    sInputStartDate <- input_startDate_text
    if (input_endDate_text == "") {
      sInputEndDate <- input_startDate_text
    } else {
      sInputEndDate <- input_endDate_text
    }
  }
  
# DEBUG   print(sInputStartDate)  # DB
# DEBUG   print(sInputEndDate)  # DB

  sStartDate <- as.Date(sInputStartDate, "%Y%m%d")
  sEndDate <- as.Date(sInputEndDate, "%Y%m%d")
  
# DEBUG  print(sStartDate)  # DB
# DEBUG  print(sEndDate)  # DB

 resultData <- data.frame()
 for(day in format(seq(sStartDate,sEndDate,30), "%Y%m%d")) {
   dayNumber <- as.Date(day, "%Y%m%d")
   sMonthStartDate <- day#format(day, "%Y%m%d")
   if (dayNumber + 30 > sEndDate )
     sMonthEndDate <- format(sEndDate, "%Y%m%d")
   else
     sMonthEndDate <- format(dayNumber + 30, "%Y%m%d")
   sDateRange = paste("&startDate=", 
                      sMonthStartDate, "&endDate=", 
                      sMonthEndDate, sep="")
   sRequestURL <- generateUrl(sLocationUrl, sDateRange)
   print(sRequestURL)  # DB
   resultData <- rbind(resultData, retrieveDataFromTWC_OneMonth(sRequestURL))
# DEBUG   print(nrow(resultData)) # DB
 }
# DEBUG  print(nrow(resultData))  # DB
 return(data.frame(resultData))
}