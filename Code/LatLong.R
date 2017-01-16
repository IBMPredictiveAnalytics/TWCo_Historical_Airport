is.LatitudeLongitude <- function(check.DF, check.latitude, check.longitude) {
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
  
  
  
  print (check.DF[, check.latitude])
  print (check.DF[, check.longitude])
  check.DF <- subset(check.DF, grepl(kLatitudePattern, check.DF[, check.latitude], perl = TRUE) &&
              grepl(kLongitudePattern, check.DF[, check.longitude], perl = TRUE))
     
  return(check.DF)
}
