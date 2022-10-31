#' @title Decimal minute to degrees decimal converter
#' 
#' @description Takes input as decimal minutes coordinates and returns a data frame formatted in degrees decimal
#' @param longitude Data frame column or character vector with longitude coordinates in decimal minutes 
#' @param latitude Data frame column or character vector with latitude coordinates in decimal minutes
#' @keywords coordinates convert
#' @export



converter <- function(longitude, latitude) {
  
##first checks to see if input is already in degrees decimal, returns error if already a numeric vector
  if(class(longitude)=="numeric") {
    sprintf("please make sure your input is in decimal minutes")
  }
  else if(class(latitude)=="numeric") {
    sprintf("please make sure your input is in decimal minutes")
  }
  else{
    longitude <- ((as.numeric(gsub("°.*", "", longitude))) + (as.numeric(gsub(".*°|\\'.*", "", longitude))/60)) * -1 
    
    latitude <- ((as.numeric(gsub("°.*", "", latitude))) + (as.numeric(gsub(".*°|\\'.*", "", longitude))/60)) 
    
    converted <- data.frame(longitude, latitude)
    colnames(converted) <- c("longitude","latitude")
    return(converted)
  }
}
 
  
  