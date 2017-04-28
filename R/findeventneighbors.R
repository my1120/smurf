#' Find event neighbors
#'
#' This function finds non-event days that are near events (e.g. the day after a heat 
#' waves or ozone events). The days are potentially excluded as potential control days when 
#' using matching approachs.
#'    
#' @param date A vector of dates. 
#' @param event A logical indicating if a day was an event.
#' @param days The number of days that must separate a given day from an event.
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be 
#'    found separately within each unique combination of id variables. This is optional.
#'    
#' @return Returns a vector of logicals that indivates which days are not events and not with in 
#'    days (number provided) of another event.
#'    
#' @author Ander Wilson
#' 
#' @importFrom data.table :=
#' 
#' @export
findeventneighbors <- function(date, event, days = 0, by = NULL){
  
  if(is.null(by)) by <- rep(1, length(date))
  by <- data.table::data.table(by)
  data.table::setkeyv(by, names(by))
  bydt <- unique(by)
  data.table::setkeyv(bydt, names(bydt))
  bydt[ , byid := 1:nrow(bydt)]
  bydt <- bydt[by]
  
  dat <- data.table::data.table(date = date, event = event, byid = bydt$byid)
  data.table::setkeyv(dat,c("byid","date"))
  
  #find days that are within buffer days of a case day
  #these will be excluded
  noteligibledays <- dat[event == TRUE, list(byid, date)]
  data.table::setkeyv(noteligibledays, c("byid", "date"))
  if(days > 0){
    for(i in 1:days){
      temp <- dat[event == TRUE, list(byid, date)]
      temp[ , date := date + i]
      data.table::setkeyv(temp, c("byid", "date"))
      noteligibledays <- merge(noteligibledays, temp, all = TRUE)
      
      temp <- dat[event == TRUE, list(byid, date)]
      temp[ , date := date - i]
      data.table::setkeyv(temp, c("byid", "date"))
      noteligibledays <- merge(noteligibledays, temp, all = TRUE)
    }
  }
  
  #indicator flag
  noteligibledays[ , eligible := FALSE]
  dat <- noteligibledays[dat]
  dat[event == FALSE & is.na(eligible), eligible := TRUE]
  
  data.table::setkeyv(bydt, "byid")
  dat <- unique(bydt)[dat]
  dat[ , byid := NULL]
  dat[ , casecontrol := ifelse(event, 1, ifelse(eligible, 0, NA))]
  
  return(dat)
}




