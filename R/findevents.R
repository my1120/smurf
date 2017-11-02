#' Find events
#'
#' This function finds events (e.g. heat waves or ozone events). The approach is to find 
#' consecutive days when the exposure variable of interest are above a thresholds. So the best
#' situation of using this function to find events is that the exposure variable has a 
#' measured value on each day. These consecutive days are called "events". 
#' 
#' 
#' @param date A vector of dates. 
#' @param x The exposure variable (e.g. temperature for heat waves or ozone for ozone events)
#' @param xmint A n by 2 matrix. n is the same lenghth of unique combination of ID variables 
#'    defined in \code{by}. The 2 columns are \code{byid} and \code{xmin}. \code{xmin} is 
#'    the minimum value that \code{x} must be for at least \code{mindays} to qualify as an event.
#' @param mindays The minimum number of consecutive days that qualify as an event.
#' @param by A vector of IDs or a matrix with columns as the ID variables. The events will be 
#'    found separately within each unique combination of ID variables. This is optional.
#'    
#' @return Returns a data.table with the columns for \code{by, x, date} as well as the following new 
#'    variables.
#' @return \code{above} An indicator of the (x > xmin) ("1" means the exposure variable of that day
#' is above the threshold, "0" means the exposure variable is below the threshold.)
#' @return \code{above_fromstart} The number of consecutive days that have exceeded \code{xmin} 
#' through the current date.
#' @return \code{above_toend} The number of remaining days that are above \code{xmin}.
#' @return \code{length} The total number of consecutive days that are above xmin.
#' @return \code{event} A logical variable indicating whether a day is part of an event.  
#'    
#' @author Ander Wilson
#' 
#' @importFrom data.table :=
#' 
#' @examples  
#' \dontrun{
#' data("chicagoNMMAPS")
#' chic <- chicagoNMMAPS
#' 
#' }
#' 
#' @export
findevents <- function(date, x, xmint, mindays = 2, by){
  
  if(missing(by)) {
    by <- NA
    bydt <- data.table::data.table(by = by, byid = rep(1, length(date)))
  } else {
    by <- data.table::data.table(by)
    data.table::setkeyv(by, names(by))
    bydt <- unique(by)
    data.table::setkeyv(bydt, names(bydt))
    bydt[ , byid := 1:nrow(bydt)]
    bydt <- bydt[by]
  }
  
  dat <- data.table::data.table(date = date, x = x, byid = bydt$byid)
  dat <- dplyr::left_join(dat, xmint, by = "byid")
  dat <- data.table(dat)
  dat[ , above := 1 * (x > xmin)]
  dat[ , above_fromstart := above]
  dat[ , above_toend := above]
  dat <- unique(dat)
  
  dat[, xmin := NULL]
  data.table::setkeyv(dat, c("byid", "date"))
  
  kg <- TRUE
  i <- 1
  while(kg){
    #days since begining
    dt <- dat[ , list(byid, date = date + i, temp = 1 * (above > 0))]
    data.table::setkeyv(dt, c("byid", "date"))
    dat <- dt[dat]
    dat[!is.na(above_fromstart) & !is.na(temp) & 
          above_fromstart == i, above_fromstart := (above_fromstart > 0) * (above_fromstart + temp)]
    dat[ , temp := NULL]
    
    #days until end
    dt <- dat[ , list(byid, date = date - i, temp = 1 * (above > 0))]
    data.table::setkeyv(dt, c("byid", "date"))
    dat <- dt[dat]
    dat[!is.na(above_toend) & !is.na(temp) & 
          above_toend == i, above_toend := (above_toend > 0) * (above_toend + temp)]
    dat[ , temp := NULL]

    
    i <- i + 1
    if(nrow(dat[above_fromstart == i]) == 0)  kg <- FALSE
  }
  
  dat[ , length := pmax(above_toend + above_fromstart - 1, 0)]
  
  dat[ , event := length >= mindays]
  data.table::setkeyv(bydt, "byid")
  dat <- unique(bydt)[dat]
  dat[ , byid:=NULL]
  
  return(dat)
}


