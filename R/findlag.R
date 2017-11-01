#' Create lag variables
#'
#' This function finds non-event days that are near events (e.g. the days before or after heat waves or 
#' ozone events). The days are potentially excluded as potential control days when using matching 
#' approachs.
#' 
#' @param date A vector of dates. 
#' @param x The variable to be lagged.
#' @param af_lags The number of days following event days.
#' @param be_lags The number of days before event days.
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be found 
#'    separately within each unique combination of id variables. This is optional.
#'    
#' @return A \code{data.table} object with lagged exposures as well as the date and by variables.
#' 
#' @author Ander Wilson
#' 
#' @importFrom data.table :=
#' 
#' @export
findlag <- function(date, x, af_lags = 1, be_lags = 0, by){
  
  if(missing(by)){
    by <- NA
  }else{
    by <- data.table::data.table(by)
    data.table::setkeyv(by, names(by))
    bydt <- unique(by)
    data.table::setkeyv(bydt, names(bydt))
    bydt[, byid := 1:nrow(bydt)]
    bydt <- bydt[by]
  }
  
  dat <- data.table::data.table(date = date, xlag0 = x,  byid = bydt$byid)
  data.table::setkeyv(dat,c("byid","date"))
  dat <- unique(dat)
  
  dat2 <- data.table::copy(dat)
  data.table::setnames(dat2, "xlag0", "xtemp")
  
  # for i in 1:K makes lags up to K days.
  if(af_lags>0){
    for(i in 1:af_lags){
      dat2[, date := date + 1]
      data.table::setkeyv(dat2, c("byid", "date"))
      dat <- dat2[dat]
      data.table::setnames(dat, "xtemp", paste0("xlag", i))
    }
  }
  
  if(be_lags>0){
    for(i in 1:be_lags){
      dat2[, date := date - 1]
      data.table::setkeyv(dat2, c("byid", "date"))
      dat <- dat2[dat]
      data.table::setnames(dat, "xtemp", paste0("xlag", "-", i))
    }
  }
  
  
  data.table::setkeyv(bydt, "byid")
  dat <- unique(bydt)[dat]
  dat[, byid := NULL]
  
  return(dat)
}

