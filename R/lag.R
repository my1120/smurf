

#-------------------------------------------------------------------------------------------------
#' @title Create lag variables
#'
#' @description This function finds non-event days that are near events (e.g. the day after a heat waves or ozone events). The days are potentially excluded as potential control days when using matching approachs.
#' @param date A vector of dates. 
#' @param x The varibale to be lagged
#' @param lags The number of days the variable should be lagged
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be found separately within each unique combination of id variables. This is optional.
#' @return Returns a data.table with lagged exposures as well as the date and by variables.
#' @author Ander Wilson
#' @import data.table
#' @export
lag <- function(date,x,lags=1, by){
  
  
  if(missing(by)){
    by <- NA
  }else{
    by <- data.table(by)
    setkeyv(by, names(by))
    bydt <- unique(by)
    setkeyv(bydt, names(bydt))
    bydt[,byid:=1:nrow(bydt)]
    bydt <- bydt[by]
  }
  
  
  dat <- data.table(date=date,xlag0=x,byid=bydt$byid)
  setkeyv(dat,c("byid","date"))
  dat2 <- copy(dat)
  setnames(dat2,"xlag0","xtemp")
  
  # for i in 1:K makes lags up to K days.
  if(lags>0){
    for(i in 1:lags){
      dat2[,date:=date+1]
      setkeyv(dat2,c("byid","date"))
      dat <- dat2[dat]
      setnames(dat,"xtemp",paste0("xlag",i))
    }
  }
  
  
  setkeyv(bydt,"byid")
  dat <- unique(bydt)[dat]
  dat[,byid:=NULL]
  
  return(dat)
}

