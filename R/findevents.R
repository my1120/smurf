
#-------------------------------------------------------------------------------------------------
#' @title Find events
#'
#' @description This function finds events (e.g. heat waves or ozone events). The approach is to find consecutive days that are above a thresholds.
#' @param date A vector of dates. 
#' @param x The exposure variable (e.g. temperature for heat waves or ozone for ozone events)
#' @param minx The minimum value that x must be for at least mindays to qualify as an event.
#' @param mindays The minimum number of consecutive days that qualify as an event.
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be found separately within each unique combination of id variables. This is optional.
#' @value Returns a data.table with the columns for by, x, date as well as the following new variables: above and indivator the x>xmin; above_fromstart number of consecutive days that have exceeded xmin through the current date; above_toend the number of remaining days that are above xmin; length the total number of consecutive days that are above xmin; event a logical indivatring a day is part of an event.  
#' @author Ander Wilson
#' @imports data.table
#' @export
findevents <- function(date,x,minx,mindays=2, by){
  
  by <- data.table(by)
  setkeyv(by, names(by))
  bydt <- unique(by)
  setkeyv(bydt, names(bydt))
  bydt[,byid:=1:nrow(bydt)]
  bydt <- bydt[by]
  
  if(missing(by)) by <- NA
  dat <- data.table(date=date,x=x,byid=bydt$byid)
  setkeyv(dat,c("byid","date"))
  dat[,above:=1*(x>xmin)]
  dat[,above_fromstart:=above]
  dat[,above_toend:=above]
  
  kg <- TRUE
  i <- 1
  while(kg){
    #days since begining
    dt <- dat[,list(byid,date=date+i,temp=1*(above>0))]
    setkeyv(dt,c("byid","date"))
    dat <- dt[dat]
    dat[!is.na(above_fromstart) & !is.na(temp) & above==i,above_fromstart:=(above_fromstart>0)*(above_fromstart+temp)]
    dat[,temp:=NULL]
    
    #days until end
    dt <- dat[,list(byid,date=date-i,temp=1*(above>0))]
    setkeyv(dt,c("byid","date"))
    dat <- dt[dat]
    dat[!is.na(above_toend) & !is.na(temp) & above==i,above_toend:=(above_toend>0)*(above_toend+temp)]
    dat[,temp:=NULL]

    
    i <- i+1
    if(nrow(dat[above==i])==0)  kg <- FALSE
  }
  
  dat[,length:= pmax(above_toend+above_fromstart-1,0)]
  dat[length>0]
  
  dat[,event:=length>mindays]
  tables()
  setkeyv(bydt,"by")
  dat <- bydt[dat]
  dat[,byid:=NULL]
  
  return(dat)
}


