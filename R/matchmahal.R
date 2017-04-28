#' Match with Mahalanobis
#'
#' This function finds matches for the events without a strata sing the Mahalanobis distrance and 
#' nearest neighbor. It is a wrapper for the \code{matchit} function from the \code{MatchIt} package.
#' 
#' @param date A vector of dates. 
#' @param casecontrol A vector where 1 indentifies events, 0 identifies potential controls, and 
#'    \code{NA} represent non-event days that are not eligible to be matched (e.g. missing data, or 
#'    excluded for some other reason).
#' @param matchvars A matrix where the columns represent variables that will be matched on using 
#'    the Mahalanobis distrance and nearest neighbor.
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be found 
#'    separately within each unique combination of id variables. This is optional.
#' @param ratio The number of control units to be matched to each case. The default is 1.  See 
#'    documentation for matchit for more details. 
#' @param datewindow A scalar that limits the potential control days to be within a a particular 
#'    number of days of the range of events. For example, if the events occur on days 100, 101, 
#'    150, 151, 152 of the year and datewindow=7 then control days will have day of years in the 
#'    range of 93 to 159. If missing then all days are eligible.
#' @param ... Additional arguments to be passed to \code{match.it}
#'    
#' @return data A \code{data.table} object of matched cases and controls.
#' @return nn A summary of the number if cases and controls that were matched. See documentation 
#'    for \code{matchit} for more details.
#' @return sum.matched A summary of the quality of each match. See documentation for \code{matchit} 
#'    for more details.
#'    
#' @author Ander Wilson
#' 
#' @seealso MatchIt
#' 
#' @importFrom data.table :=
#' 
#' @export
matchmahal <- function(date,casecontrol,matchvars,by,ratio=1,datewindow,...){
  
  if(missing(by)){
    by <- NA
  }else{
    by <- data.table::data.table(by)
    data.table::setkeyv(by, names(by))
    bydt <- unique(by)
    data.table::setkeyv(bydt, names(bydt))
    bydt[ , byid := 1:nrow(bydt)]
    bydt <- bydt[by]
  }
  
  dat <- data.table::data.table(matchvars)
  dat[ , casecontrol := casecontrol]
  dat[ , byid := bydt[ , (byid)]]
  dat[ , date := date]
  
  form <- stats::formula(paste0("casecontrol ~", 
                                paste(colnames(dat)[-which(colnames(dat) %in% 
                                                             c("casecontrol", "byid", "date"))],
                                      collapse = "+")))
  
  
  matched.sample <- sum.matched <- nn <- data.table()
  for(i in unique(dat[ , (byid)])){
    print(i)
    #make matching data for county
    dati <- dat[byid == i]
    dati <- dati[stats::complete.cases(dati), ]
    
    if(nrow(dati[casecontrol == 1]) > 0){
      #limit to time window
      if(!missing(datewindow)){
        dati <- dati[doy > min(dati[casecontrol == 1, (doy)]) - datewindow & 
                       doy < max(dati[casecontrol == 1, (doy)]) + datewindow]
      }
      
      #matchit
      matchit.fit <- MatchIt::matchit(form, data = dati, distance = "mahalanobis",
                                      method = "nearest", ratio = ratio, ...)
      
      #save match
      matched.sample <- rbind(matched.sample, data.table(MatchIt::match.data(matchit.fit)))
      
      #save statistics on number matched
      temp.nn <- data.table::data.table(matchit.fit$nn)
      temp.nn[ , type := row.names(matchit.fit$nn)]
      temp.nn[ , byid := i]
      nn <- rbind(nn, temp.nn)
      
      #add this
      temp.sum.matched <- data.table::data.table(summary(matchit.fit)$sum.matched)
      temp.sum.matched[ , type := row.names(summary(matchit.fit)$sum.matched)]
      temp.sum.matched[ , byid := i]
      sum.matched <- rbind(sum.matched, temp.sum.matched)
    }
  }
  
  data.table::setkeyv(bydt, "byid") 
  data.table::setkeyv(matched.sample, c("byid", "date")) 
  data.table::setkeyv(nn, "byid") 
  data.table::setkeyv(bydt, "byid") 
  sum.matched <- unique(bydt)[sum.matched]
  nn <- unique(bydt)[nn]
  data <- unique(bydt)[matched.sample]
  sum.matched[ , byid := NULL]
  nn[ , byid := NULL]
  data[ , byid := NULL]
  
  
  return(list(data = data, nn = nn, sum.matched = sum.matched))
}