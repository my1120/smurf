#' Match
#'
#' Finds matches for the events without a strata and within a time window. The Mahalanobis-metric 
#' can be used to match on other variables as well. This function is a wrapper for the 
#' \code{matchit} function from the \code{MatchIt} package.
#' 
#' @param date A vector of dates. 
#' @param casecontrol A vector where 1 indentifies events, 0 identifies potential controls, and 
#'    \code{NA} represent non-event days that are ineligible as matches (e.g. because of missing data, 
#'    or they are too close to other event days, as determined by \code{findeventneighbors}).
#' @param matchvars A matrix where the columns represent variables that will be matched on using 
#'    the Mahalanobis distance and nearest neighbor. 
#' @param mahdoy A logical indicating if the day of year should be included in the 
#'    Mahalanobis-metric matching within the caliper. The default is FALSE. If TRUE then the 
#'    distance to the treatment day based on day of year will be included with the other matching 
#'    variables.
#' @param caldays The number of days that matched value will be selected from. This ignores year 
#'    but only looks at date and month. For example, if \code{caldays} is set to 7 then the control 
#'    day will be from within 7 days of the day of the year of the event day (inclusive).
#' @param by A vector of IDs or a matrix with columns as the ID variables. The events will be 
#'    found separately within each unique combination of ID variables. For example, if the dataset
#'    includes multiple cities, `by` could be used to ensure that matching is always done within 
#'    city. 
#' @param ratio The number of control days to be matched to each event day. The default is 1.  See 
#'    documentation for \code{matchit} for more details. 
#' @param seed A seed for a random number generator.
#' @param ... Additional arguments to be passed to \code{matchit}
#' 
#' @return data A \code{data.table} object of event days and their matched controls.
#' @return nn A summary of the number of event days and controls in the final matched dataset. 
#'    See documentation for \code{matchit} for more details.
#' @return sum.matched A summary of the quality of each match. See documentation for 
#'    \code{matchit} for more details.
#'    
#' @author Ander Wilson
#' 
#' @seealso MatchIt
#' 
#' @importFrom data.table :=
#' 
#' @export
mtch <- function(date, casecontrol, matchvars = NULL, mahdoy = FALSE, caldays = Inf, by = NULL, 
                 ratio = 1, seed = NULL, ...){
  
  if(!is.null(seed)) set.seed(seed)
  
  if(is.null(by)) by <- rep(1, length(date))
  by <- data.table::data.table(by)
  if(is.null(by)) names(by) <- "group 1"
  data.table::setkeyv(by, names(by))
  bydt <- unique(by)
  data.table::setkeyv(bydt, names(bydt))
  bydt[ , byid := 1:nrow(bydt)]
  bydt <- bydt[by]
  
  if(!is.null(matchvars)){
    dat <- data.table::data.table(matchvars)
  } else {
    dat <- data.table::data.table(casecontrol)
  }
  
  dat[ , casecontrol := casecontrol]
  dat[ , byid := bydt[ , (byid)]]
  dat[ , date := date]
  dat <- unique(dat)
  dat[ , caldoy := as.POSIXlt(dat$date)$yday - 365 / 2]
  
  matched.sample <- sum.matched <- nn <- data.table::data.table()
  
  for(i in unique(dat[ , (byid)])){
    #make matching data for county
    dati <- dat[byid == i]
    dati <- as.data.frame(dati[stats::complete.cases(dati), ])
    
    # If there is at least one event day in the time series for the group, ...
    if(nrow(subset(dati, casecontrol == 1)) > 0){
      row.names(dati) <- as.character(1:nrow(dati))
      #match the sample
        if(is.null(matchvars)){
          matchit.fit <- MatchIt::matchit(casecontrol ~ caldoy, data = dati, method = "nearest",
                                          caliper = caldays / stats::sd(dati$caldoy),
                                          ratio = ratio, ...)
        } else {
          matchit.fit <- MatchIt::matchit(casecontrol ~ caldoy, data = dati, method = "nearest",
                                          caliper = caldays / stats::sd(dati$caldoy), 
                                          mahvars = colnames(dati)[-which(colnames(dati) %in% (c("casecontrol", "byid", "date", "caldoy")))], 
                                          ratio = ratio, ...)
        }
      
      #save match
      matchedout <- data.table::data.table(MatchIt::match.data(matchit.fit))
      matches <- cbind(as.numeric(cbind(matchit.fit$match.matrix, row.names(matchit.fit$match.matrix))),
                       rep(1:nrow(matchit.fit$match.matrix), ncol(matchit.fit$match.matrix) + 1))
      matches <- data.table::data.table(dati[matches[ , 1], ])[ , pairing := matches[ , 2]]
      matches <-  matches[ , list(date, pairing)]
      data.table::setkeyv(matches, "date")
      data.table::setkeyv(matchedout, "date")
      matched.sample <- rbind(matched.sample, matches[matchedout])
      
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
  data.table::setkeyv(sum.matched, "byid") 
  sum.matched <- unique(bydt)[sum.matched]
  nn <- unique(bydt)[nn]
  data <- unique(bydt)[matched.sample]
  sum.matched[ , byid := NULL]
  nn[ , byid := NULL]
  data[ , byid := NULL]
  
  return(list(data = data, nn = nn, sum.matched = sum.matched))
}


