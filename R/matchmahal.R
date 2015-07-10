
#-------------------------------------------------------------------------------------------------
#' @title Match with Mahalanobis
#'
#' @description This function finds matches for the events without a strata sing the Mahalanobis distrance and nearest neighbor. It is a wrapper for the matchit function from the MatchIt package.
#' @param date A vector of dates. 
#' @param casecontrol A vector where 1 indentifies events, 0 identifies potential controls, and NA represent non-event days that are not eligible to be matched (e.g. missing data, or excluded for some other reason).
#' @param matchvars A matrix where the columns represent variables that will be matched on using the Mahalanobis distrance and nearest neighbor.
#' @param by A vector of ids or a matrix with columns as the id variables. The events will be found separately within each unique combination of id variables. This is optional.
#' @value data A data.table of matched cases and controls.
#' @value nn A summary of the number if cases and controls that were matched. See documentation for matchit for more details.
#' @value sum.matched A summary of the quality of each match. See documentation for matchit for more details.
#' @author Ander Wilson
#' @imports data.table MatchIt
#' @export
matchmahal <- function(date,casecontrol,matchvars,by){
  
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
  
  dat<- data.table(matchvars)
  dat[,casecontrol:=casecontrol]
  dat[,byid:=bydt[,(byid)]]
  dat[,date:=date]
  
  form <- formula(paste0("casecontrol~",paste(colnames(dat)[-which(colnames(dat)%in%c("casecontrol","byid","date"))],collapse="+")))
  
  
  matched.sample <- sum.matched <- nn <- data.table()
  for(i in unique(dat[,(byid)])){
    print(i)
    #make matching data for county
    dati <- dat[byid==i]
    dati <- dati[complete.cases(dati),]
    
    if(nrow(dati[casecontrol==1])>0){
      #matchit
      matchit.fit <- matchit(form, data=dati, distance="mahalanobis", method="nearest")
      
      #save match
      matched.sample <- rbind(matched.sample,data.table(match.data(matchit.fit)))
      
      #save statistics on number matched
      temp.nn <- data.table(matchit.fit$nn)
      temp.nn[,type:=row.names(matchit.fit$nn)]
      temp.nn[,byid:=i]
      nn <- rbind(nn,temp.nn)
      
      #add this
      temp.sum.matched <- data.table(summary(matchit.fit)$sum.matched)
      temp.sum.matched[,type:=row.names(temp.sum.matched)]
      temp.sum.matched[,byid:=i]
      sum.matched <- rbind(sum.matched,temp.sum.matched)
    }
  }
  
  
  
  
  
  setkeyv(bydt,"byid") 
  setkeyv(matched.sample,c("byid","date")) 
  setkeyv(nn,"byid") 
  setkeyv(bydt,"byid") 
  sum.matched <- unique(bydt)[sum.matched]
  nn <- unique(bydt)[nn]
  data <- unique(bydt)[matched.sample]
  sum.matched[,byid:=NULL]
  nn[,byid:=NULL]
  data[,byid:=NULL]
  
  
  return(list(data=data,nn=nn,sum.matched=sum.matched))
}


