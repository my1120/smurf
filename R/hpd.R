

#-------------------------------------------------------------------------------------------------
#' @title Calcualte highest posterior density intervals for MCMC sample
#'
#' @description this function finds the highest posterior density intervals for a vector x.
#' @param x A numeric vector to summarize.
#' @param prop The probability that the interval will contain. The default is 0.95.
#' @return The endpoints of the interval
#' @author Ander Wilson
#' @export
  

hpd <- function(x, prob=0.95){
  x <- x[order(x)]
  size <- round(length(x)*prob)
  width <- NULL
  for(i in 1:(length(x)-size)) width <- c(width, x[i+size]-x[i])
  h <- c(x[which.min(width)],x[which.min(width)+size])
  names(h)<-c("lower","upper")
  return(h)
}
