# taken from https://rtricks.wordpress.com/2009/05/03/an-algorithm-to-find-local-extrema-in-a-vector/
findpeaks <- function(vec, bw = 1) {
  
  pos.x.max <- NULL
  pos.y.max <- NULL
  pos.x.min <- NULL
  pos.y.min <- NULL 	
  
  for (i in (1+bw):(length(vec)-bw)) {
    sup.stop <- ifelse((i+bw) > length(vec), length(vec), i+bw)
    inf.stop <- ifelse((i-bw) < 1, 1, (i-bw))
    
    ## extract previous and following values within range
    subset.sup <- vec[(i+1):sup.stop]
    subset.inf <- vec[inf.stop:(i-1)]
    
    is.max   <- sum(subset.inf > vec[i]) == 0 # all preceding values smaller?
    is.nomin <- sum(subset.sup > vec[i]) == 0 # all following values smaller?
    
    no.max   <- sum(subset.inf > vec[i]) == length(subset.inf) # all preceding values higher?
    no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup) # all following values higher?
    
    if (is.max & is.nomin) {
      pos.x.max <- c(pos.x.max, i)
      pos.y.max <- c(pos.y.max, vec[i])
    }
    
    if (no.max & no.nomin) {
      pos.x.min <- c(pos.x.min, i)
      pos.y.min <- c(pos.y.min, vec[i])
    }
  }
  
  return(list(pos.x.max,pos.y.max,pos.x.min,pos.y.min))
}
