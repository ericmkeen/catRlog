#' Compute association indices
#'
#' @param dyads desc
#'
#' @return desc
#' @import assocInd
#' @export
#'
association_index <- function(dyads){
  # Association indices
  # half weight, simple ratio, and simple gleci (assuming w=0)

  mr <- dyads
  mr$hwi <- NA
  mr$sri <- NA
  for(i in 1:nrow(mr)){
    dati <- mr[i,] ; dati
    #haii <- hai(x=dati$X, ya=dati$ya, yb=dati$yb)
    #mr$hai[i] <- haii
    #dati

    hwi <- HWI(x=dati$X,Ya=dati$ya,Yb=dati$yb,Yab=0) ; hwi
    mr$hwi[i] <- hwi[1]

    sri <- SRI(x=dati$X,Ya=dati$ya,Yb=dati$yb,Yab=0) ; sri
    mr$sri[i] <- sri[1]
  }
  head(mr)

  return(mr)
}
