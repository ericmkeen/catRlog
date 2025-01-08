#' Find quality score
#'
#' @param log desc
#' @param epath desc
#'
#' @return desc
#' @export
#'
find_quality_score <- function(log,
                               epath){

  #epath <- "../0 photos/photos/2015 Elemiah/2015-06-21_2945.JPG"
  ematch <- which(log$path == epath) ; ematch
  scores <- data.frame()
  distinct <- quality <- NA
  if(length(ematch)){
    scores <- log[ematch[1],] ; scores
    distinct <- scores$distinct
    scori <- scores[6:9] ; scori
    if(any(scori==3)){
      quality <- 3
    }else{
      if(any(scori==2) & any(scori==1)){
        quality <- 12
      }else{
        if(all(scori==2)){quality <- 2}
        if(all(scori==1)){quality <- 1}
      }
    }
  }
  return(list(score=quality,distinct=distinct,row=scores))
}
