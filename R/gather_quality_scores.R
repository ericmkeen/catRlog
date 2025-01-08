#' Gather quality scores
#'
#' @return desc
#' @export
#'
gather_quality_scores <- function(){
  scoredir <- "../2 scores/score sessions/"
  lf <- list.files(scoredir) ; lf
  lf <- paste0(scoredir,lf) ; lf
  i=1
  log <- data.frame()
  for(i in 1:length(lf)){
    lfi <- lf[i] ; lfi
    mri <- read.csv(lfi,stringsAsFactors=FALSE,header=FALSE) ; mri
    log <- rbind(log,mri)
  }
  names(log) <- c("time","analyst","event","img","path","angle","focus","exposure","visible","distinct","calf","parasites")
  head(log)

  # Remove NAs
  head(log) ; nrow(log)
  log <- log[!is.na(log$path),]
  nrow(log) ; head(log)

  return(log)
}
