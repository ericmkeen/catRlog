#' Capture history
#'
#' @param ILV desc
#' @param groups desc
#' @param samp.period desc
#'
#' @return desc
#' @export
#'
capture_history <- function(ILV,
                            groups,
                            samp.period="year"){
  # Sample period options
  # year
  # platform
  # py

  head(groups)
  groups$sp <- groups[,which(names(groups)==samp.period)]
  sps <- unique(groups$sp) ; sps

  mr <- ILV ; head(mr)

  chs <- c()
  i=1
  for(i in 1:nrow(mr)){
    mri <- mr[i,] ; mri
    gids <- strsplit(as.character(mri$groups)," ")[[1]] ; gids
    groupi <- groups[as.character(groups$groupid) %in% gids,] ; groupi
    spi <- unique(groupi$sp) ; spi

    chi <- c()
    j=1
    for(j in 1:length(sps)){
      spj <- sps[j] ; spj
      if(spj %in% spi){chj <- 1}else{chj <- 0}
      chi <- c(chi,chj)
    }
    chi
    chi <- paste(chi,collapse="") ; chi
    chs <- c(chs,chi)
  }
  chs

  head(mr)
  df <- data.frame(ch=chs,mr[,1:9]) ; head(df)

  return(df)
}
