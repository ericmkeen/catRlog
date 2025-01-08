#' Compile Individual Level Variables
#'
#' @param events desc
#'
#' @return desc
#' @export
#'
compile_ILV <- function(events){

  if(FALSE){
    events <- compile.events()
  }

  mr <- events
  sits <- compile.groups(events)
  key <- read.csv("../4 catalog/catalog key.csv",stringsAsFactors=FALSE) ; key

  ids <- unique(as.character(mr$id)) ; ids
  ids <- ids[!is.na(ids)] ; ids
  ids <- ids[ids!="UIS"] ; ids
  ids <- sort(ids) ; ids

  df <- data.frame()
  i=1
  for(i in 1:length(ids)){
    idi <- ids[i] ; idi

    keyi <- key[key$id==idi,] ; keyi
    local <- calf <- mother <- NA
    if(nrow(keyi)>0){
      local <- as.character(keyi$local)
      calf <- as.character(keyi$calf)
      mother <- as.character(keyi$mother)
    }

    siti <- sits[grep(idi,sits$id),] ; siti
    n <- nrow(siti) ; n
    ny <- length(unique(siti$year)) ; ny
    groups <- paste(siti$groupid,collapse=" ") ; groups
    years <- paste(unique(siti$year),collapse=" ") ; years
    doys <- paste(siti$doy,collapse=" ") ; doys
    doy.min <- min(as.numeric(as.character(siti$doy))) ; doy.min
    doy.max <- max(as.numeric(as.character(siti$doy))) ; doy.max
    dfi <- data.frame(id=idi,local,mother,calf,n,ny,doy.min,doy.max,years,doys,groups) ; dfi
    df <- rbind(df,dfi)
  }

  df
  return(df)
}
