#' Compile groups
#'
#' @param events desc
#'
#' @return desc
#' @export
#'
compile_groups <- function(events){

  if(FALSE){
    events <- compile.events()
  }

  sits <- events
  head(sits)

  mr <- data.frame()
  grps <- unique(sits$groupid) ; grps
  i=100
  for(i in 1:length(grps)){
    grpi <- grps[i] ; grpi
    siti <- sits[sits$groupid==grpi,] ; siti

    group <- as.character(siti$group)[1]
    platform <- as.character(siti$platform)[1]
    year <- as.character(siti$year)[1]
    py <- paste(year, platform) ; py
    month <- as.character(siti$month)[1]
    day <- as.character(siti$day)[1]
    doy <- as.character(siti$doy)[1]
    yfrac <- as.character(round(siti$yfrac[1],3))
    time <- as.character(siti$time)[1]
    lat <- as.numeric(as.character(siti$lat))
    lat <- lat[!is.na(lat)]
    if(length(lat)==0){lat=NA}
    lat <- lat[1] ; lat

    lon <- as.numeric(as.character(siti$lon))
    lon <- lon[!is.na(lon)]
    if(length(lon)==0){lon=NA}
    lon <- lon[1] ; lon

    indivs <- length(unique(siti$id)) ; indivs
    ids <- paste(unique(siti$id),collapse=" ") ; ids

    grpdf <- data.frame(groupid=grpi,py,platform,year,month,day,doy,yfrac,time,group,size=indivs,lat,lon,id=ids)
    grpdf
    mr <- rbind(mr,grpdf)
  }

  mr
  return(mr)
}
