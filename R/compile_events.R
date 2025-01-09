#' Compile events
#'
#' @return desc
#' @export
#'
compile_events <- function(events_path = 'events/'){

  if(FALSE){
    events_path = 'events/'
  }

  evdir <- events_path
  lf <- list.files(evdir)
  lf <- lf[-grep("backup",lf)] ; lf
  #sits <- data.frame(groupid=NA,id=NA,year=NA,month=NA,day=NA,doy=NA,yfrac=NA,
  #                   time=NA, group=NA,platform=NA,lat=NA,lon=NA,file=NA,path=NA)
  if(length(lf)>0){
    lf <- paste0(evdir,lf)  ; lf
    sits <- data.frame()
    for(i in 1:length(lf)){
      lfi <- lf[i]  ; lfi

      lpath <- strsplit(lfi,"/")[[1]] ; lpath
      lpath <- lpath[length(lpath)] ; lpath
      lpath <- gsub(".csv","",lpath) ; lpath
      lpath <- paste0("../0 photos/photos/",lpath,"/") ; lpath

      plat <- strsplit(lfi," ")[[1]]
      platform <- gsub(".csv","",plat[length(plat)])

      siti <- read.csv(lfi,stringsAsFactors=FALSE)  ; siti

      if("id" %in% names(siti)){
        if("lat" %in% names(siti)){lat <- siti$lat}else{lat <- rep(NA,times=nrow(siti))} ; lat
        if("lon" %in% names(siti)){lon <- siti$lon}else{lon <- rep(NA,times=nrow(siti))} ; lon
        if("time" %in% names(siti)){time <- siti$time}else{time <- rep(NA,times=nrow(siti))} ; time

        siti <- data.frame(groupid=NA,id=siti$id,
                           year=siti$year,month=siti$month,day=siti$day,doy=NA,yfrac=NA,time=time,
                           group=siti$group,platform=platform,lat=lat,lon=lon,file=siti$file,path=lpath)
        siti
        sits <- rbind(sits,siti)
      }
    }
    sits
    #sits[which(is.na(sits$group)),]

    if(nrow(sits)>0){
      mm <- as.character(sits$month) ; mm
      mm[nchar(mm)<2] <- paste0("0",mm[nchar(mm)<2]) ; mm
      sits$month <- mm

      dd <- as.character(sits$day) ; dd
      dd[nchar(dd)<2] <- paste0("0",dd[nchar(dd)<2]) ; dd
      sits$day <- dd

      gp <- as.character(sits$group) ; gp
      gp[nchar(gp)<2] <- paste0("0",gp[nchar(gp)<2]) ; gp
      sits$group <- gp

      dt <- paste0(sits$year,"-",sits$month,"-",sits$day," 01:00:00") ; dt
      sits$groupid <- paste0(sits$year,sits$month,sits$day,sits$group) ; sits$groupid
      dt <- time_turner(dt)
      dt
      sits$doy <- as.numeric(dt$j)
      sits$yfrac <- as.numeric(sits$year) + (sits$doy/365) ; sits$yfrac
    }
  }
  return(sits)
}
