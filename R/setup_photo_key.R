#' Setup photo key
#'
#' @return desc
#' @export
#'
setup_photo_key <- function(){
  # Inventory the IDs you can get photos of based on event logs

  evdir <- "../1 events/"
  lf <- list.files(evdir) ; lf
  lf <- lf[lf!="backups"] ; lf
  lf <- paste0(evdir,lf) ; lf

  i=1
  log <- data.frame()
  for(i in 1:length(lf)){
    lfi <- lf[i] ; lfi
    mri <- read.csv(lfi,stringsAsFactors=FALSE) ; mri
    fili <- as.character(mri$file) ; fili
    idi <- as.character(mri$id) ; idi
    pathcore <- gsub("../1 events/","",lfi) ; pathcore
    pathcore <- gsub(".csv","",pathcore) ; pathcore
    photopath <- paste0("../0 photos/photos/",pathcore,"/") ; photopath
    newi <- data.frame(id=idi,core=pathcore,photo=photopath,file=fili) ; newi
    log <- rbind(log,newi)
  }

  # Remove NAs
  head(log) ; nrow(log)
  log <- log[!is.na(log$id),]
  nrow(log) ; head(log)

  # Finding the photos that match these files
  log$imgpath <- NA
  i=nrow(log)
  for(i in 1:nrow(log)){
    logi <- log[i,] ; logi
    fili <- as.character(logi$file) ; fili
    pp <- as.character(logi$photo) ; pp
    plf <- list.files(pp) ; plf
    if(length(plf)>0){
      matchi <- grep(fili,plf)
      if(length(matchi)>0){
        img <- plf[matchi] ; img
        fullpath <- paste0(pp,img) ; fullpath
        log$imgpath[i] <- fullpath
      }
    }
  }

  # Remove NAs
  head(log) ; nrow(log)
  log <- log[!is.na(log$imgpath),]
  nrow(log) ; head(log)

  return(log)
}
