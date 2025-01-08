#' Create a dataset of dyads
#'
#' @param ilv desc
#' @param mr desc
#'
#' @return desc
#' @export
#'
make_dyads <- function(ilv,
                       mr){

  if(FALSE){
    mr <- compile.events()
    ilv <- compile.ILV(mr)
    var="sri"
  }

  #head(mr)
  #head(sits)
  head(ilv)

  uid <- unique(as.character(ilv$id)) ; uid
  #uid <- ids
  df <- expand.grid(A=uid,B=uid) ; nrow(df)
  head(df)

  #id <- uid[1] ; id
  idinfo <- function(id){
    idi <- ilv[as.character(ilv$id)==as.character(id),] ; idi
    years <- strsplit(as.character(idi$years)," ")[[1]] ; years
    groups <- strsplit(as.character(idi$groups)," ")[[1]] ; groups
    return(list(yr=years,sit=groups))
  }

  #a <- df$A[2] ; a
  #b <- df$B[2] ; b
  #dfrow <- df[2,]

  dyad.calc <- function(dfrow){
    a <- idinfo(as.character(dfrow[1])) ; a
    #a <- idinfo(as.character(a)) ; a

    b <- idinfo(as.character(dfrow[2])) ; b
    #b <- idinfo(as.character(b)) ; b

    xs <- which(a$sit %in% b$sit) ; xs
    X <- yrX <- 0
    if(length(xs)>0){
      xs <- a$sit[xs] ; xs
      X <- length(xs)
      yrs <- substr(xs,1,4) ; yrs
      yrX <- length(unique(yrs))
    }
    X ; yrX

    resulti <- c(yrA=length(a$yr),
                 yrB=length(b$yr),
                 yrX=yrX,
                 nA=length(a$sit),
                 nB=length(b$sit),
                 X=X)
    resulti
    return(resulti)
  }

  dyads <- apply(df,1,dyad.calc)
  dyads <- t(dyads)
  head(dyads)
  dyad <- paste(df$A,"-",df$B)
  dyads <- cbind(dyad,df,dyads) ; head(dyads)
  dyads$ya <- dyads$nA - dyads$X
  dyads$yb <- dyads$nB - dyads$X
  dyads$yabx <- dyads$ya + dyads$yb + dyads$X
  dyads$ynull <- length(unique(mr$sitid)) - (dyads$nA + dyads$nB - dyads$X)
  head(dyads)

  # Apply association indices
  dyads <- association_index(dyads)

  return(dyads)
}
