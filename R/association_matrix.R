#' Create association matrix
#'
#' @param dyads desc
#' @param var desc
#'
#' @return desc
#' @export
#' @import igraph
#'
association.matrix <- function(dyads,
                               var){
  if(FALSE){
    ilv <- compile.ILV()
    dyads <- compile.dyads(ilv)
    var="sri"
  }

  head(dyads)
  data <- dyads
  X <- data[,which(names(data)==var)] ; X

  MAT <- matrix(data = X,
                nrow = length(unique(data$A)),
                ncol = length(unique(data$A)),
                byrow = FALSE,
                dimnames = list(unique(data$A),unique(data$A)))
  MAT[1:6,1:6]

  gmat <- MAT
  diag(gmat) <- 0
  g=graph.adjacency(gmat,mode="undirected",weighted=TRUE)
  return(list(g=g,mx=MAT))
}
