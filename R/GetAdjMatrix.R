#' @title Function to compute adjacency matrix from a geostatistical data basd on TMS data
#' @description Computes adjacency matrix from TMS associate target indicator
#' @param ID ID of the form "Rect. Grid 1 (-1, -1)"
#' @return Returns adjacency matrix.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' GetAdjMatrix(ID)
#' }
#'


GetAdjMatrixTMS  <- function(ID){

  clust <- levels(as.factor(ID))

  M = lapply(1:length(clust),function(i)as.matrix(eval(parse(text=paste0("c", clust[i]))) ))
  long=NULL
  for (i in 1: length(clust)) long =c(long, M[[i]][1,] )
  lat=NULL
  for (i in 1: length(clust)) lat =c(lat, M[[i]][2,] )
  M = cbind(long,lat)
  L1 <-NULL
  L2 <- NULL
  neiglength <- 2:5
  n=length(clust)
  for(i in 1:n){
    f <- function(j) distm(c(M[i,1], M[i,2]), c(M[j,1], M[j,2]))
    c <- lapply(1:n,f )
    d<- data.frame(id =1:n,dist=unlist(c))
    d1 <- d[order(d$dist),]
    d2 <- d1[,1][neiglength]
    L1 <- c(L1,rep(i,4))
    L2 <- c(L2,d2)
  }
  mat <- data.frame(L1,L2)
  g=get.adjacency(graph_from_edgelist(as.matrix(mat)))
  g = forceSymmetric(g)
  return(list(g,clust))
}
