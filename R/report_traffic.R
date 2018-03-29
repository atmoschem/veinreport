#' Report Traffic.
#'
#' @description \code{\link{report_traffic}} runs traffic functions.
#'
#' @param net Spatial Feature net with numeric columns
#' @param width Numeric; Widht if cell grid.
#' @param ..count.. ignored.
#' @importFrom cptcity cpt
#' @importFrom sf st_as_sf st_sf st_geometry
#' @importFrom ggplot2 ggplot geom_histogram aes scale_fill_gradientn labs
#' @importFrom sp spplot
#' @importFrom data.table .SD
#' @importFrom vein make_grid emis_grid
#' @importFrom methods as
#' @importFrom stats sd
#' @return a list
#' @export
#' @examples \dontrun{
#' vein::inventory(file.path(tempdir()),rush.hour = T)
#' source(paste0(file.path(tempdir(), "main.R")))
#' net <- readRDS("network/net.rds")
#' net <- net[, c("ldv", "hdv", "ps")]
#' lot <- report_traffic(net)
#' names(lot)
#' }
report_traffic <- function(net,
                           width = 3000,
                           ..count..){
  net <- sf::st_as_sf(net)
  dfnet <- sf::st_set_geometry(net, NULL)
  dfnet <- dfnet[, grep(pattern = TRUE, x = sapply(dfnet, is.numeric))]
  a1 <- data.frame(Category = names(dfnet))
  a2 <- as.data.frame(do.call("rbind", lapply(1:ncol(dfnet), function(i){
    round(summary(as.numeric(as.character(dfnet[, i]))), 2)
  })))
  a3 <- as.data.frame(do.call("rbind", lapply(1:ncol(dfnet), function(i){
    round(sum(as.numeric(as.character(dfnet[, i]))), 2)
  })))
  a4 <- as.data.frame(do.call("rbind", lapply(1:ncol(dfnet), function(i){
    round(stats::sd(as.numeric(as.character(dfnet[, i]))), 2)
  })))
  a5 <- data.frame(Sum = a3$V1, sd = a4$V1)
  l1 <- cbind(a1, a2, a5)
  l2 <- lapply(1:ncol(dfnet), function(i){
    ggplot2::ggplot(dfnet) +
      ggplot2::geom_histogram(ggplot2::aes(x = unclass(dfnet[,i]),
                                           fill = ..count..),
                              col = "white") +
      ggplot2::scale_fill_gradientn(colours = cptcity::cpt()) +
      veinreport::theme_black() +
      ggplot2::labs(x = NULL, title = names(dfnet)[i])
  })
  net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]
  netsp <- methods::as(net, "Spatial")
  for(i in 1:ncol(netsp@data)){
    netsp@data[, i] <- as.numeric(netsp@data[, i])
  }

  l3 <- lapply(1:ncol(netsp@data), function(i){
    netsp$id <- NULL
    sp::spplot(netsp, names(net)[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = names(net)[i])
  })
  net <- netsp
  g <- vein::make_grid(net, width)
  gtraffic <- methods::as(vein::emis_grid(net, g), "Spatial")
  l4 <- lapply(1:ncol(dfnet), function(i){
    gtraffic$id <- NULL
    sp::spplot(gtraffic, names(gtraffic)[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = names(gtraffic)[i])
  })
  lista <- list(l1, l2, l3, l4)
  names(lista) <- c("summary", "histo", "streets", "grids")
  return(lista)
}

