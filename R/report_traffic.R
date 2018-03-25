#' Report Traffic.
#'
#' @description \code{\link{report_traffic}} runs traffic functions.
#'
#' @param net path to net.rds
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
#' lot <- report_traffic()
#' names(lot)
#' }
report_traffic <- function(net = "network/net.rds",
                           width = 1/102,
                           ..count..){
  net <- readRDS(net)
  net <- sf::st_as_sf(net)
  dfnet <- sf::st_set_geometry(net, NULL)
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
      theme_black() +
      ggplot2::labs(x = NULL, title = names(dfnet)[i])
  })
   netsp <- methods::as(net, "Spatial")
  for(i in 1:ncol(netsp@data)){
    netsp@data[, i] <- as.numeric(netsp@data[, i])
  }

  l3 <- lapply(1:ncol(netsp@data), function(i){
    sp::spplot(netsp, names(net)[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = names(net)[i])
  })
  net <- netsp
  g <- vein::make_grid(net, width)
  gtraffic <- methods::as(vein::emis_grid(net, g), "Spatial")
  l4 <- lapply(1:ncol(dfnet), function(i){
    sp::spplot(gtraffic, names(gtraffic)[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = names(gtraffic)[i])
  })
  lista <- list(l1, l2, l3, l4)
  names(lista) <- c("summary", "histo", "streets", "grids")
  return(lista)
}

