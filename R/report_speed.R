#' Report Speed.
#'
#' @description \code{\link{report_traffic}} runs traffic functions.
#'
#' @param speed path to speed.rds
#' @param net path to net.rds
#' @importFrom cptcity cpt
#' @importFrom ggplot2 ggplot geom_histogram aes scale_fill_gradientn labs
#' @importFrom sp spplot
#' @importFrom data.table .SD
#' @importFrom vein make_grid emis_grid
#' @importFrom methods as
#' @importFrom stats sd
#' @importFrom tidyr gather
#' @importFrom forcats fct_inorder
#' @return a list
#' @export
#' @examples \dontrun{
#' vein::inventory(file.path(tempdir()))
#' source(paste0(file.path(tempdir(), "main.R")))
#' los <- report_speed()
#' names(los)
#' }
report_speed <- function(speed = "network/speed.rds",
                         net = "network/net.rds"){
  lista <- list()
  net <- readRDS(net)
  net <- sf::st_as_sf(net)
  speed <- readRDS(speed)
  dfnet <- sf::st_set_geometry(net, NULL)
  if(exists("speed$geometry")){
    dfspeed <- sf::st_set_geometry(speed, NULL)
  } else{
    dfspeed <- speed
  }
  time <- ifelse(ncol(dfspeed) > 24, 24, ncol(dfspeed))
  if(time == 1){
    dfspeed <- dfspeed
  } else{
    dfspeed <- dfspeed[, 1:time]
  }
  a1 <- data.frame(Hour = names(dfspeed))
  a2 <- as.data.frame(do.call("rbind", lapply(1:ncol(dfspeed), function(i){
    round(summary(as.numeric(as.character(dfspeed[, i]))), 2)
  })))
  a3 <- sapply(dfspeed, stats::sd)
  a3 <- data.frame(sd = a3)
  l1 <- cbind(a1, a2, a3)
  for(i in 1:time){
    dfspeed[, i] <- as.numeric(dfspeed[, i])
  }
  df <- data.frame(dfspeed[, 1:time])
  names(df) <- paste0("S", 1:ncol(df))
  dfspeedg <- tidyr::gather(df, "Hours", "Speed")
  Hours <-   as.numeric(gsub("S", "", dfspeedg$Hours))
  Hours <- factor(Hours, levels = unique(Hours))
  # l2 <-
   l2 <-  ggplot2::ggplot(dfspeedg,
                  ggplot2::aes(x = forcats::fct_inorder(Hours),
                               y = unclass(dfspeedg$Speed)))+
    ggplot2::geom_boxplot() +
     ggplot2::labs(x = "Hours", "Speed [km/h]")


  net <- sf::st_as_sf(net)
  speed <- sf::st_sf(dfspeed, geometry = net$geometry)
  time <- ifelse(ncol(dfspeed) > 24, 24, ncol(dfspeed))
  if(time > 24){
    speed <- speed[, c(1, 9, 15)]
  }
  speedsp <- methods::as(speed, "Spatial")
  for(i in 1:ncol(speedsp@data)){
    speedsp@data[, i] <- as.numeric(speedsp@data[, i])
  }

  l3 <- lapply(1:ncol(speedsp@data), function(i){
    sp::spplot(speedsp, names(speedsp@data)[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = names(speedsp@data)[i])
  })
  lista <- list(l1, l2, l3)
  names(lista) <- c("summary", "boxplots", "streets")
  return(lista)
}
