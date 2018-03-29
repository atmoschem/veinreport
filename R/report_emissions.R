#' Report Emissions.
#'
#' @description \code{\link{report_traffic}} runs traffic functions.
#'
#' @param emispath Character; path to post .rds
#' @param path_df Character; path to data-frames .rds
#' @param path_streets Character; path to streets .rds
#' @param path_grids  Character; path to grids .rds
#' @importFrom cptcity cpt
#' @importFrom ggplot2 ggplot geom_histogram aes scale_fill_gradientn labs
#' @importFrom sp spplot
#' @importFrom data.table .SD rbindlist
#' @importFrom vein make_grid emis_grid
#' @importFrom methods as
#' @importFrom stats sd aggregate
#' @return a list
#' @export
#' @examples \dontrun{
#' name <- file.path(tempdir())
#' vein::inventory(name)
#' source(paste0(name, "/main.R"))
#' loe <- report_emissions()
#' names(loe)
#' }
report_emissions <- function(emispath = "post",
                             path_df = "post/df",
                             path_streets = "post/streets",
                             path_grids = "post/grids"){
  lista <- list()
  # l1 ####
  if(exists("path_df")){
    x <- list.files(path = path_df,
                    pattern = ".rds",
                    all.files = T,
                    full.names = T,
                    recursive = T)
    xx <- lapply(x, readRDS)
    xxx <- data.table::rbindlist(xx)
    dfa <- stats::aggregate(xxx$g, # Table
                     by = list(xxx$pollutant),
                     sum, na.rm = T)
    names(dfa) <- c("Pollutant", "g_in_all_hours")

    dfb <- stats::aggregate(xxx$g, # plot
                     by = list(xxx$pollutant,  xxx$age),
                     sum, na.rm = T)
    names(dfb) <- c("Pollutant", 'Age', "g_in_all_hours")

    dfc <- stats::aggregate(xxx$g, # plot
                     by = list(xxx$pollutant,  xxx$hour),
                     sum, na.rm = T)
    names(dfc) <- c("Pollutant",  'Hours', "g")

    dfd <- stats::aggregate(xxx$g, # table
                     by = list(xxx$pollutant,  xxx$fuel),
                     sum, na.rm = T)
    names(dfd) <- c("Pollutant",  'Fuel', "g")

    dfaa <- stats::aggregate(xxx$g, # table
                      by = list(xxx$pollutant, xxx$veh, xxx$fuel),
                      sum, na.rm = T)
    names(dfaa) <- c("Pollutant", "Vehicle", "Fuel","g_in_all_hours")

    dfbb <- stats::aggregate(xxx$g, # plot
                      by = list(xxx$pollutant, xxx$veh, xxx$age),
                      sum, na.rm = T)
    names(dfbb) <- c("Pollutant", "Vehicle", "Age","g_in_all_hours")

    tablas <- list(dfa, dfd, dfaa)
    names(tablas) <- c("Veh", "Fuel", "VehFuel")

    p1 <- ggplot2::ggplot(dfb,
                          ggplot2::aes(x = dfb$Age,
                                       y = unclass(dfb$g_in_all_hours),
                                       fill = unclass(dfb$g_in_all_hours)))+
      ggplot2::geom_bar(stat = "identity", color = "white", size = 0.2) +
      ggplot2::scale_fill_gradientn("g", colours = cptcity::cpt()) +
      veinreport::theme_black() +
      ggplot2::facet_wrap(~Pollutant, scales = "free", ncol = 2) +
      ggplot2::labs(x = "Age", y = "g")

    p2 <- ggplot2::ggplot(dfc,
                          ggplot2::aes(x = dfc$Hours,
                                       y = unclass(dfc$g),
                                       fill = unclass(dfc$g)))+
      ggplot2::geom_bar(stat = "identity", color = "white", size = 0.2) +
      ggplot2::scale_fill_gradientn("g", colours = cptcity::cpt()) +
      veinreport::theme_black() +
      ggplot2::facet_wrap(~Pollutant, scales = "free", ncol = 2) +
      ggplot2::labs(x = "Hours", y = "g")

    p3 <- ggplot2::ggplot(dfbb,
                          ggplot2::aes(x = dfbb$Age,
                                       y = unclass(dfbb$g_in_all_hours),
                                       colour = dfbb$Vehicle))+
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~Pollutant, scales = "free", ncol = 2) +
      ggplot2::labs(x = "Age", y = "g") +
      ggplot2::theme_bw()
    plots <- list(p1, p2, p3)
    names(plots) <- c("p1", "p2", "P3")
    lista[[1]] <- tablas
    lista[[2]] <- plots

  }
# l2 ####
  if(exists("path_streets")){
    x <- list.files(path = path_streets,
                    pattern = ".rds",
                    all.files = T,
                    full.names = T,
                    recursive = T)
    nombres <- list.files(path = paste0(emispath, "/streets"),
                    pattern = ".rds",
                    all.files = T,
                    full.names = F,
                    recursive = T)
    nombres <- gsub(pattern = ".rds", replacement = "", x = nombres)
    xx <- lapply(1:length(x), function(i){
      as(readRDS(x[i]), "Spatial")
    })
    names(xx) <- nombres

    times <- ifelse(length(names(xx[[1]])) > 24, 24, length(names(xx[[1]])))
streets <- lapply(1:length(xx), function(j){
  lapply(1:times, function(i){
    xx[[j]]@data[, i] <- as.numeric(xx[[j]]@data[, i])
    sp::spplot(xx[[j]], names(xx[[j]])[i],
               scales = list(draw = T),
               col.regions = rev(cptcity::cpt()),
               main = paste0(names(xx)[[j]]," ", names(xx[[j]])[i], " [g/h]"))
  })
  })

names(streets) <- nombres
lista[[3]] <- streets
  }
  if(exists("path_grids")){
    x <- list.files(path = path_grids,
                    pattern = ".rds",
                    all.files = T,
                    full.names = T,
                    recursive = T)
    nombres <- list.files(path = paste0(emispath, "/grids"),
                          pattern = ".rds",
                          all.files = T,
                          full.names = F,
                          recursive = T)
    nombres <- gsub(pattern = ".rds", replacement = "", x = nombres)
    xx <- lapply(1:length(x), function(i){
      as(readRDS(x[i]), "Spatial")
    })
    names(xx) <- nombres
    times <- ifelse(length(names(xx[[1]])) > 24, 24, length(names(xx[[1]])))
    grids <- lapply(1:length(xx), function(j){
      lapply(1:times, function(i){
        xx[[j]]@data[, i] <- as.numeric(xx[[j]]@data[, i])
        # xx[[j]]@data[, i]$id <- NULL
        sp::spplot(xx[[j]], names(xx[[j]])[i],
                   scales = list(draw = T),
                   col.regions = rev(cptcity::cpt()),
                   main = paste0(names(xx)[[j]]," ", names(xx[[j]])[i], " [g/h]"))
      })
    })
    names(grids) <- nombres
    lista[[4]] <- grids
  }
  names(lista) <- c("tablas", "plots", "streets", "grids")
  return(lista)
}

