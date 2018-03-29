#' Inventory function.
#'
#' @description \code{\link{vein_report}} produces an structure of directories and scripts
#' in order to run vein. It is required to know the vehicular composition of the
#' fleet.
#'
#' @param author Character; name of the author of the report.
#' @param path  Character; path where to create report.Rmd.
#' @param traffic Logical; if TRUE writes traffic functions.
#' @param speed Logical; if TRUE writes speed functions.
#' @param ef  Character; 'local', 'speed' or 'scaled'.
#' @param emissions Logical; if TRUE writes emissions functions.
#' @return report.Rmd.
#' @export
#' @examples \dontrun{
#' name <- file.path(tempdir())
#' vein::inventory(name, rush.hour = TRUE)
#' source(paste0(name, "/main.R"))
#' vein_report(author = "me", path = name)
#' rmarkdown::render(paste0(name, "/report.Rmd"))
#' }
vein_report <- function(author = "me",
                        path = getwd(),
                        traffic = TRUE,
                        speed = TRUE,
                        ef = "scaled",
                        emissions = TRUE){
  sink(paste0(path,"/report.Rmd"))
  cat("---\n")
  cat(paste0("title: ", deparse("VEIN Report"), "\n"))
  cat(paste0("author: ", deparse(author), "\n"))
  cat("output: 'word_document'\n")
  cat("toc: yes\n")
  cat("---\n\n")
  cat("```{r setup, include=FALSE}\n")
  cat("knitr::opts_chunk$set(echo = TRUE)\n")
  cat("```\n\n")
  cat("### Date ")
  cat("`r Sys.time()` \n\n")
  cat("`r getwd()` \n\n")
  if(traffic){
    cat("# Traffic\n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("library(ggplot2, quietly = T)\n")
    cat("library(sf, quietly = T)\n")
    cat("net <- readRDS('network/net.rds')\n")
    cat("net <- net[, c('ldv', 'hdv', 'ps')]\n")
    cat("a <- veinreport::report_traffic(net = net, width = 3000)\n")
    cat("knitr::kable(a$summary, align = 'lrrrrrr')\n")
    cat("a$plots\n")
    cat("a$histo\n")
    cat("a$streets\n")
    cat("a$grids\n")
    cat("```\n\n")
  }
  if(speed){
    cat("# Speed \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("net <- readRDS('network/net.rds')\n")
    cat("net <- net[, c('ldv', 'hdv', 'ps')]\n")
    cat("speed <- readRDS('network/speed.rds')\n")
    cat("a <- veinreport::report_speed(speed = speed, net = net)\n")
    cat("knitr::kable(a$summary, align = 'lrrrrrr')\n")
    cat("a$plots\n")
    cat("a$boxplots\n")
    cat("a$streets\n")
    cat("a$grids\n")
    cat("```\n\n")
  }
  if(ef == "local"){
    cat("# Emission Factors\n\n")
    cat("f1\n\n")
  }
  if(ef == "speed"){
    cat("# Emission Factors\n\n")
    cat("f1\n\n")
  }
  if(ef == "scaled"){
    cat("# Emission Factors\n\n")
    cat("f1\n\n")
  }
  if(emissions){
    cat("# Emissions \n\n")
    cat("## Tables \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("a <- veinreport::report_emissions()\n")
    cat("knitr::kable(a$tablas$Veh, align = 'llr')\n")
    cat("```\n\n")
    cat("### Tables \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("knitr::kable(a$tablas$Fuel, align = 'llr')\n")
    cat("```\n\n")
    cat("### Tables \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("knitr::kable(a$tablas$VehFuel, align = 'llr')\n")
    cat("```\n\n")
    cat("### Plots \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE,  out.width='100%'}\n")
    cat("a$plots\n")
    cat("```\n\n")
    cat("### Street plots \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("a$streets\n")
    cat("```\n\n")
    cat("### Grid plots \n\n")
    cat("```{r, echo = FALSE, warning=FALSE, message=FALSE, out.width='100%'}\n")
    cat("a$grids\n")
    cat("```\n\n")
  }
  sink()
}

