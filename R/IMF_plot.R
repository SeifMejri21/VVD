#' Plots a chosen IMF of an EEMD time series
#'
#' @param ts time series to manipulate
#' @param imf the index of the IMF you want to plot
#'
#' @return
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#' #' VVD::IMF_plot(a10,2)
#'
IMF_plot <- function(ts,imf){
  ts_eemd<-VVD::IMF_maker(ts,VVD::IMF_number(ts))
  ggplot(ts_eemd, aes(x= stats::time(ts), y=ts_eemd[,imf])) +
    geom_line() +
    geom_point(shape=21, color="blue3", fill="mediumblue", size=0.5)+
    ggtitle(paste("IMF",imf))+
    ylab("Value")  +
    xlab("Time")
}



