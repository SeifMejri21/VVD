#' Plots a chosen IMF of an EEMD time series
#'
#' @param x data frame containing IMF s
#' @param i the index of the column you want to plot
#'
#' @return
#' @export
#'
#' @examples
#' #' VVD::IMF_plot(VVD::IMF_maker(data(a10),VVD::IMF_number(data(a10)),2)
#'
IMF_plot <- function(data,imf){
  ggplot(data, aes(x=seq(1:length(data[,imf])), y=data[,imf])) +
    geom_line() +
    geom_point(shape=21, color="blue3", fill="mediumblue", size=0.5)+
    ggtitle(paste("IMF",imf))+
    ylab("Value")  +
    xlab("Time")
}



