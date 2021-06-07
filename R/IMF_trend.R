#' Fonds the trend of a given time series
#'
#' @param ts time series xts format
#' @param plot boolean if True Plots the trend
#'
#' @return  returns an xts variable containing a
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#' VVD::IMF_trend(a10,TRUE)
#'
IMF_trend <- function(ts,pl){
   if(class(ts)!="ts"){
      stop("you must pass a ts type time series in argument")
   }
   else{
   ts_eemd<-VVD::IMF_maker(ts,VVD::IMF_number(ts))
   x_trend<-VVD::IMF_number(ts)
   if(pl == TRUE){
     plot(seq(1,length(ts_eemd[,x_trend])),ts_eemd[,x_trend] , type = "l",
          main = "Trend" , xlab = "Time", ylab = "Value", col="blue3")
          }
   return(ts_eemd[,VVD::IMF_number(ts)])
   }
}
