#' Funds the seasonality of the a given time series
#'
#' @param ts  given time series in xts
#' @param plot  boolean that returns a plot of the seasonality if true is given
#'
#' @return   seasonality of the a given time series
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#'VVD::IMF_season(a10,TRUE)
#'
IMF_season<-function(ts,pl){
  if(class(ts)!="ts"){
    stop("you must pass a ts type time series in argument")
  }
  else{
  ts_eemd<-VVD::IMF_maker(ts,VVD::IMF_number(ts))
  seasonality_vec=c()
  long<-length(ts_eemd[,1])
  red<-length(VVD::IMF_number(ts)-1)
  ts_eemd$season <- rowSums(ts_eemd[1:(VVD::IMF_number(ts)-1)])
  seasonality_vec<-ts_eemd$season
  if(pl==TRUE){
    plot(seq(1,length(ts_eemd$season)),ts_eemd$season , type = "l",
         main = "Seasonality" , xlab = "Time", ylab = "Value" , col="blue3")
  }
    return(ts_eemd$season)
  }
}



