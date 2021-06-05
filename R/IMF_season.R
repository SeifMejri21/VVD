#' Funds the seasonality of the a given time series
#'
#' @param ts  given time series in xts
#' @param plot  boolean that returns a plot of the seasonality if true is given
#'
#' @return   seasonality of the a given time series
#' @export
#'
#' @examples
#'VVD::IMF_season(data(a10),T)
#'
IMF_season<-function(ts,pl){
  ts_eemd<-VVD::IMF_maker(ts,VVD::IMF_number(ts))
  seasonality_vec=c()
  long<-length(ts_eemd[,1])
  red<-length(VVD::IMF_number(ts)-1)
  ts_eemd$season <- rowSums(ts_eemd[1:(VVD::IMF_number(ts)-1)])
  seasonality_vec<-ts_eemd$season
  if(pl==T){
    plot(seq(1,length(ts_eemd$season)),ts_eemd$season , type = "l",
         main = "Seasonality" , xlab = "Time", ylab = "Value" , col="blue3")
  }
    return(ts_eemd$season)
}



