
#' Combines all IMF s plots into one graph
#'
#' function that plots all IMFs into one combined graph
#' @param ts given time series
#'
#' @return combined plots
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#' VVD::IMF_multi_plot(a10)
#'
IMF_multi_plot<-function(ts){
  if(class(ts)!="ts"){
    stop("you must pass a ts type time series in argument")
  }
  else{
  nb_imfs<-VVD::IMF_number(ts)
  ts_imf_df<-VVD::IMF_maker(ts,nb_imfs)
  stats::plot.ts(ts_imf_df)
  }
}





