
#' Combines all IMF s plots into one graph
#'
#' function that plots all IMFs into one combined graph
#' @param ts given time series
#'
#' @return combined plots
#' @export
#'
#' @examples
IMF_multi_plot<-function(ts){
  nb_imfs<-VVD::IMF_number(ts)
  ts_imf_df<-VVD::IMF_maker(ts,nb_imfs)
  par(mfrow = c(nb_imfs, 1))
  for (i in 1:nb_imfs) {
    plot(seq(1,length(ts_imf_df[,i])),ts_imf_df[,i] , type = "l",
         main = "Trend" , xlab = "Time", ylab = "Value", col="blue3")

    VVD::IMF_plot(ts_imf_df,i)
  }
}





