
#' Makes Ensemble Empiric Mode Decomposition  for a given Time Series
#'
#' @param ts   time series in xts format
#' @param nb_imf  chosen number of IMFs
#'
#' @return  data frame containing the IMF s of a given time series
#' @export
#'
#' @examples
#' VVD::IMF_maker(data(a10),6)
#'
#'
IMF_maker <- function(ts,nb_imf){
  eemd_ts<-Rlibeemd::eemd(ts, num_imfs = nb_imf , num_siftings = 10,
                           ensemble_size = 50,  threads = 1)

  eemd_ts_df<-as.data.frame(eemd_ts)
  return(eemd_ts_df)
}



