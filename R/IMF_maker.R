
#' Makes Ensemble Empiric Mode Decomposition  for a given Time Series
#'
#' @param ts   time series in xts format
#' @param nb_imf  chosen number of IMFs
#'
#' @return  data frame containing the IMF s of a given time series
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#' VVD::IMF_maker(a10,6)
#'
#'
IMF_maker <- function(ts,nb_imf){
  if(class(ts)!="ts"){
    stop("you must pass a ts type time series in argument")
  }
  else if (( all.equal(nb_imf, as.integer(nb_imf))) != TRUE)
  {
    stop("nb_imf must be a positive integer")
  }
  else {
  eemd_ts<-Rlibeemd::eemd(ts, num_imfs = nb_imf , num_siftings = 10,
                           ensemble_size = 50,  threads = 1)

  eemd_ts_df<-as.data.frame(eemd_ts)
  return(eemd_ts_df)
  }
}



