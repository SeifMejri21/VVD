#' Finds appropriate number of IMFs for a given time series
#'
#' @param ts time series to decompose in xts format
#'
#' @return the appropriate number of IMF s
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#' VVD::IMF_number(a10)
#'
IMF_number <-function(ts){
  if(class(ts)!="ts"){
    stop("you must pass a ts type time series in argument")
  }
  else{
  test_vector<-list()
  eemd_dec<-Rlibeemd::eemd(ts, num_imfs = 1000 , num_siftings = 10,
                           ensemble_size = 50,  threads = 1)
  eemd_dec_df<-as.data.frame(eemd_dec)

    reality_vec<-c()

  for (j in 1:1000) {

    extrems<-Rlibeemd::extrema(eemd_dec_df[,j])
    mins_list<-extrems[["minima"]]
    maxs_list<-extrems[["maxima"]]

    nb_min<-length(mins_list)/2
    nb_max<-length(maxs_list)/2

    if(!((nb_min<=3) && (nb_max<=3))){
      reality_vec[j]<-FALSE}
    else{reality_vec[j]<-TRUE}

  }
  for (m in 1:1000) {
    if(reality_vec[m] == TRUE){
      break
    }
  }
  return(m)
  }
}





