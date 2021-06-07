#' Testing the sationarity of a given time series using EEMD method
#'
#' @param ts time series data in xts format
#' @param imf_nb  number  of imfs of EEMD
#' @param imf_chosen chosen imf to be tested
#' @param beta  framing value [1-beta , 1+beta]
#'
#' @return  a boolean True if test is valid
#' @import fpp
#' @export
#'
#' @examples
#'library(fpp)
#'data(a10)
#'VVD::IMF_SCA(a10,6,2,0.3)
#'
IMF_SCA <- function(ts,imf_nb,imf_chosen,beta){
  if(class(ts)!="ts"){
    stop("you must pass a ts type time series in argument")
  }
  else if((beta <= 0) || (  beta >= 1 )){
    stop("beta must be in ]0,1[ ,and preferably 0.3")
  }
  else if(imf_nb < imf_chosen){
    stop("you can't test an IMF with order greater than the number of IMFs ")
  }
  else{
  if (!all(is.finite(ts))) {
    stop("'input' must contain finite values only.")
  }
  else{
  eemd_dec<-Rlibeemd::eemd(ts, num_imfs = imf_nb , num_siftings = 10,
                           ensemble_size = 50,  threads = 1)
  eemd_dec_df<-as.data.frame(eemd_dec)

  extrems<-Rlibeemd::extrema(eemd_dec_df[,imf_chosen])
  mins_list<-extrems[["minima"]]
  maxs_list<-extrems[["maxima"]]

  nb_min<-length(mins_list)/2
  nb_max<-length(maxs_list)/2

  dist_min<-c()
  dist_max<-c()

  for (i in 1:nb_min) {
    dist_min[i]<-mins_list[i][1]-mins_list[i-1][1]
  }

  for (i in 1:nb_max) {
    dist_max[i]<-maxs_list[i][1]-maxs_list[i-1][1]
  }

  dist_min<-dist_min[2:length(dist_min)]
  dist_max<-dist_max[2:length(dist_max)]


  dist_min_mode<-modeR(dist_min)
  dist_max_mode<-modeR(dist_max)

  dist_min_sum<-sum(dist_min)
  dist_max_sum<-sum(dist_max)

  mins_test<-((dist_min_sum/dist_min_mode)/(nb_min-1))
  maxs_test<-((dist_max_sum/dist_max_mode)/(nb_max-1))

  print(paste('Test value by minimas for the',imf_chosen,'th IMF is:',mins_test))
  print(paste('Test value by Maximas for the',imf_chosen,'th IMF is:',maxs_test))

  if(((mins_test<= 1+ beta) && (mins_test>= 1 - beta)) || ((maxs_test<= 1+ beta) && (maxs_test>= 1 - beta)) ){
    print(paste("the",imf_chosen,"th   IMF is SEASONAL"))
    test<-TRUE
  }
  else {
    print(paste("the",imf_chosen,"th   IMF is NOT seasonal"))
    test<-FALSE
  }

  return(test)
  }
    }
}








