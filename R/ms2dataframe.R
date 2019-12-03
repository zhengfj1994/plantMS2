#' @name ms2dataframe
#' @description Transfer list to dataframe.
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param mgfDataMS2
#'
#' @return mgfMS2Dataframe
#' @example mgfMS2Dataframe <- ms2dataframe(mgfDataMS2)

ms2dataframe <- function(mgfDataMS2){
  mgfMS2Dataframe <- data.frame(productIon = 0,intensity = 0)
  mgfMS2Dataframe <- mgfMS2Dataframe[-1,]
  ms2List=strsplit(mgfDataMS2,split = " ")
  for (i in c(1:length(ms2List))){
    mgfMS2Dataframe[i,'productIon'] <- as.numeric(ms2List[[i]][1])
    mgfMS2Dataframe[i,'intensity'] <- as.numeric(ms2List[[i]][2])
  }
  return(mgfMS2Dataframe)
}
