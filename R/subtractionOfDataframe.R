#' @name subtractionOfDataframe
#' @description One data.frame - one row of data.frame
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param df1
#' @param df2Row
#'
#' @return dfRes
#' @example dfRes <- subtractionofdataframe(df1=selectNLresult[,sugarStart:sugarEnd],df2row=biggestNL[,sugarStart:sugarEnd])

subtractionOfDataframe <- function(df1,df2row){
  for (i in c(1:nrow(df1))){
    df1[i,] <- df2row - df1[i,]
  }
  dfres <- df1
  return(dfres)
}

