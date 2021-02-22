#' @name ms1ms2Conbination
#' @description Conbina ms1 file and ms2 identified result.
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param ms1File
#' @param selectNLfliterResult
#' @param deltaMZppm
#' @param deltaTR
#'
#' @return ms1ms2ConbRes
#' @example ms1ms2ConbRes <- ms1ms2Conbination(ms1File="D:\\github\\ModifiedMetMRM\\示例文件\\POS_一级峰表\\POS-TIC.txt", selectNLfliterResult, deltaMZppm=20000, deltaTR=5)

ms1ms2Conbination <- function(ms1File, selectNLfliterResult, deltaMZppm, deltaTR){
  require(xlsx)
  require(tcltk)
  ms1Data <- read.xlsx2(file=ms1File,sheetIndex=1)
  ms1ms2ConbRes <- cbind(ms1Data[0,],selectNLfliterResult[0,])

  pb <- tkProgressBar("ms1ms2Conbination","Rate of progress %", 0, 100)
  for (i in c(1:nrow(ms1Data))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(ms1Data)))
    setTkProgressBar(pb, i*100/nrow(ms1Data), sprintf("ms1ms2Conbination (%s)", info),info)

    mzInMs1 <- as.numeric(as.character(ms1Data$m.z[i]))
    trInMs1 <- as.numeric(as.character(ms1Data$RT..min.[i]))
    matchedMs2Posi <- which(abs(as.numeric(as.character(selectNLfliterResult$pepmass))-mzInMs1)/mzInMs1 * 1000000 < deltaMZppm &
                            abs(as.numeric(as.character(selectNLfliterResult$tr))/60 - trInMs1) < deltaTR)
    if (length(matchedMs2Posi)==0){
      ms2Data <- selectNLfliterResult[0,]
      ms2Data[1,] <- NA
      matchedMs1Ms2 <- cbind(ms1Data[i,],ms2Data)
      ms1ms2ConbRes <- rbind(ms1ms2ConbRes,matchedMs1Ms2)
    }
    else if (length(matchedMs2Posi)==1){
      matchedMs1Ms2 <- cbind(ms1Data[i,],selectNLfliterResult[matchedMs2Posi,])
      ms1ms2ConbRes <- rbind(ms1ms2ConbRes,matchedMs1Ms2)
    }
    else if (length(matchedMs2Posi) > 1){
      pointMatrix <- data.frame(matrix('...',length(matchedMs2Posi)-1,ncol(ms1Data)))
      colnames(pointMatrix) <- colnames(ms1Data)
      matchedMs1Ms2 <- cbind(rbind(ms1Data[i,],pointMatrix),selectNLfliterResult[matchedMs2Posi,])
      ms1ms2ConbRes <- rbind(ms1ms2ConbRes,matchedMs1Ms2)
    }
  }
  close(pb)
  ms1ms2ConbRes <- ms1ms2ConbRes[which(is.na(ms1ms2ConbRes$lable)==FALSE),]
  return(ms1ms2ConbRes)
}

