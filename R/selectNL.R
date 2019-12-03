#' @name selectNL
#' @description Find the neutral loss which fit to database.
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param precusorAndProductPair
#' @param NLmatrix
#' @param deltaMZppm
#'
#' @return selectNLresult
#' @example selectNLresult <- selectNL(precusorAndProductPair,NLmatrix,deltaMZppm=20)

selectNL <- function(precusorAndProductPair,NLmatrix,deltaMZppm){
  require(tcltk)

  NLmatrixi <- as.data.frame(NLmatrix)
  NLmatrixi$ppm <- abs((NLmatrixi$mzOfChain-precusorAndProductPair$neutralLoss[1])/precusorAndProductPair$neutralLoss[1])*1000000
  matchedNL <- NLmatrixi[which(NLmatrixi$ppm < deltaMZppm),]
  selectNLresult <- cbind(precusorAndProductPair[0,],matchedNL[0,])

  pb <- tkProgressBar("selectNL","Rate of progress %", 0, 100)
  for (i in c(1:nrow(precusorAndProductPair))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(precusorAndProductPair)))
    setTkProgressBar(pb, i*100/nrow(precusorAndProductPair), sprintf("selectNL (%s)", info),info)
    NLmatrixi <- as.data.frame(NLmatrix)
    NLmatrixi$ppm <- abs((NLmatrixi$mzOfChain-precusorAndProductPair$neutralLoss[i])/precusorAndProductPair$neutralLoss[i])*1000000
    matchedNL <- NLmatrixi[which(NLmatrixi$ppm < deltaMZppm),]

    if (nrow(matchedNL) > 0){
      addprecusorAndProductPair <- matrix(rep(as.matrix(precusorAndProductPair[i,]),nrow(matchedNL)),nrow = nrow(matchedNL),byrow=T)
      colnames(addprecusorAndProductPair) <- colnames(precusorAndProductPair)
      selectNLresulti <- cbind(addprecusorAndProductPair,matchedNL)
      selectNLresult <- rbind(selectNLresult,selectNLresulti)
    }
    else{
      selectNLresult <- selectNLresult
    }
  }
  close(pb)
  sugarStart <- which(colnames(selectNLresult)=='neutralLoss')+1
  sugarEnd <- which(colnames(selectNLresult)=='mzOfChain')-1
  selectNLresult$numOfSugar <- rowSums(selectNLresult[,sugarStart:sugarEnd])

  return(selectNLresult)
}
