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

selectNL <- function(sugarFile,precusorAndProductPair,NLmatrix,deltaMZppm,ionMode){
  require(tcltk)
  require(xlsx)
  sugarsInFile <- read.xlsx2(sugarFile,sheetIndex=1)
  acid.name <- sugarsInFile$name[which(sugarsInFile$class == "acid")]
  acid.MW <- sugarsInFile$molecular.weight[which(sugarsInFile$class == "acid")]

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

    all.product.ions <- precusorAndProductPair$productIon[which(precusorAndProductPair$beginNum == precusorAndProductPair$beginNum[i])]
    matchedNL.delete.pos <- c()
    if (nrow(matchedNL) > 0){
      # print(nrow(matchedNL))
      for (j in c(1:nrow(matchedNL))){
        for (k in c(1:length(acid.name))){
          if (matchedNL[j,acid.name[k]] != 0 & ionMode == "P"){
            acid.product.ion <- as.numeric(as.character(acid.MW[k]))+1.007276
            pos.jk <- which(abs(all.product.ions - acid.product.ion)/acid.product.ion * 1000000 < deltaMZppm)
            if (length(pos.jk) == 0){
              matchedNL.delete.pos <- c(matchedNL.delete.pos, j)
            }
            else{
              matchedNL.delete.pos <- matchedNL.delete.pos
            }
          }
          else if (matchedNL[j,acid.name[k]] != 0 & ionMode == "N"){
            acid.product.ion <- as.numeric(as.character(acid.MW[k]))-1.007276
            pos.jk <- which(abs(all.product.ions - acid.product.ion)/acid.product.ion * 1000000 < deltaMZppm)
            if (length(pos.jk) == 0){
              matchedNL.delete.pos <- c(matchedNL.delete.pos, j)
            }
            else{
              matchedNL.delete.pos <- matchedNL.delete.pos
            }
          }
          else{
            matchedNL.delete.pos <- matchedNL.delete.pos
          }
        }
      }
      if (is.null(unique(matchedNL.delete.pos))){
        matchedNL <- matchedNL
      }
      else{
        matchedNL <- matchedNL[-unique(matchedNL.delete.pos),]
      }

      if (nrow(matchedNL) > 0){
        addprecusorAndProductPair <- matrix(rep(as.matrix(precusorAndProductPair[i,]),nrow(matchedNL)),nrow = nrow(matchedNL),byrow=T)
        colnames(addprecusorAndProductPair) <- colnames(precusorAndProductPair)
        selectNLresulti <- cbind(addprecusorAndProductPair,matchedNL)
        selectNLresult <- rbind(selectNLresult,selectNLresulti)
      }
      else {
        selectNLresult <- selectNLresult
      }
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

