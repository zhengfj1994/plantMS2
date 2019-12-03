#' @name ms1Identification
#' @description MS1 identification
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param ms1ms2ConbRes
#' @param aglyconeFile
#' @param ionMode
#' @param deltaMZppm
#'
#' @return ms1IdentificationRes
#' @example ms1IdentificationRes <- ms1Identification(ms1ms2ConbRes, aglyconeFile="D:\\github\\plantMS2\\Example Data\\aglyconeDB.xlsx", ionMode='N', deltaMZppm=10)

ms1Identification <- function(ms1ms2ConbRes, aglyconeFile, ionMode, deltaMZppm){
  require(xlsx)
  aglyconeData <- read.xlsx2(aglyconeFile, sheetIndex=1)
  ms1ms2ConbRes$Algycone <- ' '
  if (ionMode == 'P'){
    addMZ <- -1.007276
  }
  else if (ionMode == 'N'){
    addMZ <- 1.007276
  }

  pb <- tkProgressBar("ms1Identification","Rate of progress %", 0, 100)
  for (i in c(1:nrow(ms1ms2ConbRes))){

    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(ms1ms2ConbRes)))
    setTkProgressBar(pb, i*100/nrow(ms1ms2ConbRes), sprintf("ms1Identification (%s)", info),info)

    if (ms1ms2ConbRes$lable[i] != 'NL matched to biggest NL'){
      if (as.character(ms1ms2ConbRes$m.z[i])!='...'){
        mwInMs1 <- as.numeric(as.character(ms1ms2ConbRes$m.z[i])) + addMZ
      }
      else if (as.character(ms1ms2ConbRes$m.z[i])=='...'){
        mwInMs1 <- mwInMs1
      }
      mzOfChain <- as.numeric(as.character(ms1ms2ConbRes$mzOfChain[i]))
      aglyconeMass <- mwInMs1-mzOfChain
      aglyconePosi <- which(abs(as.numeric(as.character(aglyconeData$molecular.weight)) - aglyconeMass)/aglyconeMass*1000000 < deltaMZppm)
      if (length(aglyconePosi)==0){
        ms1ms2ConbRes$Algycone[i] <- "No matched algycone"
      }
      else if (length(aglyconePosi)==1){
        ms1ms2ConbRes$Algycone[i] <- as.character(aglyconeData$Aglycone[aglyconePosi[1]])
      }
      else if (length(aglyconePosi) > 1){
        addAglyconData <- as.character(aglyconeData$Aglycone[aglyconePosi])
        ms1ms2ConbRes$Algycone[i] <- paste(addAglyconData,collapse=" / ")
      }
    }
  }
  close(pb)
  #ms1IdentificationRes <- ms1ms2ConbRes[which(is.na(ms1ms2ConbRes$Aglycone)==FALSE), ]
  ms1IdentificationRes <- ms1ms2ConbRes
  return(ms1IdentificationRes)
}
