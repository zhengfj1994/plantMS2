#' @name selectNLfliter
#' @description Filter the result of selectNL by some rules
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param selectNLresult, the result of selectNL
#' @param ratioThreshold
#'
#' @return selectNLfliterResult
#' @example selectNLfliterResult <- selectNLfliter2(selectNLresult, ratioThreshold = 0.05)

selectNLfliter <- function(selectNLresult, ratioThreshold){
  require(tcltk)
  #####
  sugarStart <- which(colnames(selectNLresult)=="neutralLoss")+1
  sugarEnd <- which(colnames(selectNLresult)=="mzOfChain")-1

  selectNLresult$k.radio <- NA

  pb <- tkProgressBar("k.radio calculation","Rate of progress %", 0, 100)
  for (i in c(1:nrow(selectNLresult))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(selectNLresult)))
    setTkProgressBar(pb, i*100/nrow(selectNLresult), sprintf("k.radio calculation (%s)", info),info)
    scanNumi <- selectNLresult$scanNum[i]
    matrixi <- selectNLresult[which(selectNLresult$scanNum==scanNumi),sugarStart:sugarEnd]
    counti <- 1
    for (j in c(1:nrow(matrixi))){
      if (sum(abs(selectNLresult[i,sugarStart:sugarEnd]-matrixi[j,]))==sum(selectNLresult[i,sugarStart:sugarEnd]-matrixi[j,]) &
          sum(selectNLresult[i,sugarStart:sugarEnd]-matrixi[j,]) != 0){
        counti <- counti + 1
      }
    }
    selectNLresult$k.radio[i] <- round(counti/sum(selectNLresult[i,sugarStart:sugarEnd]),4)
  }
  close(pb)
  #####

  selectNLresult$lable <- " "

  selectNLresult <- selectNLresult[which(selectNLresult$k.radio >= ratioThreshold), ]

  scanFrequency <- as.data.frame(table(selectNLresult$scanNum))

  pb <- tkProgressBar("selectNLfilter","Rate of progress %", 0, 100)
  for (i in c(1:nrow(scanFrequency))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(scanFrequency)))
    setTkProgressBar(pb, i*100/nrow(scanFrequency), sprintf("selectNLfilter (%s)", info),info)
    scanNumi <- scanFrequency$Var1[i]
    Freqi <- scanFrequency$Freq[i]
    if (Freqi==1){
      selectNLresult$lable[which(selectNLresult$scanNum==scanNumi)] <- "Biggest NL (One)"
    }
    else if (Freqi > 1){
      matrixi <- selectNLresult[which(selectNLresult$scanNum==scanNumi), ]
      biggest.NL.index <- row.names(matrixi)[1]
      biggest.NL.mass <- as.numeric(as.character(matrixi$neutralLoss[1]))
      biggest.NL.N <- as.numeric(as.character(matrixi$numOfSugar[1]))
      biggest.NL.K <- as.numeric(as.character(matrixi$k.radio[1]))
      biggest.NL.int <- as.numeric(as.character(matrixi$intensity[1]))

      for (i in c(2:nrow(matrixi))){
        second.NL.index <- row.names(matrixi)[i]
        second.NL.mass <- as.numeric(as.character(matrixi$neutralLoss[i]))
        second.NL.N <- as.numeric(as.character(matrixi$numOfSugar[i]))
        second.NL.K <- as.numeric(as.character(matrixi$k.radio[i]))
        second.NL.int <- as.numeric(as.character(matrixi$intensity[i]))

        if (biggest.NL.N > second.NL.N){
          next()
        }
        else if (biggest.NL.N <= second.NL.N){
          if (biggest.NL.K > second.NL.K){
            next()
          }
          else if (biggest.NL.K < second.NL.K){
            biggest.NL.index <- second.NL.index
            biggest.NL.mass <- second.NL.mass
            biggest.NL.N <- second.NL.N
            biggest.NL.K <- second.NL.K
            biggest.NL.int <- second.NL.int
          }
          else if (biggest.NL.K == second.NL.K){
            if (biggest.NL.int > second.NL.int){
              next()
            }
            else if (biggest.NL.int < second.NL.int){
              biggest.NL.index <- second.NL.index
              biggest.NL.mass <- second.NL.mass
              biggest.NL.N <- second.NL.N
              biggest.NL.K <- second.NL.K
              biggest.NL.int <- second.NL.int
            }
            else if (biggest.NL.int == second.NL.int){
              next()
            }
          }
        }
      }

      biggest.NL.final <- which(selectNLresult$scanNum==scanNumi &
                                selectNLresult$neutralLoss==biggest.NL.mass &
                                selectNLresult$numOfSugar==biggest.NL.N &
                                selectNLresult$k.radio==biggest.NL.K &
                                selectNLresult$intensity==biggest.NL.int)
      if (length(biggest.NL.final) > 1){
        selectNLresult$lable[biggest.NL.final] <- "Biggest NL (Multiple)"
      }
      else if (length(biggest.NL.final) == 1){
        selectNLresult$lable[biggest.NL.final] <- "Biggest NL (One)"
      }
      else {
        print("Some errors happened")

      }
    }
  }
  close(pb)

  selectNLfliterResult <- selectNLresult
  return(selectNLfliterResult)
}
