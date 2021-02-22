#' @name selectNLfliter
#' @description Filter the result of selectNL by some rules
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param selectNLresult, the result of selectNL
#' @param intThreshold
#' @param ratioThreshold
#'
#' @return selectNLfliterResult
#' @example selectNLfliterResult <- selectNLfliter2(selectNLresult, intThreshold = 0.005, ratioThreshold = 0.05)

selectNLfliter2 <- function(selectNLresult,intThreshold, ratioThreshold){
  require(tcltk)
  pb <- tkProgressBar("selectNLfliter","Rate of progress %", 0, 100)

  sugarStart <- which(colnames(selectNLresult)=="neutralLoss")+1
  sugarEnd <- which(colnames(selectNLresult)=="mzOfChain")-1

  selectNLresult$lable <- 'delete'
  scanFrequency <- as.data.frame(table(selectNLresult$scanNum))
  for (i in c(1:nrow(scanFrequency))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(scanFrequency)))
    setTkProgressBar(pb, i*100/nrow(scanFrequency), sprintf("selectNLfliter (%s)", info),info)

    scanNumi <- scanFrequency$Var1[i]
    scanFreqi <- scanFrequency$Freq[i]
    scanNumiPosition <- which(selectNLresult$scanNum==scanNumi)

    if (scanFreqi==1){
      if (as.numeric(as.character(selectNLresult$intensity[scanNumiPosition])) >= intThreshold & 1/selectNLresult$numOfSugar[scanNumiPosition] >= ratioThreshold){
        selectNLresult[scanNumiPosition, ]$lable <- 'Only find biggest NL and the biggest NL only matched one result'
      }
    }
    else if (scanFreqi>1){
      NLFrequency <- as.data.frame(table(as.character(selectNLresult$neutralLoss[scanNumiPosition])))
      NLFrequency <- NLFrequency[sort(as.numeric(as.character(NLFrequency$Var1))*-1,index.return=TRUE)$ix,]
      if (nrow(NLFrequency)==1){
        selectNLresult[scanNumiPosition, ]$lable <- 'delete'
        keepPosi <- which(selectNLresult$scanNum==scanNumi &
                          selectNLresult$neutralLoss==as.character(NLFrequency$Var1[1]) &
                          as.numeric(as.character(selectNLresult$intensity)) >= intThreshold &
                          1/selectNLresult$numOfSugar >= ratioThreshold)
        if (length(keepPosi)==1){
          selectNLresult[keepPosi, ]$lable <- 'Only find biggest NL and the biggest NL matched mutiple results but only one has intensity exceeding the threshold'
        }
        else if (length(keepPosi) > 1){
          selectNLresult[keepPosi, ]$lable <- 'Only find biggest NL and the biggest NL matched mutiple results'
        }
      }
      else if (nrow(NLFrequency) > 1){
        for (j in c(1:nrow(NLFrequency))){
          NLVar1 <- NLFrequency$Var1[j]
          NLFreq <- NLFrequency$Freq[j]
          keepPosi <- which(selectNLresult$scanNum==scanNumi &
                            selectNLresult$neutralLoss==as.character(NLVar1) &
                            as.numeric(as.character(selectNLresult$intensity)) >= intThreshold)
          deletePosi <- which(selectNLresult$scanNum==scanNumi &
                              selectNLresult$neutralLoss==as.character(NLVar1) &
                              as.numeric(as.character(selectNLresult$intensity)) < intThreshold)
          if (length(keepPosi)==1){
            selectNLresult[keepPosi,]$lable <- 'delete'
            biggestNL <- selectNLresult[keepPosi,]
            sugarSubtraction <- as.matrix(subtractionOfDataframe(df1 = selectNLresult[,sugarStart:sugarEnd], df2row = biggestNL[,sugarStart:sugarEnd]))
            keepPosiLittleNL <- which(selectNLresult$scanNum==scanNumi &
                                      as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                      as.character(rowSums(sugarSubtraction)) == as.character(rowSums(abs(sugarSubtraction))))
            deletePosiLittleNL <- which(selectNLresult$scanNum==scanNumi &
                                        as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                        as.character(rowSums(sugarSubtraction)) != as.character(rowSums(abs(sugarSubtraction))))

            if (length(keepPosiLittleNL)==0 & 1/selectNLresult$numOfSugar[keepPosi] >= ratioThreshold){
              selectNLresult[keepPosi,]$lable <- 'Find the only biggest NL but no others matched to it'
              break
            }
            else if (length(keepPosiLittleNL) > 0 & (1+length(keepPosiLittleNL))/selectNLresult$numOfSugar[keepPosi] >= ratioThreshold){
              selectNLresult[keepPosi,]$lable <- 'Find the only biggest NL and has others matched to it'
              selectNLresult[keepPosiLittleNL,]$lable <- 'NL matched to biggest NL'
              break
            }
            else {
              next
            }
          }
          else if (length(keepPosi) > 1){
            selectNLresult[keepPosi[1],]$lable <- 'delete'
            biggestNL <- selectNLresult[keepPosi[1],]
            sugarSubtraction <- as.matrix(subtractionOfDataframe(df1 = selectNLresult[,sugarStart:sugarEnd], df2row = biggestNL[,sugarStart:sugarEnd]))
            keepPosiLittleNL <- which(selectNLresult$scanNum==scanNumi &
                                        as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                        as.character(rowSums(sugarSubtraction)) == as.character(rowSums(abs(sugarSubtraction))))
            deletePosiLittleNL <- which(selectNLresult$scanNum==scanNumi &
                                        as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                        as.character(rowSums(sugarSubtraction)) != as.character(rowSums(abs(sugarSubtraction))))

            if (length(keepPosiLittleNL)==0 & 1/selectNLresult$numOfSugar[keepPosi[1]] >= ratioThreshold){
              selectNLresult[keepPosi[1],]$lable <- 'Find the only biggest NL but no others matched to it'
            }
            else if (length(keepPosiLittleNL) > 0 & (1+length(keepPosiLittleNL))/selectNLresult$numOfSugar[keepPosi[1]] >= ratioThreshold){
              selectNLresult[keepPosi[1],]$lable <- 'Find the only biggest NL and has others matched to it'
              selectNLresult[keepPosiLittleNL,]$lable <- 'NL matched to biggest NL'
            }

            for (k in c(2:length(keepPosi))){
              biggestNL <- selectNLresult[keepPosi[k],]
              sugarSubtraction <- as.matrix(subtractionOfDataframe(df1 = selectNLresult[,sugarStart:sugarEnd], df2row = biggestNL[,sugarStart:sugarEnd]))
              keepPosiLittleNLk <- which(selectNLresult$scanNum==scanNumi &
                                          as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                          as.character(rowSums(sugarSubtraction)) == as.character(rowSums(abs(sugarSubtraction))))
              deletePosiLittleNLk <- which(selectNLresult$scanNum==scanNumi &
                                          as.numeric(as.character(selectNLresult$neutralLoss)) < as.numeric(as.character(biggestNL$neutralLoss)) &
                                          as.character(rowSums(sugarSubtraction)) != as.character(rowSums(abs(sugarSubtraction))))
              if ((1+length(keepPosiLittleNLk))/selectNLresult$numOfSugar[keepPosi[k]] >= ratioThreshold){
                if (length(keepPosiLittleNLk) > length(keepPosiLittleNL)){
                  if (length(keepPosiLittleNL)>0){
                    selectNLresult[keepPosiLittleNL,]$lable <- 'delete'
                  }
                  selectNLresult[keepPosi[k-1],]$lable <- 'delete'
                  selectNLresult[keepPosi[k],]$lable <- 'Find the only biggest NL and has others matched to it'
                  selectNLresult[keepPosiLittleNLk,]$lable <- 'NL matched to biggest NL'
                }
                else if (length(keepPosiLittleNLk) == 0 & length(keepPosiLittleNL) == 0){
                  selectNLresult[keepPosi[k-1],]$lable <- 'Mutiple biggest NL but no others matched to them'
                  selectNLresult[keepPosi[k],]$lable <- 'Mutiple biggest NL but no others matched to them'
                }
                else if (length(keepPosiLittleNLk) == length(keepPosiLittleNL)){
                  selectNLresult[keepPosi[k-1],]$lable <- 'Mutiple biggest NL and has others matched to them'
                  selectNLresult[keepPosi[k],]$lable <- 'Mutiple biggest NL and has others matched to them'
                  selectNLresult[keepPosiLittleNLk,]$lable <- 'NL matched to biggest NL'
                }
                else if (length(keepPosiLittleNLk) < length(keepPosiLittleNL)){
                  selectNLresult[keepPosi[k],]$lable <- 'delete'
                }
              }
            }
            break
          }
        }
      }
    }
  }
  close(pb)
  selectNLfliterResult <- selectNLresult[which(selectNLresult$lable != 'delete'),]
  return(selectNLfliterResult)
}
