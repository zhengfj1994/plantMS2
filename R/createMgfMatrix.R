#' @name createMgfMatrix
#' @description Use mgf file to create a matrix
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param mgfFile
#'
#' @return mgfMatrix mgfData
#' @example mgfMatrix <- createMgfMatrix(mgfFile="D:\\github\\plantMS2\\Example Data\\NEG-FullMS-ddMS2_flavoneinsilicolist1-skeleton3-SR-1.mgf")

createMgfMatrix <- function(mgfFile){
  require(stringr)
  mgfData <<- scan(mgfFile, what = character(0), sep = "\n")
  beginNum <- grep("BEGIN IONS", mgfData)
  scanNum <- grep("scan=",mgfData)
  pepmass <- grep("PEPMASS=",mgfData)
  tr <- grep("RTINSECONDS=",mgfData)
  endNum <- grep("END IONS", mgfData)
  mgfMatrix <- cbind(beginNum,scanNum,tr,pepmass,endNum)

  for (i in c(1:length(pepmass)))
  {
    pepmassi <- gsub("[^0-9,.]", "", strsplit(mgfData[pepmass[i]],split = " ")[[1]][1])
    mgfMatrix[i,"pepmass"] <- pepmassi

    scani <- gsub("[^0-9,.]", "", str_extract_all(mgfData[scanNum[i]],"ScanNumber:([0-9]*) Charge:")[[1]])
    mgfMatrix[i,"scanNum"] <- scani

    tri <- gsub("[^0-9,.]", "", mgfData[tr[i]])
    mgfMatrix[i,"tr"] <- tri
  }
  return(mgfMatrix)
}
##########
