#' @name createPrecusorProductPair
#' @description Create precusor ion and product ion pairs
#' @author Fujian Zheng <zhenfj@dicp.ac.cn>
#'
#' @param mgfFile
#'
#' @return precusorAndProductPair
#' @example precusorAndProductPair <- createPrecusorProductPair(mgfFile="D:\\github\\plantMS2\\Example Data\\NEG-FullMS-ddMS2_flavoneinsilicolist1-skeleton3-SR-1.mgf")

createPrecusorProductPair <- function(mgfFile){
  require(tcltk)
  mgfMatrix <- createMgfMatrix(mgfFile)
  precusorAndProductPair <- mgfMatrix[0,]
  productIonInfo <- matrix(c(1:3),nrow=1,ncol=3,dimnames=list(c("r1"),c("productIon","intensityOfProductIon","neutralLoss")))
  precusorAndProductPair <- cbind(precusorAndProductPair,productIonInfo[0,])

  pb <- tkProgressBar("createPrecusorProductPair","Rate of progress %", 0, 100)
  for (i in c(1:nrow(mgfMatrix))){
    info<- sprintf("Rate of progress %d%%", round(i*100/nrow(mgfMatrix)))
    setTkProgressBar(pb, i*100/nrow(mgfMatrix), sprintf("createPrecusorProductPair (%s)", info),info)

    beginNum <- as.numeric(mgfMatrix[i, 'beginNum'])
    endNum <- as.numeric(mgfMatrix[i,'endNum'])

    if (str_detect(mgfData[beginNum+4],'CHARGE=')){
      addNum <- 5
    } else{
      addNum <- 4
    }
    if (str_detect(mgfData[beginNum+addNum],'END IONS')){
      next()
    }

    mgfDataMS2 <- ms2dataframe(mgfData[(beginNum+addNum):(endNum-1)])
    mgfDataMS2$intensity <- mgfDataMS2$intensity/max(mgfDataMS2$intensity)

    addMgfMatrix <- matrix(rep(mgfMatrix[i,],nrow(mgfDataMS2)),nrow = nrow(mgfDataMS2),byrow=T)
    colnames(addMgfMatrix) <- c("beginNum","scanNum","tr","pepmass","endNum")
    precusorAndProductPairi <- cbind(addMgfMatrix,mgfDataMS2)
    precusorAndProductPairi$neutralLoss <- as.numeric(as.character(precusorAndProductPairi$pepmass))-precusorAndProductPairi$productIon
    precusorAndProductPair <- rbind(precusorAndProductPair,precusorAndProductPairi)
  }
  close(pb)
  return(precusorAndProductPair)
}
