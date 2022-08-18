#' @name createNLmatrix
#' @description Creat a matrix containing theoretical neutral loss.
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#'
#' @param sugarFile, a xlsx file containing the information od sugars, the first column is 'name', the second column is 'foluma' and the third is 'molecular weight'.
#' @param numOfSugar, the length of the chain of sugar.
#'
#' @return NLmatrix
#' @example NLmatrix <- createNLmatrix(sugarFile="D:\\github\\plantMS2\\Example Data\\Sugar used for neutral loss.xlsx", numOfSugar=6)

createNLmatrix <- function(sugarFile, numOfSugar){
  require(gtools)
  require(openxlsx)
  sugarsInFile <- openxlsx::read.xlsx(xlsxFile =  sugarFile, sheet = 1)
  nrowOfSugar <- nrow(sugarsInFile)
  NLmatrix <- as.data.frame(permutations(n = numOfSugar+1, r = 1, v = 0:numOfSugar, repeats.allowed = TRUE))
  NLmatrix$rowsum <- apply(NLmatrix,1,sum)
  NLmatrix <- subset(NLmatrix[which(NLmatrix$rowsum<numOfSugar+1),], select = -rowsum)

  count <- 1
  while (count < nrowOfSugar){
    add <- rep(0,nrow(NLmatrix))
    NLmatrix2 <- cbind(add, NLmatrix)
    for (i in c(1:numOfSugar)){
      add <- rep(i,nrow(NLmatrix))
      NLmatrix2 <- rbind(NLmatrix2, cbind(add, NLmatrix))
      NLmatrix2$rowsum <- apply(NLmatrix2,1,sum)
      NLmatrix2 <- subset(NLmatrix2[which(NLmatrix2$rowsum<numOfSugar+1),], select = -rowsum)
    }
    NLmatrix <- NLmatrix2
    count <- count + 1
  }
  NLmatrix <- as.matrix(NLmatrix)[-1,]
  MW <- as.numeric(as.character(sugarsInFile$molecular.weight))
  mzOfChain <- NLmatrix %*% MW - 18.0105647 * rowSums(NLmatrix)
  NLmatrix <- cbind(NLmatrix, mzOfChain)

  row <- c(1:nrow(NLmatrix))
  column <- c(as.character(sugarsInFile$name), "mzOfChain")
  dimnames(NLmatrix)=list(row,column)
  return(NLmatrix)
}







