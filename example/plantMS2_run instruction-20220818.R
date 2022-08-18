library(plantMS2)
NLmatrix <- createNLmatrix(sugarFile="D:\\plantMS2软件试运行\\plantMS2\\Sugar and Acid used for neutral loss-PlantMS2.xlsx", numOfSugar=6)
NLmatrix <- NLmatrix[which( 1 <= (as.data.frame(NLmatrix)$Hex + as.data.frame(NLmatrix)$dHex + as.data.frame(NLmatrix)$Pen+ as.data.frame(NLmatrix)$HexA) & as.data.frame(NLmatrix)$Mal <= 1 & as.data.frame(NLmatrix)$Caf <= 1 & as.data.frame(NLmatrix)$DDMP <= 1 & as.data.frame(NLmatrix)$Cou <= 1 & as.data.frame(NLmatrix)$Fur <= 1 & as.data.frame(NLmatrix)$Sin <= 1 & (as.data.frame(NLmatrix)$Mal + as.data.frame(NLmatrix)$Cou + as.data.frame(NLmatrix)$Fur + as.data.frame(NLmatrix)$Caf + as.data.frame(NLmatrix)$Sin + as.data.frame(NLmatrix)$DDMP) <= 1), ]
precusorAndProductPair <- createPrecusorProductPair(mgfFile="D:\\plantMS2软件试运行\\plantMS2\\neg-1-398-35CE.mgf",intThreshold = 0.01)
selectNLresult <- selectNL(sugarFile = "D:\\plantMS2软件试运行\\plantMS2\\Sugar and Acid used for neutral loss-PlantMS2.xlsx", precusorAndProductPair,NLmatrix,deltaMZppm=10,ionMode = "N")
selectNLfliterResult <- selectNLfliter(selectNLresult, ratioThreshold = 0.001)
write.csv(selectNLfliterResult,file = "D:\\plantMS2软件试运行\\plantMS2\\selectNLfliterResult-neg-1-398-35CE.csv")
ms1ms2ConbRes <- ms1ms2Conbination(ms1File="D:\\plantMS2软件试运行\\plantMS2\\Peak table-neg-1-398-35CE.xlsx", selectNLfliterResult, deltaMZppm=10, deltaTR=0.2)
write.csv(ms1ms2ConbRes,file = "D:\\plantMS2软件试运行\\plantMS2\\ms1ms2ConbinationResult-neg-1-398-35CE.csv")
ms1IdentificationRes <- ms1Identification(ms1ms2ConbRes, aglyconeFile="D:\\plantMS2软件试运行\\plantMS2\\in silico aglyconeDB.xlsx", ionMode='N', deltaMZppm=10)
write.csv(ms1IdentificationRes, file = "D:\\plantMS2软件试运行\\plantMS2\\ms1IdentificationRes-neg-1-398-35CE.csv")
proc.time()-ptm