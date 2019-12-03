####################################################################
ptm <- proc.time()
library(plantMS2)
NLmatrix <- createNLmatrix(sugarFile="D:\\github\\plantMS2\\Example Data\\Sugar used for neutral loss.xlsx", numOfSugar=6)
precusorAndProductPair <- createPrecusorProductPair(mgfFile="D:\\github\\plantMS2\\Example Data\\NEG-FullMS-ddMS2_flavoneinsilicolist1-skeleton3-SR-1.mgf")
selectNLresult <- selectNL(precusorAndProductPair,NLmatrix,deltaMZppm=20)
#selectNLfliterResult <- selectNLfliter(selectNLresult)
selectNLfliterResult <- selectNLfliter2(selectNLresult, intThreshold = 0.05, ratioThreshold = 0.5)
write.csv(selectNLfliterResult,file = "D:\\github\\plantMS2\\Example Data\\selectNLfliterResult.csv")

ms1ms2ConbRes <- ms1ms2Conbination(ms1File="D:\\github\\plantMS2\\Example Data\\peak table-Features-Skeleton3.xlsx", selectNLfliterResult, deltaMZppm=10, deltaTR=0.2)
write.csv(ms1ms2ConbRes,file = "D:\\github\\plantMS2\\Example Data\\ms1ms2ConbinationResult.csv")

ms1IdentificationRes <- ms1Identification(ms1ms2ConbRes, aglyconeFile="D:\\github\\plantMS2\\Example Data\\aglyconeDB.xlsx", ionMode='N', deltaMZppm=10)
write.csv(ms1IdentificationRes, file = "D:\\github\\plantMS2\\Example Data\\ms1IdentificationRes.csv")

proc.time()-ptm
#######################################################################################################################
