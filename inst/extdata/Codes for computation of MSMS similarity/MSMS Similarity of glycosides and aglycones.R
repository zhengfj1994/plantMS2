######################
mgfFile = "D:\\plantMS2软件试运行\\MSMS similarity of glycosides and aglycones\\neg1-509.mgf"
xlsxFile = "D:\\plantMS2软件试运行\\MSMS similarity of glycosides and aglycones\\Sim.xlsx"
ionMode = "N"
intThreshold1_Exp <- 0.005
intThreshold2_DB <- 0.01
ppm = 10
resultFile <- "D:\\plantMS2软件试运行\\MSMS similarity of glycosides and aglycones\\Sim-result.csv"

createMgfMatrix <- function(mgfFile){
  require(stringr)
  mgfData <<- scan(mgfFile, what = character(0), sep = "\n")
  beginNum <- grep("BEGIN IONS", mgfData)
  scanNum <- grep("scan=",mgfData)
  pepmass <- grep("PEPMASS=",mgfData)
  tr <- grep("RTINSECONDS=",mgfData)
  endNum <- grep("END IONS", mgfData)
  mgfMatrix <- as.data.frame(cbind(beginNum,scanNum,tr,pepmass,endNum))

  for (i in c(1:length(pepmass)))
  {
    pepmassi <- gsub("[^0-9,.]", "", strsplit(mgfData[pepmass[i]],split = " ")[[1]][1])
    mgfMatrix[i,"pepmass"] <- pepmassi

    scani <- gsub("[^0-9,.]", "", str_extract_all(mgfData[scanNum[i]],"scan=([0-9]*)")[[1]])
    mgfMatrix[i,"scanNum"] <- scani

    tri <- gsub("[^0-9,.]", "", mgfData[tr[i]])
    mgfMatrix[i,"tr"] <- tri
  }
  return(mgfMatrix)
}

mgfMatrix <- createMgfMatrix(mgfFile = mgfFile)
sheet1 <- openxlsx::read.xlsx(xlsxFile = xlsxFile, sheet = 1)
sheet2 <- openxlsx::read.xlsx(xlsxFile = xlsxFile, sheet = 2)


if (ionMode == "P"){
  sheet2$`m/z.of.aglycone` <- sheet2$`m/z.of.aglycone` + 1.007276
} else if (ionMode == "N"){
  sheet2$`m/z.of.aglycone` <- sheet2$`m/z.of.aglycone` - 1.007276
} else {
  packageStartupMessage("Wrong!")
}

for (ithRow in c(1:nrow(sheet1))){
  ithScanNum <- sheet1$Scan[ithRow]
  ithName <- sheet1$Algycone[ithRow]
  ithNameID <- which(sheet2$Aglycone == ithName)
  ithMZ <- sheet2$`m/z.of.aglycone`[ithNameID]
  ithExpMS2ID <- which(mgfMatrix$scanNum == ithScanNum)
  ithExpMS2 <- mgfData[mgfMatrix$beginNum[ithExpMS2ID]:mgfMatrix$endNum[ithExpMS2ID]]
  ithExpMS2 <- ithExpMS2[which(!grepl("[a-zA-Z]", ithExpMS2))]
  ithExpMS2 <- as.data.frame(do.call("rbind", strsplit(ithExpMS2, split = " ")))
  colnames(ithExpMS2) <- c("mz", "intensity")
  ithExpMS2 <- as.data.frame(lapply(ithExpMS2,as.numeric))

  #ithExpMS2 <- ithExpMS2[which(ithExpMS2$mz < ithMZ),]
  ithExpMS2 <- ithExpMS2[which(ithExpMS2$intensity >= max(ithExpMS2$intensity) * intThreshold1_Exp),]

  ithDBMS2_1 <- sheet2$Energy1[ithNameID]
  ithDBMS2_1 <- as.data.frame(lapply(as.data.frame(do.call("rbind", strsplit(unlist(strsplit(ithDBMS2_1, split = " ")), split = ":"))),as.numeric))
  colnames(ithDBMS2_1) <- c("mz", "intensity")
  ithDBMS2_1 <- ithDBMS2_1[which(ithDBMS2_1$intensity >= max(ithDBMS2_1$intensity) * intThreshold2_DB),]
  ithDBMS2_2 <- sheet2$Energy2[ithNameID]
  ithDBMS2_2 <- as.data.frame(lapply(as.data.frame(do.call("rbind", strsplit(unlist(strsplit(ithDBMS2_2, split = " ")), split = ":"))),as.numeric))
  colnames(ithDBMS2_2) <- c("mz", "intensity")
  ithDBMS2_2 <- ithDBMS2_2[which(ithDBMS2_2$intensity >= max(ithDBMS2_2$intensity) * intThreshold2_DB),]
  ithDBMS2_3 <- sheet2$Energy3[ithNameID]
  ithDBMS2_3 <- as.data.frame(lapply(as.data.frame(do.call("rbind", strsplit(unlist(strsplit(ithDBMS2_3, split = " ")), split = ":"))),as.numeric))
  colnames(ithDBMS2_3) <- c("mz", "intensity")
  ithDBMS2_3 <- ithDBMS2_3[which(ithDBMS2_3$intensity >= max(ithDBMS2_3$intensity) * intThreshold2_DB),]

  count1 <- 0
  for (ithDBIon in c(1:nrow(ithDBMS2_1))){
    ithDBMz <- ithDBMS2_1$mz[ithDBIon]
    matchID <- which(abs(ithDBMz - ithExpMS2$mz)/ithExpMS2$mz * 1000000 <= 10)
    if (length(matchID) > 0){
      count1 <- count1 + 1
    }
  }

  count2 <- 0
  for (ithDBIon in c(1:nrow(ithDBMS2_2))){
    ithDBMz <- ithDBMS2_2$mz[ithDBIon]
    matchID <- which(abs(ithDBMz - ithExpMS2$mz)/ithExpMS2$mz * 1000000 <= ppm)
    if (length(matchID) > 0){
      count2 <- count2 + 1
    }
  }

  count3 <- 0
  for (ithDBIon in c(1:nrow(ithDBMS2_2))){
    ithDBMz <- ithDBMS2_3$mz[ithDBIon]
    matchID <- which(abs(ithDBMz - ithExpMS2$mz)/ithExpMS2$mz * 1000000 <= ppm)
    if (length(matchID) > 0){
      count3 <- count3 + 1
    }
  }

  sheet1$"Sim_Ions_Num_Energy1"[ithRow] <- count1
  sheet1$"Sim_Energy1"[ithRow] <- count1/nrow(ithDBMS2_1)
  # sheet1$"Totle_Num_Energy1"[ithRow] <- nrow(ithDBMS2_1)

  sheet1$"Sim_Ions_Num_Energy2"[ithRow] <- count2
  sheet1$"Sim_Energy2"[ithRow] <- count2/nrow(ithDBMS2_2)
  # sheet1$"Totle_Num_Energy2"[ithRow] <- nrow(ithDBMS2_2)

  sheet1$"Sim_Ions_Num_Energy3"[ithRow] <- count3
  sheet1$"Sim_Energy3"[ithRow] <- count3/nrow(ithDBMS2_3)
  # sheet1$"Totle_Num_Energy3"[ithRow] <- nrow(ithDBMS2_3)
}
write.csv(sheet1, file = resultFile)


