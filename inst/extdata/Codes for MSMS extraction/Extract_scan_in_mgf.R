#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
path_file <- 'D:\\plantMS2软件试运行\\MSMS Extraction'
scan_num_file <- 'Scan.txt'
mgf_file_name <- "pos-mix-1-30-NCE.mgf"
mgf_path_filename <- paste0(path_file,"/",mgf_file_name)
scan_num_path_filename <- paste0(path_file,"/",scan_num_file)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd(path_file)

library(tcltk)
library(stringr)

text_mgf <- scan(mgf_path_filename, what = character(0), sep = "\n")
Begin_num <- grep("BEGIN IONS", text_mgf)
Scan_num <- grep("scan=",text_mgf)
End_num <- grep("END IONS", text_mgf)

text_scan_num <- scan(scan_num_path_filename, what = character(0), sep = "\n")

pb <- tkProgressBar("Read scan in mgf","?????? %", 0, 100)

result_mgf <- text_mgf[0]

csv_file = data.frame(Scan = 0,RTINSECONDS = 0,PEPMASS = 0, MS2 = 0)
len_mgf_num <- (1:length(Begin_num))

for (i in c(1:length(text_scan_num)))
{
  info<- sprintf("Scan %d%%", round(i*100/length(text_scan_num)))
  setTkProgressBar(pb, i*100/length(text_scan_num), sprintf("Scan (%s)", info),info)

  scan_num_i <- as.numeric(text_scan_num[i])
  ScanNumber_i <- paste0('ScanNumber:',scan_num_i,' Charge')
  scan_row_i <- grep(ScanNumber_i, text_mgf)
  posi <- which(Scan_num==scan_row_i)
  if (length(posi)==0){
    next()
  }
  else{
    result_mgf <- rbind(result_mgf, as.matrix(text_mgf[Begin_num[posi]:End_num[posi]]))
  }
}

#---"Close progress bar"
close(pb)

write.table(result_mgf,file='pos-mix-1-30-NCE-result.mgf', row.names=F,col.names = F, quote=F ,sep="\t")
