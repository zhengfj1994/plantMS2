#' @name mgfFilter
#' @description Filter mgf
#' @author Fujian Zheng <zhengfj@dicp.ac.cn>
#' @param rawMS2path
#' @param resultMS2path
#' @param diff.MS2MS1
#' @param ms2.intensity.big
#' @param ms2.intensity.small
#' @return None
#' @example mgfFilter(rawMS2path="D:/github/mgfFilter/Raw mgf", resultMS2path="D:/github/mgfFilter/Result", diff.MS2MS1=20, ms2.intensity.big=1, ms2.intensity.small=0)

# The main function

mgfFilter <- function(rawMS2path, resultMS2path, diff.MS2MS1, ms2.intensity.big, ms2.intensity.small){
  library(tcltk)
  library(stringr)
  require(tcltk)

  # Function: exact a matrix from mgf_data
  ##########
  createmgfmatrix <- function(mgf_data){
    Begin_num <- grep("BEGIN IONS", mgf_data)
    Pepmass_num <- grep("PEPMASS=",mgf_data)
    TR_num <- grep("RTINSECONDS=",mgf_data)
    End_num <- grep("END IONS", mgf_data)
    mgf_matrix <- cbind(Begin_num,TR_num,Pepmass_num,End_num)

    for (i in c(1:length(Pepmass_num)))
    {
      pepmass <- unlist(strsplit(mgf_data[Pepmass_num[i]], "="))[2]
      mgf_matrix[i,"Pepmass_num"] <- pepmass

      tr <- gsub("[^0-9,.]", "", mgf_data[TR_num[i]])
      mgf_matrix[i,"TR_num"] <- tr
    }
    return(mgf_matrix)
  }
  ##########

  mgfFiles <- list.files(rawMS2path)
  for (mgfi in mgfFiles){
    mgf_data <- scan(paste0(rawMS2path,"\\",mgfi), what = character(0), sep = "\n")

    # Delete the data by diff.MS2MS1 and ms2_intensity
    ########
    mgf_matrix <- createmgfmatrix(mgf_data)  # create mgf_matrix
    pb <- tkProgressBar(paste("Delete the data in", mgfi, "by diff.MS2MS1 and ms2_intensity"),"rate of progress %", 0, 100)
    for (i in c(1:length(mgf_data))){
      info<- sprintf("rate of progress %d%%", round(i*100/length(mgf_data)))
      setTkProgressBar(pb, i*100/length(mgf_data), sprintf(paste("Delete the data in", mgfi, "by diff.MS2MS1 and ms2_intensity (%s)"), info),info)

      if (is.na(mgf_data[i])){
        next()
      }

      if (!grepl("[a-zA-Z]", mgf_data[i])){

        ms2_start <- as.character(mgf_matrix[tail(which(as.numeric(mgf_matrix[,"Begin_num"]) < i),1),"Begin_num"])
        ms2_end <- as.character(mgf_matrix[tail(which(as.numeric(mgf_matrix[,"Begin_num"]) < i),1),"End_num"])

        int_ms2_biggest <- as.numeric(unlist(strsplit(mgf_data[as.numeric(ms2_start)+5], " "))[2])
        for (j in c(ms2_start:ms2_end)){
          if (!grepl("[a-zA-Z]", mgf_data[j])){
            int_ms2_j <- as.numeric(unlist(strsplit(mgf_data[j], " "))[2])
            if (int_ms2_j >= int_ms2_biggest){
              int_ms2_biggest <- int_ms2_j
            }
          }
        }

        mz_ms2 <- as.numeric(unlist(strsplit(mgf_data[i], " "))[1])
        int_ms2 <- as.numeric(unlist(strsplit(mgf_data[i], " "))[2])
        mz_int_ms1 <- as.character(mgf_matrix[tail(which(as.numeric(mgf_matrix[,"Begin_num"]) < i),1),"Pepmass_num"])
        mz_ms1 <- as.numeric(unlist(strsplit(mz_int_ms1, " "))[1])
        if (abs(mz_ms1-mz_ms2)/mz_ms1*1000000 <= diff.MS2MS1 & (int_ms2/int_ms2_biggest >= ms2.intensity.big | int_ms2/int_ms2_biggest <= ms2.intensity.small)){
          mgf_data[ms2_start:ms2_end] <- NA

        }
      }
    }
    close(pb)
    mgf_data <- na.omit(mgf_data)
    packageStartupMessage(paste("Deleting the data in", mgfi, "by diff.MS2MS1 and ms2_intensity is finished."))
    ########
    write.table(mgf_data,file=paste0(resultMS2path,"\\Filter ",mgfi), row.names=F,col.names = F, quote=F ,sep="\t")
  }
}
