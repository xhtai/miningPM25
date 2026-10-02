aggregateFun <- function(combos, outFilePrefix, outFileSuffix) {
  for (i in seq_len(nrow(combos))) {
    cat(i, ", ")
    
    tmp <- readRDS(paste0("./output/models/", outFilePrefix, combos$outcomeType[i], "_", combos$radius[i], outFileSuffix, ".rds")) # change this 
    
    mod_list <- tmp[[1]]
    agg_list <- tmp[[2]]
    
    for (nm in names(resultsList)) { # All Mines, ... 
      
      tmpResultsDTF <- resultsList[[nm]]
      
      tmp <- agg_list[[nm]]
      whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
      
      tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "leadLag"] <- tmp$egt[whichtmp]
      tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "att"] <- tmp$att.egt[whichtmp]
      tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "se"] <- tmp$se.egt[whichtmp]
      tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "critValue"] <- tmp$crit.val.egt
      tmpResultsDTF[tmpResultsDTF$outcomeType == combos[i, "outcomeType"] & tmpResultsDTF$dist == combos[i, "radius"], "numMines"] <- mod_list[[nm]]$n
      
      resultsList[[nm]] <- tmpResultsDTF
    }
  }
  return(resultsList)
}
