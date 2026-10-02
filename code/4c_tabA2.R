rm(list = ls()); gc()
library(dplyr); library(ggplot2)

# Main result (Fig 2)
# Absolute effect (A.3)
# Alternative mining onset (B.5)
# Active mines (B.7)
# No anticipation (D.1)
# Anticipation 2 (D.2)
# Spatial overlap (D.4)
# Balancing scheme (D.6)

col1 <- readRDS("./figs/fig2data.rds")[["All Mines"]]
col2 <- readRDS("./figs/nonLogPMdata.rds")[["All Mines"]] # A.3
col3 <- readRDS("./figs/minLossYearV2.rds")[["All Mines"]] # B.5
col4 <- readRDS("./figs/mausRob.rds")[["In Maus"]] # B.7
col5 <- readRDS("./figs/noAntPMdata.rds")[["All Mines"]]
col6 <- readRDS("./figs/anticipation2.rds")[["All Mines"]] %>% 
  filter(outcomeType == "PM")
col7 <- readRDS("./figs/spatialOverlap.rds")[["All Mines"]] %>% 
  filter(outcomeType == "PM")
col8 <- readRDS("./figs/balancingScheme.rds")[["All Mines"]]

tmpFun <- function(dframe) {
  
  out <- dframe %>%
    filter(dist == "5") %>%
    mutate(
      ci_low  = att - critValue * se,
      ci_high = att + critValue * se,
      exclude0 = ifelse(ci_low > 0 | ci_high < 0, "Y", "N")
    )  %>%
    select(leadLag, att, exclude0) %>%
    mutate(att = paste0(sprintf("%.3f", att), ifelse(exclude0 == "Y", "*", ""))) %>%
    select(-exclude0)
  
  out[, deparse(substitute(dframe))] <- out$att
  out <- out %>%
    select(-leadLag, -att)
  
  return(out)
  
} 
# tmpFun(col1)

outTable <- bind_cols(leadLag = -5:10,
  tmpFun(col1), tmpFun(col2), tmpFun(col3), tmpFun(col4),
          tmpFun(col5), tmpFun(col6), tmpFun(col7), tmpFun(col8))


knitr::kable(outTable, 
             "latex", booktabs = TRUE,
             # longtable = TRUE,
             align = "ccccccccc",
             # digits = 4,
             linesep = "", 
             col.names = c("Lead/lag", "Fig 2", "Fig A.3", "Fig B.5", "Fig B.7", 
                           "Fig D.1A", "Fig D.2", "Fig D.4", "Fig D.6")#,
             # na = ""
)

