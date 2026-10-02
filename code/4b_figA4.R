### this code: 
# remove high-income above mines (1000 or so)
  # heterogeneity: above below baseline PM 2.5, by income

# outcome: PM 5, 5-20, 20-50, 50-100
# outfiles: output/models/

rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./cleanCode/myFun.R")

conditions <- list(
  "All Mines" =
    quote(
      Shape_Area > 0
    ),
  "PM2.5 baseline below median" =
    quote(
      baselinePM25_100km <= medianPM    ),
  "PM2.5 baseline above median" =
    quote(
      baselinePM25_100km > medianPM & Income != "High income"
    )#,
)

outcomeType <- c("PM"#, "NL", "pop"
                 )
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i],
    conditions = conditions,
    outFilePrefix = "", 
    outFileSuffix = "removeHighAbove"
  )
}

####################### making the actual fig ########################
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- vector(mode = "list", length = 3)
resultsList <- list(
  "All Mines" = bind_rows(resultsDTF %>%
                            mutate(outcomeType = "PM")#,
                          # resultsDTF %>%
                          #   mutate(outcomeType = "NL"),
                          # resultsDTF %>%
                          #   mutate(outcomeType = "pop")
                          ),
  "PM2.5 baseline below median" = bind_rows(resultsDTF %>%
                                              mutate(outcomeType = "PM")
                                            # ,
                                            # resultsDTF %>%
                                            #   mutate(outcomeType = "NL"),
                                            # resultsDTF %>%
                                            #   mutate(outcomeType = "pop")
                                            ),
  "PM2.5 baseline above median" = bind_rows(resultsDTF %>%
                                              mutate(outcomeType = "PM")
                                            # ,
                                            # resultsDTF %>%
                                            #   mutate(outcomeType = "NL"),
                                            # resultsDTF %>%
                                            #   mutate(outcomeType = "pop")
                                            )
)
outcomeType <- c("PM"#, "NL", "pop"
                 )
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

outFilePrefix <- ""
outFileSuffix <- "removeHighAbove"

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

saveRDS(resultsList, file = "./figs/rob_removeHighAbv.rds")

############### PM abv/below baseline
rm(list = ls()); gc()
resultsList <- readRDS("./figs/rob_removeHighAbv.rds")
# Colors for above/below baseline
baseline_colors <- viridis::viridis(3, option = "D")[1:2]  # option D is a good diverging set
names(baseline_colors) <- c("Above", "Below")

pdf(paste0("./figs/robRmHiAbv_abvBelow.pdf"), width = 12, height = 3.5)
for (outcome in c("PM"
                  #, "NL", "pop"
                  )) {
  plotDF <- resultsList[["PM2.5 baseline above median"]] %>%
    filter(outcomeType == outcome) %>%
    mutate(baseline = "Above") %>%
    bind_rows(resultsList[["PM2.5 baseline below median"]] %>%
                filter(outcomeType == outcome) %>%
                mutate(baseline = "Below")) %>%
    mutate(
      dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
      ci_low  = att - critValue * se,
      ci_high = att + critValue * se
    )
  levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")
  
  tmpPlot <- ggplot(plotDF, aes(x = leadLag, y = att, color = baseline, fill = baseline)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, group = baseline),
                alpha = 0.2, color = NA,
                show.legend = FALSE) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5,
               show.legend = FALSE) +
    
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    facet_wrap(~ dist, nrow = 1, scales = "fixed") +  # Four distance panels
    scale_color_manual(values = baseline_colors) +
    scale_fill_manual(values = baseline_colors) +
    
    scale_x_continuous(breaks = -5:10, limits = c(-5, 10)) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(
      x = "Time since mining onset",
      y = "ATT",
      color = "Baseline PM2.5",
      # title = outcome
      title = ifelse(outcome == "PM", "", ifelse(outcome == "NL", "Nightlight intensity", "Population density"))
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()


############################ CODE FOR BY INCOME FIG ############################
rm(list = ls()); gc()
source("./cleanCode/myFun.R")

conditions <- list(
  "High income" =
    quote(
      Income == "High income" & baselinePM25_100km <= medianPM
    )
)

outcomeType <- c("PM"#, 
  # "NL", "pop"
  )
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i],
    conditions = conditions,
    outFilePrefix = "", 
    outFileSuffix = "highInc_removeHighAbove"
  )
}

###################### by income
# keep the results for upper-middle and lower; re-run high income only, removing above-median mines 
rm(list = ls()); gc()

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- vector(mode = "list", length = 1)
resultsList <- list(
  "High income" = bind_rows(resultsDTF %>%
                              mutate(outcomeType = "PM"))
)
outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

outFilePrefix <- ""
outFileSuffix <- "highInc_removeHighAbove"

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
resultsList2 <- readRDS("./figs/LMIC.rds") # this is PM only
resultsList2$`High income` <- resultsList$`High income` # only high income is different; keep the upper middle and lower income lines
resultsList <- resultsList2
saveRDS(resultsList, file = "./figs/rob_removeHighAbv_income.rds")

############### code for fig
rm(list = ls()); gc()
resultsList <- readRDS("./figs/rob_removeHighAbv_income.rds")

# Colors for above/below baseline
baseline_colors <- viridis::viridis(4, option = "E")[c(3:1)]  # option D is a good diverging set
names(baseline_colors) <- c("High", "Upper-middle", "Lower-middle and low")

pdf(paste0("./figs/robRmHiAbv_income.pdf"), width = 12, height = 3.5)
plotDF <- resultsList[["High income"]] %>%
    # filter(outcomeType == outcome) %>%
    mutate(baseline = "High") %>%
    bind_rows(resultsList[["Upper middle income"]] %>%
                # filter(outcomeType == outcome) %>%
                mutate(baseline = "Upper-middle")) %>%
    # bind_rows(resultsList[["Lower income"]] %>%
                # filter(outcomeType == outcome) %>%
                # mutate(baseline = "Lower-middle and low")) %>%
    mutate(
      dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
      ci_low  = att - critValue * se,
      ci_high = att + critValue * se,
      baseline = factor(baseline, levels = c("High", "Upper-middle", "Lower-middle and low")) # needs this line to retain the order in the legend (if not will become alphabetical)
    )
  levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")
  
  tmpPlot <- ggplot(plotDF, aes(x = leadLag, y = att, color = baseline, fill = baseline
  )) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high#, group = baseline
    ),
    alpha = 0.2, color = NA,
    show.legend = FALSE) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5,
               show.legend = FALSE) +
    
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    facet_wrap(~ dist, nrow = 1, scales = "fixed") +  # Four distance panels
    scale_color_manual(values = baseline_colors) +
    scale_fill_manual(values = baseline_colors) +
    
    scale_x_continuous(breaks = -5:10, limits = c(-5, 10)) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(
      x = "Time since mining onset",
      y = "ATT",
      color = "Income"#,
      # title = outcome
    )
gridExtra::grid.arrange(tmpPlot, nrow = 1)

dev.off()

