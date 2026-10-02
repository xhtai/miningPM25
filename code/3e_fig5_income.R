####################### FIG 5 ########################
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list( 
  "High income" = resultsDTF,
  #= #bind_rows(resultsDTF %>%
  # mutate(outcomeType = "PM"))#,
  # resultsDTF %>%
  #   mutate(outcomeType = "NL"),
  # resultsDTF %>%
  #   mutate(outcomeType = "pop"))
  #,
  "Upper middle income" = resultsDTF#,
  # "Lower income" = resultsDTF
)

outcomeType <- c("PM"#, 
                 # "NL", "pop"
)
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  if (combos$outcomeType[i] == "PM") {
    tmp <- readRDS(paste0("./output/models/log", combos$outcomeType[i], "_", combos$radius[i], "LMIC.rds"))
  } 
  mod_list <- tmp[[1]]
  agg_list <- tmp[[2]]
  
  for (nm in names(resultsList)) { # All Mines, ... 
    
    tmpResultsDTF <- resultsList[[nm]]
    
    tmp <- agg_list[[nm]]
    whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
    
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "leadLag"] <- tmp$egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "att"] <- tmp$att.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "se"] <- tmp$se.egt[whichtmp]
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "critValue"] <- tmp$crit.val.egt
    tmpResultsDTF[tmpResultsDTF$dist == combos[i, "radius"], "numMines"] <- mod_list[[nm]]$n
    
    resultsList[[nm]] <- tmpResultsDTF
  }
  
}

saveRDS(resultsList, file = "./figs/LMIC.rds") 

#############################
rm(list = ls()); gc()

resultsList <- readRDS("./figs/LMIC.rds")
baseline_colors <- viridis::viridis(4, option = "E")[c(3:1)]  # option D is a good diverging set
names(baseline_colors) <- c("High", "Upper-middle")

plotDF <- resultsList[["High income"]] %>%
  # filter(outcomeType == outcome) %>%
  mutate(baseline = "High") %>%
  bind_rows(resultsList[["Upper middle income"]] %>%
              # filter(outcomeType == outcome) %>%
              mutate(baseline = "Upper-middle")) %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se,
    baseline = factor(baseline, levels = c("High", "Upper-middle")) 
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")

pdf(paste0("./figs/PM_byIncome.pdf"), width = 12, height = 3.8)
ggplot(plotDF, aes(x = leadLag, y = att, color = baseline, fill = baseline
)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, group = baseline
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
    y = "ATT (proportional change)",
    color = "Income",
    title = "PM 2.5 concentration"
  )
# gridExtra::grid.arrange(tmpPlot, nrow = 1)
dev.off()


####################### pop / NL ########################
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list(
  "High income" = bind_rows(resultsDTF %>%
                              mutate(outcomeType = "NL"),
                            resultsDTF %>%
                              mutate(outcomeType = "pop")),
  "Upper middle income" = bind_rows(resultsDTF %>%
                                      mutate(outcomeType = "NL"),
                                    resultsDTF %>%
                                      mutate(outcomeType = "pop"))
)
outcomeType <- c("NL", "pop")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)


outFilePrefix <- ""
outFileSuffix <- "byIncome"
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

saveRDS(resultsList, file = "./figs/fig5data_byIncome.rds")

#############################
rm(list = ls()); gc()
resultsList <- readRDS("./figs/fig5data_byIncome.rds")

baseline_colors <- viridis::viridis(4, option = "E")[c(3:2)]  # option D is a good diverging set
names(baseline_colors) <- c("High", "Upper-middle")


pdf(paste0("./figs/NLpop_byIncome.pdf"), width = 12, height = 3.8)
for (outcome in c("NL", "pop")) {
  plotDF <- resultsList[["High income"]] %>%
    filter(outcomeType == outcome) %>%
    mutate(baseline = "High") %>%
    bind_rows(resultsList[["Upper middle income"]] %>%
                filter(outcomeType == outcome) %>%
                mutate(baseline = "Upper-middle")) %>%
    mutate(
      dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
      ci_low  = att - critValue * se,
      ci_high = att + critValue * se,
      baseline = factor(baseline, levels = c("High", "Upper-middle"))
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
      y = ifelse(outcome == "NL", "ATT (digital number)", expression(ATT~(pop/km^2))),
      color = "Income",
      title = ifelse(outcome == "NL", "Nightlight intensity", "Population density")
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()

