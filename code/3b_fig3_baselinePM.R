####################### FIG 3 ########################
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list(
  "PM2.5 baseline below median" = resultsDTF,
  "PM2.5 baseline above median" = resultsDTF
  ) 

outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

outFilePrefix <- ""
outFileSuffix <- "altBaseline"

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  tmp <- readRDS(paste0("./output/models/", outFilePrefix, combos$outcomeType[i], "_", combos$radius[i], outFileSuffix, ".rds")) # change this 
  mod_list <- tmp[[1]]
  agg_list <- tmp[[2]]
  
  for (nm in names(resultsList)) { 
    
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

saveRDS(resultsList, file = "./figs/altBaseline.rds")

#############################
rm(list = ls()); gc()
resultsList <- readRDS("./figs/altBaseline.rds")

plotDF <- resultsList[["PM2.5 baseline above median"]] %>%
  mutate(baseline = "Above") %>%
  bind_rows(resultsList[["PM2.5 baseline below median"]] %>%
              mutate(baseline = "Below")) %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")


# Colors for above/below baseline
baseline_colors <- viridis::viridis(3, option = "D")[1:2]  # option D is a good diverging set
names(baseline_colors) <- c("Above", "Below")

pdf(paste0("./figs/fig3.pdf"), width = 12, height = 3.8)
ggplot(plotDF, aes(x = leadLag, y = att, color = baseline, fill = baseline)) +
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
    y = "ATT (proportional change)",
    color = "Baseline PM2.5",
    # title = "Event Study by Distance Band: Above vs Below Baseline"
    title = "PM 2.5 concentration"
  )
dev.off()


tmp <- plotDF %>%
  filter(baseline == "Above" & leadLag >= 0) 
min(tmp$att) # -0.05616757
max(tmp$att) # -0.02738219
exp(min(tmp$att)) - 1 # -0.05461929
exp(max(tmp$att)) - 1 # -0.02701069


tmp <- plotDF %>%
  filter(baseline == "Below") 

exp(0.0194680997) - 1 # 0.01965884
exp(0.01600972662) - 1 # 0.01613857

exp(0.017) - 1 #  0.01386384


####################### NL / pop ########################
# this one for above vs. below baseline 
rm(list = ls()); gc()
resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list(
  "PM2.5 baseline below median" = bind_rows(resultsDTF %>%
                                              mutate(outcomeType = "NL"),
                                            resultsDTF %>%
                                              mutate(outcomeType = "pop")),
  "PM2.5 baseline above median" = bind_rows(resultsDTF %>%
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
outFileSuffix <- "abvBelowPM100"
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

saveRDS(resultsList, file = "./figs/fig3bc_data.rds")

#############################
rm(list = ls()); gc()
resultsList <- readRDS("./figs/fig3bc_data.rds")

# Colors for above/below baseline
baseline_colors <- viridis::viridis(3, option = "D")[1:2]  # option D is a good diverging set
names(baseline_colors) <- c("Above", "Below")

pdf(paste0("./figs/NLpop_byBaseline.pdf"), width = 12, height = 3.8) # when there is both a title and a legend, change height to 3.8
for (outcome in c("NL", "pop")) {
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
      x = "Time since mining onset", # change x 
      y = ifelse(outcome == "NL", "ATT (digital number)", expression(ATT~(pop/km^2))),
      color = "Baseline PM2.5",
      title = ifelse(outcome == "NL", "Nightlight intensity", "Population density") # change
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()


