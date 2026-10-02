rm(list = ls()); gc()
library(dplyr); library(ggplot2)
# two ways to do it: 
# v1: random year from 2001:2024
# v2: draw from the distribution of minLossYear, i.e., retain distribution of minLossYear

# first make EDA fig of minLossYears
minLossYear <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear)

# placebo1 <- readRDS("./output/models/logPM_5placebo1.rds")
placebo1 <- readRDS("./output/models/logPM_5placebo2.rds")

agg_list <- placebo1[[2]] # this is a list of length 200
length(agg_list)

resultsDTF <- data.frame(trial = rep(1:200, each = 16), leadLag = NA, att = NA, se = NA, critValue = NA)

for (i in 1:200) {
  tmp <- agg_list[[i]]
  whichtmp <- which(tmp$egt >= -5 & tmp$egt <= 10)
  
  resultsDTF[resultsDTF$trial == i, "leadLag"] <- tmp$egt[whichtmp]
  resultsDTF[resultsDTF$trial == i, "att"] <- tmp$att.egt[whichtmp]
  resultsDTF[resultsDTF$trial == i, "se"] <- tmp$se.egt[whichtmp]
  resultsDTF[resultsDTF$trial == i, "critValue"] <- tmp$crit.val.egt
}
# saveRDS(resultsDTF, file = "./figs/placebo1data.rds")
saveRDS(resultsDTF, file = "./figs/placebo2data.rds")

####################### making the actual fig ########################
rm(list = ls()); gc()
resultsDTF <- readRDS("./figs/placebo1data.rds") %>%
  mutate(leadLag = leadLag - .1,
         placeboV = 1) %>%
  bind_rows(readRDS("./figs/placebo2data.rds") %>%
              mutate(leadLag = leadLag + .1,
                     placeboV = 2))

# plotDF <- resultsDTF %>%
#   mutate(
#     ci_low  = att - critValue * se,
#     ci_high = att + critValue * se
#   )

fig2results <- readRDS("./figs/fig2data.rds")
fig2results <- fig2results[["All Mines"]]
fig2results <- fig2results %>%
  filter(dist == "5") %>%
  mutate(
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )

cols <- viridis::viridis(5)[c(2, 4)]  

pdf(paste0("./figs/placeboFig.pdf"), width = 6, height = 3.5)
ggplot(fig2results, aes(x = leadLag, y = att)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +   # moved to treatment time
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = -5:10,           # show every integer tick
    limits = c(-5.1, 10.1)        # restrict axis to window
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    x = "Time since mining onset",
    y = "ATT",
    # title = "All Mines",
    # title = "Large Mines"
    color = "Placebo\ntest"
  ) +  
  geom_point(data = resultsDTF, 
             alpha = .5, 
             aes(col = as.factor(placeboV))) +
  scale_color_manual(values = cols)  
dev.off()

