##########  baseline NL
rm(list = ls()); gc()
library(dplyr); library(ggplot2)
source("./cleanCode/aggregateFun.R")

resultsDTF <- data.frame(dist = rep(c("5", "5_20", "20_50", "50_100"), each = 16), leadLag = NA, att = NA, se = NA, critValue = NA, numMines = NA)

resultsList <- list(
  "NL baseline below median" = bind_rows(
    resultsDTF %>%
      mutate(outcomeType = "PM"),
    resultsDTF %>%
      mutate(outcomeType = "NL"),
    resultsDTF %>%
      mutate(outcomeType = "pop")),
  "NL baseline above median" = bind_rows(resultsDTF %>%
                                           mutate(outcomeType = "PM"),
                                         resultsDTF %>%
                                           mutate(outcomeType = "NL"),
                                         resultsDTF %>%
                                           mutate(outcomeType = "pop"))
)
outcomeType <- c("PM", "NL", "pop")
radius <- c("5", "5_20", "20_50", "50_100")
combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)
outFilePrefix <- ""
outFileSuffix <- "abvBelowNL100"

resultsList <- aggregateFun(combos, outFilePrefix, outFileSuffix) 
saveRDS(resultsList, file = "./figs/fig4data_abvBelowNL100.rds")


##### Fig 
rm(list = ls()); gc()

resultsList <- readRDS("./figs/fig4data_abvBelowNL100.rds")
plotDF <- resultsList[["NL baseline above median"]] %>%
  filter(outcomeType == "PM") %>%
  mutate(baseline = "Above") %>%
  bind_rows(resultsList[["NL baseline below median"]] %>%
              filter(outcomeType == "PM") %>%
              mutate(baseline = "Below")) %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")


# Colors for above/below baseline
baseline_colors <- viridis::viridis(2, option = "C")[c(2, 1)]  # option D is a good diverging set
names(baseline_colors) <- c("Above", "Below")

pdf(paste0("./figs/fig4_NLbaseline.pdf"), width = 12, height = 3.8)
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
    color = "Baseline NL",
    # title = "Event Study by Distance Band: Above vs Below Baseline"
    title = "PM 2.5 concentration"
  )
dev.off()


######### NL POP #######
# Colors for above/below baseline NL
baseline_colors <- viridis::viridis(2, option = "C")[c(2, 1)]  # option D is a good diverging set
names(baseline_colors) <- c("Above", "Below")

pdf(paste0("./figs/NLpop_byNLbaseline.pdf"), width = 12, height = 3.8) # when there is both a title and a legend, change height to 3.8
for (outcome in c("NL", "pop")) {
  plotDF <- resultsList[["NL baseline above median"]] %>%
    filter(outcomeType == outcome) %>%
    mutate(baseline = "Above") %>%
    bind_rows(resultsList[["NL baseline below median"]] %>%
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
      color = "Baseline NL",
      title = ifelse(outcome == "NL", "Nightlight intensity", "Population density") # change
    )
  gridExtra::grid.arrange(tmpPlot, nrow = 1)
}
dev.off()




plotDF <- resultsList[["NL baseline above median"]] %>%
  mutate(baseline = "Above") %>%
  bind_rows(resultsList[["NL baseline below median"]] %>%
              mutate(baseline = "Below")) %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )

