rm(list = ls()); gc()
library(dplyr); library(ggplot2)

resultsList <- readRDS("./figs/fig2data.rds")
plotDF <- resultsList[["All Mines"]] %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )

resultsList <- readRDS("./figs/NL_pop_allmines.rds")

plotDF <- plotDF %>%
  mutate(outcomeType = "PM") %>%
  bind_rows(
    resultsList[["All Mines"]] %>%
      filter(outcomeType %in% c("NL", "pop")) 
  ) %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100")),
    ci_low  = att - critValue * se,
    ci_high = att + critValue * se
  )
levels(plotDF$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")


tmp <- plotDF %>%
  filter(leadLag %in% -2:-5) %>%
  mutate(exclude0 = ifelse(ci_low > 0 | ci_high < 0, "Y", "")) %>%
  select(outcomeType, dist, leadLag, att, ci_low, ci_high, exclude0)


knitr::kable(tmp, 
             "latex", booktabs = TRUE,
             # longtable = TRUE,
             align = "ccccc",
             digits = 4,
             linesep = c("", "", "", "\\addlinespace"), 
             col.names = c("Outcome", "Distance", "Lead/lag", "ATT", "CI (low)", "CI (high)", "Excludes 0?"),
             na = ""
)


