rm(list = ls()); gc()
radius <- "50_100" #"20_50" #"5_20" # "5" #  

tmp <- readRDS(paste0("./data/bufferDonut_", radius, "_PM.rds")) %>% 
  select(-centroid)
# then the col names are OBJECTID, pm25_1998 through pm25_2023
tmp <- tmp %>%
  rename_with(
    ~ sub("^pm25_", "outcome_", .x),
    starts_with("pm25_")
  )

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds") %>%
  select(OBJECTID, minLossYear, Shape_Area)

yearlyPM25 <- yearlyPM25_wide %>%
  left_join(tmp) %>%
  filter(!is.na(minLossYear)) %>%
  tidyr::pivot_longer(
    cols = starts_with("outcome_"),
    names_to = "year",
    names_prefix = "outcome_",
    names_transform = list(year = as.numeric),
    values_to = "outcome"
  )
### ADD THIS
yearlyPM25 <- yearlyPM25 %>%
  filter(outcome > 0) %>%
  mutate(outcome = log(outcome))

yearlyPM25 <- yearlyPM25 %>%
  mutate(OBJECTID = as.factor(OBJECTID))

# yearlyPM25$event_time <- yearlyPM25$year - yearlyPM25$minLossYear

yearlyPM25$g_shift <- yearlyPM25$minLossYear - 1 # default doesn't allow anticipation, so shift manually
# estimates horizons >= 0 only

yearlyPM25$event_time <- yearlyPM25$year - yearlyPM25$g_shift

Sys.time()
es <- didimputation::did_imputation(
  data = yearlyPM25 %>%
    filter(event_time %in% -5:11), yname = "outcome", gname = "g_shift",
  tname = "year", idname = "OBJECTID",
  horizon = TRUE, pretrends = -3:-1
  # actual event time: -6 to 10, pre-trends -4 to -2, estimates horizons -1 to 10
)
Sys.time()


es$term <- as.character(as.numeric(es$term) - 1)
# 25 minutes using all event_times

saveRDS(es, file = "./output/models/borusyak.rds")
saveRDS(es, file = "./output/models/borusyak_5_20.rds")
saveRDS(es, file = "./output/models/borusyak_20_50.rds")
saveRDS(es, file = "./output/models/borusyak_50_100.rds")

###################################
rm(list = ls()); gc()
pts <- readRDS("./output/models/borusyak.rds") %>%
  mutate(dist = "5") %>%
  bind_rows(
    readRDS("./output/models/borusyak_5_20.rds") %>%
      mutate(dist = "5_20")
  ) %>%
  bind_rows(
    readRDS("./output/models/borusyak_20_50.rds") %>%
      mutate(dist = "20_50")
  ) %>%
  bind_rows(
    readRDS("./output/models/borusyak_50_100.rds") %>%
      mutate(dist = "50_100")
  ) %>%
  mutate(
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error,
    rel_year = as.numeric(term)
  )

pts <- pts %>%
  mutate(
    dist = factor(dist, levels = c("5", "5_20", "20_50", "50_100"))
  )
levels(pts$dist) <- c("0-5 km", "5-20 km", "20-50 km", "50-100 km")


pdf(paste0("./figs/fig_borusyak_alldists.pdf"), width = 12, height = 3.5)
ggplot() +
  # 0 effect
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # Confidence Intervals
  geom_linerange(data = pts, mapping = aes(x = rel_year, ymin = ci_lower, ymax = ci_upper), color = "black") +
  # Estimates
  geom_point(data = pts, mapping = aes(x = rel_year, y = estimate), size = 2) +
  facet_wrap(~ dist, nrow = 1, scales = "fixed") +  # Four distance panels
  scale_x_continuous(breaks = -4:10) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(x = "Time since mining onset",
       y = "ATT", 
       color = NULL, 
       title = NULL) +
  theme(legend.position = "bottom")
dev.off()

############## PRE-TREND CHECKS ###############
# keep untreated observations
df <- yearlyPM25 %>%
  select(-Shape_Area) %>%
  rename(unit = OBJECTID,
         g = g_shift,
         y = outcome)
df_untreated <- df[df$year < df$g, ]

# event time
df_untreated$event_time <- df_untreated$year - df_untreated$g

df_untreated <- df_untreated %>%
  filter(event_time >= -5) # this means -6

# create pre-treatment leads
df_untreated$lead5 <- as.integer(df_untreated$event_time == -5)
df_untreated$lead4 <- as.integer(df_untreated$event_time == -4)
df_untreated$lead3 <- as.integer(df_untreated$event_time == -3)
df_untreated$lead2 <- as.integer(df_untreated$event_time == -2)
df_untreated$lead1 <- as.integer(df_untreated$event_time == -1) # this is actually lead 2

# regression on untreated observations

### use this version 
m <- fixest::feols(y ~ lead3 + lead2 + lead1  | unit + year,
                   data = df_untreated,
                   cluster = "unit")
fixest::wald(m, c("lead1", "lead2", "lead3")) 



