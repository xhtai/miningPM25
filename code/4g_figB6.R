rm(list = ls()); gc()
library(dplyr); library(ggplot2)

yearlyPM25_wide <- readRDS("./data/tang2023_PM.rds")

tang2023 <- sf::st_read("./data/tang2023/74548_projected polygons.shp", quiet = TRUE) 
tang2023 <- sf::st_zm(tang2023, drop = TRUE, what = "ZM") 
tang2023 <- sf::st_make_valid(tang2023)
tang2023 <- tang2023 %>%
  sf::st_simplify(50, preserveTopology = TRUE) 

############
# first need to match tang2023 polygons to jasansky
facilities <- sf::st_read("./data/jasansky2023/data/facilities.shp") %>% # 2415 obs
  filter(!is.na(prdctn_s)) %>% # 598
  filter(prdctn_s %in% 2001:2024) # 297

# sf::st_crs(tang2023) #  WGS 84 / NSIDC EASE-Grid Global 
# sf::st_crs(facilities) # WGS 84 

tang2023 <- sf::st_transform(tang2023, crs = 4326)

# districts <- sf::st_intersects(tang2023, facilities, sparse = FALSE) # 
# #--- each facility is on a column
# # 74548*598
# numMatches <- apply(districts, MARGIN = 1, FUN = function(x) sum(x == TRUE)) # this will only work with points
# table(numMatches)
# # numMatches
# #     0     1     2     3 
# # 74267   266    14     1 

# more than 1 match is fine
# make a data set that is tang2023$OBJECTID, fclty_d, prdctn_s for all with >= 1 match. OBJECTID can be repeated for those that have multiple matches. 
sparseTrue <- sf::st_intersects(tang2023, facilities, sparse = TRUE) #  74548
names(sparseTrue) <- tang2023$OBJECTID
sparseTrue <- sparseTrue[lengths(sparseTrue) > 0]

dataframe <- do.call(
  rbind,
  lapply(names(sparseTrue), function(id) {
    data.frame(
      OBJECTID = id,
      match = sparseTrue[[id]]
    )
  })
)
# dataframe has nrow 157 and length(unique(dataframe$OBJECTID)) = 148
dataframe$fclty_d <- facilities$fclty_d[dataframe$match]
dataframe$prdctn_s <- facilities$prdctn_s[dataframe$match]

# length(unique(dataframe$fclty_d)) # 144

dataframe <- dataframe %>%
  left_join(yearlyPM25_wide %>%
              select(OBJECTID, minLossYear) %>%
              mutate(OBJECTID = as.character(OBJECTID)),
            by = "OBJECTID")

# sum(!is.na(dataframe$minLossYear)) # 124 

# for facilities with multiple matches, take the minimum minLossYear
out <- dataframe %>%
  filter(!is.na(minLossYear)) %>%
  select(fclty_d, prdctn_s, minLossYear)
  group_by(fclty_d, prdctn_s) %>%
  summarize(minLossYear = min(minLossYear))
# 124 obs

pdf("./figs/jasanskyVsHansen.pdf", width = 6, height = 5)
out %>%
  filter(!is.na(minLossYear)) %>%
  # filter(prdctn_s %in% 1995:2024) %>%
  # filter(prdctn_s >= 2001) %>%
  ggplot(aes(x = jitter(minLossYear, factor = .3), y = jitter(prdctn_s, factor = .2))) +
  geom_point(size = .8) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 1)) +
  scale_y_continuous(breaks = seq(2000, 2024, by = 1)) +
  labs(x = "Mining onset", y = "Production start (Jasansky et al.)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

mean(out$minLossYear <= out$prdctn_s) # 0.9112903
