# this code: all mines 
  # outcome: PM 5, 5-20, 20-50, 50-100
  # outfiles: output/models/

rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./myFun.R")

conditions <- list(
  "All Mines" =
    quote(
      Shape_Area > 0
    )
)

outcomeType <- c("PM")
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  cat(i, ", ")
  myFun(
    outcomeType = combos$outcomeType[i],
    radius = combos$radius[i],
    conditions = conditions,
    outFilePrefix = "log", 
    outFileSuffix = ""
  )
}


### NL pop all mines 
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./myFun.R")

conditions <- list(
  "All Mines" =
    quote(
      !is.na(minLossYear)
    )
) # this takes care of NA --- NAs are excluded

outcomeType <- c(#"PM", 
  "NL", "pop")
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
    outFileSuffix = ""
  )
}

