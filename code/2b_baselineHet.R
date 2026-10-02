### this code: above and below median PM 2.5, NL
  # outcome: PM, NL, pop 5, 5-20, 20-50, 50-100
  # outfiles: output/models/

# here use baseline 100 km PM above and below median for heterogeneity 

rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./myFun.R")

conditions <- list(
  "PM2.5 baseline below median" =
    quote(
      baselinePM25_100km <= medianPM    ),
  "PM2.5 baseline above median" =
    quote(   baselinePM25_100km > medianPM     )
) # this takes care of NA --- NAs are excluded


outcomeType <- c(#"PM", 
                 "NL", "pop")
radius <- c("5", "5_20", "20_50", "50_100")

combos <- expand.grid(outcomeType = outcomeType,
                      radius = radius,
                      stringsAsFactors = FALSE)

for (i in seq_len(nrow(combos))) {
  if (combos$outcomeType[i] == "PM") { # because of the file names
    myFun(
      outcomeType = combos$outcomeType[i],
      radius = combos$radius[i],
      conditions = conditions,
      outFilePrefix = "", 
      outFileSuffix = "altBaseline"
    )    
  } else if (combos$outcomeType[i] != "PM") { # because of the file names
      myFun(
        outcomeType = combos$outcomeType[i],
        radius = combos$radius[i],
        conditions = conditions,
        outFilePrefix = "", 
        outFileSuffix = "abvBelowPM100"
      )
  }

}



#### baseline NL
conditions <- list(
  "NL baseline below median" =
    quote(
      baselineNL_100km <= medianNL    ),
  "NL baseline above median" =
    quote(   baselineNL_100km > medianNL     )
) # this takes care of NA --- NAs are excluded


outcomeType <- c("PM", "NL", "pop")
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
      outFileSuffix = "abvBelowNL100"
    )
}

