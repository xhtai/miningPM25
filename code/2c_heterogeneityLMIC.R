### this code: high, upper middle income, lower middle+low
# outcome: PM, NL, pop 5, 5-20, 20-50, 50-100 # for NL, pop only high and upper middle 
  # outfiles: output/models/

rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./myFun.R")

conditions <- list(
  "High income" =
    quote(
      Income == "High income"
    ),
  "Upper middle income" =
    quote(
      Income == "Upper middle income"
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
    outFilePrefix = "log", 
    outFileSuffix = "LMIC"
  )
}

############################ high and upper middle NL/pop
rm(list = ls()); gc()
library(dplyr); library(ggplot2)

source("./myFun.R")

conditions <- list(
  "High income" =
    quote(
      Income == "High income"
    ),
  "Upper middle income" =
    quote(
      Income == "Upper middle income"
    )
)

outcomeType <- c("NL", "pop")
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
    outFilePrefix = "", 
    outFileSuffix = "byIncome"
  )
}


