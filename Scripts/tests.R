library("plotseries")

textutils::here("
time, deviation
2024-10-20 16:39:00,  16
2024-10-20 21:55:00,  13
2024-10-21 07:10:00,  11
2024-10-21 15:00:00,  7
", sep = ",") -> data

debug(plotseries)
plotseries(data$deviation, t = as.POSIXct(data$time),
           ## xlab = "",
           ylab = "Value", type = "b", col = grey(.5))

abline(lm(data$deviation ~ as.POSIXct(data$time)))
