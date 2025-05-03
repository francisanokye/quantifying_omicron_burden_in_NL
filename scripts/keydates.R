library(shellpipes)
set.seed(2024)

start_date <- "2021-12-15"
last_date <-"2022-05-26"
trim_report <- "2022-05-01"

change_dates <- as.Date(c("2021-12-15", "2021-12-25", "2022-01-09", "2022-02-08", "2022-03-15"))
# as.Date(c("2021-12-15", "2022-01-03", "2022-01-24", "2022-02-24", "2022-03-17"))

saveEnvironment()

