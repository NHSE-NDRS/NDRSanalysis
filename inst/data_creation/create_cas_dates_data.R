library(NDRSAfunctions)

main_snapshot <- "casref01"

# extraction to R --------------------------------------------------------------
cas_snapshot <- createConnection(sid = main_snapshot)

cas_dates <- dbGetQueryOracle(
  cas_snapshot,
  "select apptdate date_with_time
  from rtds.at_episodes_england@cas2407",
  rowlimit = 1000
) %>% rename_with(str_to_lower)
