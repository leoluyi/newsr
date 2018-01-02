suppressPackageStartupMessages({
  library(newsr)
  library(methods)
  library(readr)
  # library(feather)
})
# source("refactor_appledaily.R")

# go ----------------------------------------------------------------------

con <- dbConnect(SQLite(), '~/data/appledaily_keelung.sqlite')
# con <- dbConnect(SQLite(), ":memory:")
on.exit(close(con))
# dbReadTable(con, "NEWS_APPLE_DAILY")
# write_success <- dbWriteTable(con, "NEWS_APPLE_DAILY",
#                               data.table(),
#                               row.names = FALSE, append = TRUE)

out <- search_appledaily(
  c("基隆"),
  date_from = "2017-12-07",
  max_page = 1
  # ,nb_core = 1
)
write_feather(out, "~/data/appledaily_keelung.feather")
write_excel_csv(out, "~/data/appledaily_keelung.csv")

# DB ----------------------------------------------------------------------

# library(RSQLServer)
# library(DBI)
# library(dplyr)
#
# con <- dbConnect(RSQLServer::SQLServer(), # jTDS driver
#                  "TSDB_MSSQL-connection",
#                  database = 'TSDB_MSSQL')
#
# ## List tables
# dbListTables(con)
#
# ## List table fields
# dbListFields(con, 'NEWS_APPLEDAILY')
#
# ## Write DB
# dbWriteTable(con, "NEWS_APPLEDAILY", res)
