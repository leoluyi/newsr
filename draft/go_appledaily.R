suppressPackageStartupMessages({
library(methods)
library(readr)
library(feather)
})
source("refactor_appledaily.R")

# go ----------------------------------------------------------------------

con <- dbConnect(SQLite(), '~/data/appledaily_keelung.sqlite')
# con <- dbConnect(SQLite(), ":memory:")
on.exit(close(con))
# dbReadTable(con, "NEWS_APPLE_DAILY")
# write_success <- dbWriteTable(con, "NEWS_APPLE_DAILY",
#                               data.table(),
#                               row.names = FALSE, append = TRUE)

res <- search_appledaily(c("基隆"),
                         date_from = "2015-01-01")
write_feather(res, "~/data/appledaily_keelung.feather")
write_excel_csv(res, "~/data/appledaily_keelung.csv")

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
