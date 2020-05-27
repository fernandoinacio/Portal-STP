
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "covidportal",
                 host = "173.249.21.143",
                 user = "covid", password = "StopCovid2019!")

data= dbGetQuery(con, "SELECT * from stp_core_indicators ")

dbDisconnect(con)