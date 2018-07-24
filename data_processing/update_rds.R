# For uploading files to the database - requires first 
# an ssh connection to the database (through terminal)

# specify data and schema and table names, e.g.
# data <- dfFinal  # must be already loaded in your environment
# schemaName <- "clean"
# tableName <- "blockgroup_nbhds"

require("RPostgreSQL")

update_rds <- function(data, schemaName, tableName, overwrite = FALSE) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  # load credentials for the connection:
  # dbname, host, port, user, password
  source('/Users/kelliemacphee/Desktop/dssg2018/cred.txt')
  
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = dbname,
                   host = host, port = port,
                   user = user, password = password)
  
  dbWriteTable(con,
               c(schemaName, tableName),
               value = data,
               row.names = FALSE,
               overwrite = overwrite  # overwrite an existing table
  )
  
  # when you're done, close the connection and unload the driver 
  dbDisconnect(con) 
  dbUnloadDriver(drv)
  
}
