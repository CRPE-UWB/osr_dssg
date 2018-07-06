 # install.packages("RPostgreSQL")

require("RPostgreSQL")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
source('/Volumes/GoogleDrive/My Drive/eScience/projects/dssg2018/db_cred.txt')
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = name,
                 host = "localhost", port = 9000,
                 user = user, password = pswd)

dbListTables(con) #list tables 

#here you will do what you need to do in the database.. like
#dbWriteTable(con, c("<schema_name>", "<table_name"), value = <df_name>)
#or: 
#sql_df = dbGetQuery(con, "select * from _schema.table_")

#when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)