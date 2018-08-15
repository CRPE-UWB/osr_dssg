# get the data as it exists in the database tosave to github

library(RPostgreSQL)

# set working directory to same as this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load credentials for the connection: dbname, host, port, user, password
# looks for cred.txt in parent dir to cloned github repo
source(file.path('RShiny', 'cred.txt'))

# specify where to save the data files
shiny_folder <- file.path('data', 'shiny_tables')

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# create a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = dbname,
                 host = host, port = port,
                 user = user, password = password)

# museum data
museums = dbGetQuery(con, "SELECT * from clean.museums")
write.csv(museums, file.path('data', 'museums.csv'), row.names = FALSE)
shiny_museums = dbGetQuery(con, "SELECT * from shiny.museums")
write.csv(shiny_museums, file.path(shiny_folder, 'museums.csv'), row.names = FALSE)

# fields data
shiny_fields = dbGetQuery(con, "SELECT * from shiny.fields")
write.csv(shiny_fields, file.path(shiny_folder, 'fields.csv'), row.names = FALSE)

# libraries data
shiny_libraries = dbGetQuery(con, "SELECT * from shiny.libraries")
write.csv(shiny_libraries, file.path(shiny_folder, 'libraries.csv'), row.names = FALSE)

# playgrounds data
shiny_playgrounds = dbGetQuery(con, "SELECT * from shiny.playgrounds")
write.csv(shiny_playgrounds, file.path(shiny_folder, 'playgrounds.csv'), row.names = FALSE)

# rec centers data
shiny_rec_centers = dbGetQuery(con, "SELECT * from shiny.rec_centers")
write.csv(shiny_rec_centers, file.path(shiny_folder, 'rec_centers.csv'), row.names = FALSE)

# parks data
shiny_parks = dbGetQuery(con, "SELECT * from shiny.parks")
write.csv(shiny_parks, file.path(shiny_folder, 'parks.csv'), row.names = FALSE)




# clean up when done
dbDisconnect(con) 
dbUnloadDriver(drv)