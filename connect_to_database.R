require("RPostgreSQL")
mypath <- dirname(rstudioapi::getActiveDocumentContext()$path)

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# make a file called "cred.txt" of the form
#
# user: "YOUR_AWS_USERNAME"
# password: "YOUR_AWS_PASSWORD"
#
# in the directory above the osr_dssg2018 folder

source(file.path(dirname(mypath),"cred.txt"))

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dssg2018uw",
                 host = "localhost", port =9000,
                 user = user, password = password)

