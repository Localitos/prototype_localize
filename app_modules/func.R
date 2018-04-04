library(data.table)
library(RPostgreSQL)

databaseName <- "localize"
host <- "localhost"
port <-5432

## Define save data ##
saveData <- function(data, table) {
  # connect to database
  con <- dbConnect(dbDriver("PostgreSQL"), 
                   dbname=databaseName, 
                   host=host,
                   port=port)
  
  # close db connection after function call exits
  on.exit(dbDisconnect(con))
  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  
  # submit the update query and disconnext
  dbGetQuery(con, query)

}

## Define load data ##
loadData <- function(table) {
  # connect to database
  con <- dbConnect(dbDriver("PostgreSQL"),
                   dbname=databaseName, 
                   host=host,
                   port=port)
  
  # close db connection after function call exits
  on.exit(dbDisconnect(con))
  # construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(con, query)
  data
  
}

killDbConnections <- function () {
  
  all_cons <- dbListConnections(dbDriver("PostgreSQL"))
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

thumbnail <- function(label, content, button = TRUE, 
                      button_link, button_label){
  
  if (button) {
    HTML(paste0("<div class='row'>
              <div class='col-sm-14 col-md-12'>
                <div class='thumbnail'>
                <div class='caption'>
                <h4>", label, "</h4>
                <p>", content, "</p>
                <p><a href='", button_link, "' class='btn btn-primary' role='button'>", button_label, "</a> </p>
                </div>
                </div>
                </div>
                </div>") )
  } else {
    HTML(paste0("<div class='row'>
              <div class='col-sm-14 col-md-12'>
                <div class='thumbnail'>
                <div class='caption'>
                <h4>", label, "</h4>
                <p>", content, "</p>
                </div>
                </div>
                </div>
                </div>") )
    
  }
  
}

agerange_out <- function(age) {
  if (age %in% c(0:2)) {
    return("_0_2")
  } else if (age %in% c(3:5)) {
    return("_03_5")
  } else if (age %in% c(6:11)) {
    return("_06_11")
  } else if (age %in% c(12:17)) {
    return("_12_17")
  } else if (age %in% c(18:19)) {
    return("_18_19")
  } else if (age %in% c(20:29)) {
    return("_20_29") 
  } else if (age %in% c(30:44)) {
    return("_30_44") 
  } else if (age %in% c(45:59)) {
    return("_45_59") 
  } else if (age %in% c(60:64)) {
    return("_60_64")
  } else {
    return("_64_")
  }
}
