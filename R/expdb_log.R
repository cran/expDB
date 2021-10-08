# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:07 PM Saturday, 04 May 2013
# * Copyright: AS IS
# *


# expDB API for log

#' Add log from expDB
#' @param con a connection object as produced by dbConnect
#' @param msg Add message into expdb
#' @param date Create time of message
#' @return No return values
#' @export
dbAddLog <- function(con, msg, 
    date = format(Sys.time(), format='%Y-%m-%d'))
{
    data <- NULL
    data$date <- date
    data$comments <- msg
    data <- as.data.frame(data)
    DBI::dbWriteTable(con, 'expdb_log', data, 
        append = TRUE, row.names = FALSE)
}

#' Get log from expDB
#' @param con a connection object as produced by dbConnect
#' @return A data.frame with all logs
#' @export
dbGetLog <- function(con)
{
    DBI::dbReadTable(con, 'expdb_log')
}

