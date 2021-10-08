# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:07 PM Saturday, 04 May 2013
# * Copyright: AS IS
# *


# expDB API for source


#' Insert and update source into expDB
#' @param con A connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddSource <- function(con, data)
{
    names(data) <- tolower(names(data))
    dbInsertUpdateByRow(con, 'expdb_source', data)
}


#' Get source from expDB
#' @param con a connection object as produced by dbConnect
#' @return A data.frame for all source in the data base
#' @export
dbGetSource <- function(con)
{
    DBI::dbReadTable(con, 'expdb_source')
}
