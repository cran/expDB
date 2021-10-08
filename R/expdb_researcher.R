# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# experimentDB API for researcher

#' Insert and update researcher into expDB
#' @param con A connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddResearcher <- function(con, data)
{
    names(data) <- tolower(names(data))
    dbInsertUpdateByRow(con, 'expdb_researcher', data)
}
