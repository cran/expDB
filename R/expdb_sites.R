# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# experimentDB API for site

#' Insert or update site into expDB
#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddSites <- function(con, data)
{
    site_alias <- list(soil = c('soils'))
    names(data) <- checkAlias(names(data), site_alias)
    if (tibble::has_name(data, 'soil'))
    {
        soil_id <- getIdByUniqueIndex(con, 'expdb_apsoil', data, 'soilid', 'soil')
        if (sum(is.na(soil_id)) == 0)
        {
            # stop(paste('Soils "', paste(unique(data$soil[is.na(soil_id)]), collapse = ', '), 
                # '" are not in the database', sep = ''))
            data$soil_id <- soil_id
        }
        data$soil <- NULL
    }
    dbInsertUpdateByRow(con, 'expdb_site', data)
}

#' Get site into expDB
#' @param con a connection object as produced by dbConnect
#' @return a data.frame for all sites in the data base
#' @export
dbGetSites <- function(con)
{
    DBI::dbReadTable(con, 'expdb_site')
}

