# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   3:33 PM Wednesday, 5 September 2012
# * Copyright: AS IS
# *

# experimentDB API for manager

#' Insert or Update methods into expDB
#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddMethods <- function(con, data)
{
    dbInsertUpdateByRow(con, 'expdb_method', data)
}


#' Insert or Update irrigation into expDB
#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddIrrigatons <- function(con, data)
{
    trial_alias <- list(trial = c('trialcode'))
    names(data) <- checkAlias(names(data), trial_alias)
    
    if (!is.null(data$trial))
    {
        trials <- unique(data$trial)
        trial_id <- getIdByUniqueIndex(con, 'expdb_trial', 
            as.data.frame(list(name = trials)), 'name', 'name')
        
        if (sum(is.na(trial_id)) > 0)
        {
            stop(paste('Trials "', paste(as.character(trials[is.na(trial_id)]), collapse = ', '), 
                '" are not in the database', sep = ''))
        }
        data$trial_id <- trial_id[match(data$trial, trials)]
        data$trial <- NULL
    }
    cols <- DBI::dbListFields(con, 'expdb_irrigation')
    data$date <- format(data$date)
    DBI::dbWriteTable(con, 'expdb_irrigation', as.data.frame(data[,cols]), row.names = FALSE, append = TRUE)
}

#' Insert or Update fertilization into expDB
#' 
#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddFertilization <- function(con, data)
{
    trial_alias <- list(trial = c('trialcode'))
    names(data) <- checkAlias(names(data), trial_alias)
    
    if (tibble::has_name(data, 'trial'))
    {
        trials <- unique(data$trial)
        trial_id <- getIdByUniqueIndex(con, 'expdb_trial', 
            as.data.frame(list(name = trials)), 'name', 'name')
        
        if (sum(is.na(trial_id)) > 0)
        {
            stop(paste('Trials "', paste(data$site[is.na(trial_id)], collapse = ', '), 
                '" are not in the database', sep = ''))
        }
        data$trial_id <- trial_id[match(data$trial, trials)]
        data$trial <- NULL
    }
    cols <- DBI::dbListFields(con, 'expdb_fertilization')
    data$date <- format(data$date)
    DBI::dbWriteTable(con, 'expdb_fertilization', as.data.frame(data[,cols]), row.names = FALSE, append = TRUE)

}

#' Get irrigation from database
#' 
#' @param con a connection object as produced by dbConnect
#' @param ... Other arguments to specify meta data
#' @return a data frame for irrigation information
#' @export
dbGetIrrigation <- function(con, ...) {
    trials <- dbGetTrials(con, ...)
    
    sql <- sprintf('SELECT name as trial, date, amount FROM expdb_irrigation I
                    LEFT OUTER JOIN expdb_trial T ON I.[trial_id] == T.[id] WHERE trial_id in (%s)',
                   paste(unique(trials$trial_id), collapse = ', '))
    DBI::dbGetQuery(con, sql)
}


#' Get fertilization from database
#' 
#' @param con a connection object as produced by dbConnect
#' @param ... Other arguments to specify meta data
#' @return a data.frame for fertilization information
#' @export
dbGetFertilization <- function(con, ...) {
    trials <- dbGetTrials(con, ...)
    
    sql <- sprintf('SELECT name as trial, date, fertilizer, amount FROM expdb_fertilization F
                   LEFT OUTER JOIN expdb_trial T ON F.[trial_id] == T.[id] WHERE trial_id in (%s)',
                   paste(unique(trials$trial_id), collapse = ', '))
    DBI::dbGetQuery(con, sql)
}

