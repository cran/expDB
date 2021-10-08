# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   8:54 AM Wednesday, 22 August 2012
# * Copyright: AS IS
# *

# Connect and disconnect to expDB

#' Connect to expDB
#' @param filename The filename of SQLite
#' @return a connection object as produced by dbConnect
#' @examples 
#' \dontrun{
#' con <- connect('filename')
#' }
#' @export
expdbConnect <- function(filename)
{
    if (is.null(filename)) {
        # host <- Sys.getenv('EXPDB_MYSQL_HOST')
        # if (nchar(host) == 0) {
        #     stop('Set environment variable "EXPDB_MYSQL_HOST" to specify to host name in a mysql server')
        # }
        # 
        # db_name <- Sys.getenv('EXPDB_MYSQL_DB')
        # if (nchar(db_name) == 0) {
        #     stop('Set environment variable "EXPDB_MYSQL_DB" to specify to database name in a mysql server')
        # }
        # 
        # user <- Sys.getenv('EXPDB_MYSQL_USER')
        # if (nchar(user) == 0) {
        #     stop('Set environment variable "EXPDB_MYSQL_USER" to specify to user name in a mysql server')
        # }
        # 
        # password <- Sys.getenv('EXPDB_MYSQL_PASSWORD')
        # if (nchar(db_name) == 0) {
        #     stop('Set environment variable "EXPDB_MYSQL_PASSWORD" to specify to password name in a mysql server')
        # }
        # 
        # con <- dbConnect(
        #     RMySQL::MySQL(), 
        #     dbname = db_name, host = host, 
        #     port = 3306, 
        #     username = user, 
        #     password = password)
    } else {
        if (!file.exists(filename))
        {
            stop('Cannot find the SQLite database')
        }
        m <- RSQLite::dbDriver("SQLite")
        con <- RSQLite::dbConnect(m, dbname = filename)
    }
    return(con)
}

#' Didconnect to expDB
#' @param con a connection object as produced by dbConnect. 
#' @examples 
#' \dontrun{
#' con <- connect('filename')
#' disconnect(con)
#' }
#' @return no return values
#' @export
expdbDisconnect <- function(con)
{
    invisible(RSQLite::dbDisconnect(con))
}

#' Append a table into db and check the column name
#'
#' @param con A connection object as produced by dbConnect
#' @param table The target table name
#' @param data A data frame to write into table
dbAppendTable <- function(con, table, data)
{
    dbcols <- DBI::dbListFields(con, table)
    data <- data[, names(data) %in% dbcols]
    data <- data[, match(dbcols, names(data))]
    DBI::dbWriteTable(con, table, data, 
                      append = TRUE, row.names = FALSE)
}

#' Insert new rows or update existing rows to a specific table according to a specific column (unique) by each row
#' 
#' @param con A connection object as produced by dbConnect
#' @param table The target table name
#' @param data A data frame to write into table
#' @param unique_col A character vector to indentify each row in the table
dbInsertUpdateByRow <- function(con, table, data, unique_col = 'name')
{
    if (sum(unique_col %in% names(data)) != length(unique_col))
    {
        stop(paste('column(s) "',
                   paste(unique_col, collapse = ', '), 
                   '" must be in the input data.frame', sep = ''))
    }
    
    # check columns
    dbcols <- DBI::dbListFields(con, table)
    e_cols <- names(data) %in% dbcols
    if(sum(!e_cols) > 0)
    {
        warning(paste(names(data)[!e_cols], collapse = ', '), 
                ' do not exist in the table ',
                table, sep = '')
        data <- data[,e_cols]
    }
    
    unique_id <- getIdByUniqueIndex(con, table, data, unique_col)
    insert_pos <- is.na(unique_id)
    sql <- NULL
    if (sum(insert_pos) > 0)
    {
        col_names <- paste(paste0('`', names(data), '`'), collapse = ', ')
        new_values <- apply(as.data.frame(data[insert_pos,]), 1, function(x) {
            paste(paste(
                ifelse(is.na(x), 'NULL', 
                       paste('"', x, '"', sep = '')), sep = ''), 
                collapse = ', ')
        })
        sql_insert <- sprintf('INSERT INTO %s (%s) VALUES (%s)', table, col_names, new_values)
        
        sql <- c(sql, sql_insert)
    }
    
    data_col <- !(names(data) %in% unique_col)
    if (sum(!insert_pos) > 0 & sum(data_col) > 0)
    {
        temp <- as.data.frame(data[!insert_pos,data_col])
        names(temp) <- names(data)[!(names(data) %in% unique_col)]
        new_values <- apply(temp, 1, function(x) {
            paste(paste('`', names(x), '`', '=', 
                        ifelse(is.na(x), 'NULL', 
                               paste('"', x, '"', sep = '')),
                        sep = ''), 
                  collapse = ', ')
        })
        where <- paste('id = ', unique_id[!insert_pos], sep = '')
        sql_update <- sprintf('UPDATE %s SET %s WHERE %s', table, new_values, where)
        sql <- c(sql, sql_update)
    }
    if (!is.null(sql))
    {
        DBI::dbBegin(con)
        for (i in seq(along = sql))
        {
            res <- DBI::dbSendQuery(con, sql[i])
            DBI::dbClearResult(res)
        }
        success <- DBI::dbCommit(con)
        return(success)
    }
    return (TRUE)
}



#' Get index id by unique_columns.
#' @param con a connection object as produced by dbConnect
#' @param table the table name
#' @param data A data frame to write into table
#' @param unique_col A character vector to identify each row in the table
#' @param data_col The column names in the data.frame
#' @param ignore_case Whether ignore_case
#' @return id of unique_col
getIdByUniqueIndex <- function(con, table, data,
                               unique_col = 'name', data_col = unique_col, 
                               ignore_case = FALSE)
{
    if ('character' %in% class(data) & length(unique_col) == 1)
    {
        temp <- as.list(NULL)
        temp[[unique_col]] <- data
        data <- as.data.frame(temp)
    }
    if (ignore_case)
    {
        for (i in seq(along = data_col))
        {
            data[[data_col[i]]] <- tolower(data[[data_col[i]]])
        }
    }
    vls <- apply(as.data.frame(data[,data_col]), 2, function(x)
    {
        paste(paste('"', x, '"', sep = ''), collapse = ',')
    })
    if (ignore_case)
    {
        where <- paste(paste('LOWER(', unique_col, ') in (',
                             vls, ')', sep = ''), collapse = ' AND ')
        case <- switch(class(con), 
                       'MySQLConnection' = '',
                       'SQLiteConnection' = ' COLLATE NOCASE'
        )
        where <- paste0(where, case)
        sql <- sprintf('SELECT id, %s from %s WHERE %s', 
                       paste('LOWER(', unique_col, ') AS ', unique_col, collapse = ','),
                       table, where)
        
    } else
    {
        case <- switch(class(con), 
                       'MySQLConnection' = ' COLLATE latin1_general_cs',
                       'SQLiteConnection' = ''
        )
        where <- paste(paste(unique_col, case, ' in (',
                             vls, ')', sep = ''), collapse = ' AND ')
        
        sql <- sprintf('SELECT id, %s from %s WHERE %s', 
                       paste(unique_col, collapse = ','),
                       table, where)
    }
    
    res <- DBI::dbGetQuery(con, sql)
    res <- merge(data, res, by.x = data_col,
                 by.y = unique_col, sort = FALSE, all.x = TRUE)
    
    id <- res$id[match(apply(as.data.frame(data[,data_col]), 1, paste, collapse = ''),
                       apply(as.data.frame(res[,data_col]), 1, paste, collapse = ''))]
    return(id)
}


#' create to expDB
#' @param filename The filename of new expDB
#' @param system_traits Whether to import system traits
#' @return a connection object as produced by dbConnect
#' @export
expdbCreateDB <- function(filename, system_traits = TRUE)
{
    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    
    sql <- readLines(system.file('expdbDefault.txt', package = 'expDB'))
    sql <- paste(sql, collapse = '\n')
    sql <- unlist(strsplit(sql, ';'))
    sql <- gsub('\t+', '', sql)
    sql <- gsub('\n+', '', sql)
    sql <- sql[nchar(sql) > 0]
    # sql <- append(sql, 
    # sprintf('INSERT INTO expdb_history (date,comments) VALUES ("%s","Create the new database")',
    # format(Sys.time(), format='%Y-%m-%d')))
    DBI::dbBegin(db)
    for (i in seq(along = sql))
    {
        res <- DBI::dbSendQuery(db, sql[i])
        DBI::dbClearResult(res)
    }
    success <- DBI::dbCommit(db)
    # Copy database from memory to file
    RSQLite::sqliteCopyDatabase(db, filename)
    DBI::dbDisconnect(db)
    db <- DBI::dbConnect(RSQLite::SQLite(), filename)
    if (system_traits) {
        traits <- utils::read.csv(system_file('meta/dbtraits.csv', 'expDB'))
        dbAddTraits(db, traits)
    }
    return(db)
}
