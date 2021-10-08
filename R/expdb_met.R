# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# expDB API for met
# Get the database file name
# @param con a connection object as produced by dbConnect
dbGetDBName <- function(con)
{
  methods::slot(con, 'dbname')
}


#' Insert and update met into expDB
#' @param con a connection object as produced by dbConnect
#' @param data Met design
#' @return no return values
#' @export
dbAddMets <- function(con, data)
{
    names(data) <- tolower(names(data))
    if (!tibble::has_name(data, 'type'))
    {
        type <- rep('daily', nrow(data))
    } else
    {
        type <- tolower(data$type)
    }
    
    data$type <- ifelse(type %in% 'daily', 1, 2)
    # Check existing met file
    unique_id <- getIdByUniqueIndex(con, 
                'expdb_met', data, 'name')
    if (class(con) == 'MySQLConnection') {
        for (i in seq(length = nrow(data)))
        {
            if (!is.na(unique_id[i]))
            {
                next
            }

            dbInsertUpdateByRow(con, 'expdb_met', data, 
                                unique_col = 'name')
        }
    } else if (class(con) == 'SQLiteConnection') {
        file_lim <- 300
        for (i in seq(length = nrow(data)))
        {
            if (!is.na(unique_id[i]))
            {
                next
            }
            sql <- sprintf('SELECT * FROM expdb_met_file WHERE type=%s ORDER BY id',
                           data$type[i])
            met_file <- DBI::dbGetQuery(con, sql)        
            if (sum(met_file$num < file_lim) > 0)
            {
                temp <- met_file[met_file$num < file_lim,]
                file_id <- temp$id[1]
                c_num <- temp$num[1]
            } else
            {
              # Create a new data base for met
              filename <- sprintf('%s_%smet_', 
                                  gsub('\\.db', '', basename(dbGetDBName(con))), 
                                  type[i])
              idx <- ifelse(nrow(met_file) > 0,
                            1 + as.numeric(sub(sprintf('%s(\\d+)\\.db', 
                                                       filename), '\\1', met_file$name)),
                            0)
              filename <- file.path(
                dirname(dbGetDBName(con)),
                paste(filename, idx, '.db', sep = ''))
              
              db <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
              if (type[i] == "daily") {
                 sql <- '
    CREATE TABLE [expdb_met_daily] (
      [met_id] INTEGER NOT NULL ON CONFLICT IGNORE, 
      [year] INTEGER NOT NULL ON CONFLICT IGNORE, 
      [day] INTEGER NOT NULL ON CONFLICT IGNORE, 
      [radn] FLOAT, 
      [maxt] FLOAT, 
      [mint] FLOAT, 
      [rain] FLOAT, 
      [evap] FLOAT, 
      [vp] FLOAT, 
      CONSTRAINT [] PRIMARY KEY ([met_id], [year], [day]) ON CONFLICT IGNORE);            
                '
              } else {
                  sql <- '
  CREATE TABLE [expdb_met_hourly](
  [met_id] INTEGER NOT NULL ON CONFLICT IGNORE, 
  [timestamp] TIMESTAMP NOT NULL ON CONFLICT IGNORE, 
  [temperature] FLOAT, 
  PRIMARY KEY([met_id], [timestamp]) ON CONFLICT IGNORE);
                '
              }
              sql <- gsub('\t+', '', sql)
              sql <- sql[nchar(sql) > 0]
              DBI::dbBegin(db)
              for (j in seq(along = sql))
              {
                res <- DBI::dbSendQuery(db, sql[j])
                DBI::dbClearResult(res)
              }
              success <- DBI::dbCommit(db)
              RSQLite::sqliteCopyDatabase(db, filename)
              DBI::dbDisconnect(db)
              f_data <- as.data.frame(list(
                name = basename(filename),
                type = data$type[i],
                num = 0))
              dbInsertUpdateByRow(con, 'expdb_met_file', f_data, 
                                  unique_col = 'name')
              sql <- sprintf('SELECT id FROM expdb_met_file WHERE name="%s"',
                             f_data$name)
              file_id <- as.numeric(unlist(DBI::dbGetQuery(con, sql)))
              c_num <- 0        
            }
            data$file_id <- file_id
            dbInsertUpdateByRow(con, 'expdb_met', data[i,], 
                                unique_col = 'name')
            sql  <- sprintf('UPDATE expdb_met_file SET NUM=%s WHERE id=%s',
                            c_num + 1, file_id)
            DBI::dbExecute(con, sql)
        }
    } else {
      warning('not implemented')
    }
}

#' Add weather records into expDB
#' @param con a connection object as produced by dbConnect
#' @param data A string character for the path to met file,
#' a WeaAna object, or a data frame.
#' @param name The met name in the database 
#' if data is a data frame.
#' @return no return values
#' @export
dbAddWeather <- function(con, data, name=NULL)
{
  
  if (class(data) == 'character')
  {
    stop('NOT implemented')   
    # records <- readWeatherRecords(data)
    # records <- getWeatherRecords(records)
  } else if (class(data) == 'WeaAna')
  {
    if (is.null(name))
    {
      site_infor <- weaana::siteInfor(data)
      site_infor$Name <- tolower(site_infor$Name)
      dbAddMets(con, site_infor)
      name <- site_infor$Name 
    }
    met_id <- getIdByUniqueIndex(con, 'expdb_met', 
                                 as.character(name), 'name')
    if (is.na(met_id))
    {
      stop('met id doesn\'t exist')
    }
    records <- weaana::getWeatherRecords(data)
  } else if (class(data) == 'data.frame')
  {
    met_id <- getIdByUniqueIndex(con, 'expdb_met', name)
    if (sum(is.na(met_id)) > 0)
    {
      stop(sprintf('Met %s is not in the database', name))
    }
    records <- data
  } else
  {
    stop('NOT implemented')
  }
  if (class(con) == 'MySQLConnection') {
    
    
    sql <- sprintf('SELECT * FROM expdb_met WHERE id = %s', met_id)
    met_meta <- DBI::dbGetQuery(con, sql)
    
    if (met_meta$type == 1)
    {
      records$met_id <- met_id
      dbcols <- DBI::dbListFields(con, 'expdb_met_daily')
      missing_cols <- dbcols[!(dbcols %in% names(records))]
      for (k in seq(along = missing_cols))
      {
        records[[missing_cols[k]]] <- NA
      }
      records <- records[,dbcols]
      DBI::dbAppendTable(con, 'expdb_met_daily', 
                         as.data.frame(records))
    } else {
        warning("Not implemented")
    }
    
  } else if (class(con) == 'SQLiteConnection') {
    
    
    sql <- sprintf('SELECT M.id, M.type, F.name as filename
    FROM expdb_met M LEFT OUTER JOIN expdb_met_file F ON 
    M.[file_id]=F.[id] WHERE M.id = %s', met_id)
    met_meta <- DBI::dbGetQuery(con, sql)
    records$met_id <- met_id
    filename <- file.path(
        dirname(dbGetDBName(con)),
        met_meta$filename)
    if (!file.exists(filename))
    {
        stop(sprintf('%s does not existed', filename))
    }
    m <- DBI::dbDriver("SQLite")
    conf <- DBI::dbConnect(m, dbname = filename)
    if (met_meta$type == 1)
    {
      dbcols <- DBI::dbListFields(conf, 'expdb_met_daily')
      missing_cols <- dbcols[!(dbcols %in% names(records))]
      for (k in seq(along = missing_cols))
      {
        records[[missing_cols[k]]] <- NA
      }
      records <- records[,dbcols]
      DBI::dbAppendTable(conf, 'expdb_met_daily', 
                         as.data.frame(records))
      DBI::dbDisconnect(conf)
    } else if (met_meta$type == 2)
    {
        # Assume the hourly records are csv format and have columns (timestamp, temperature)
        names(records) <- tolower(names(records))
        if (!tibble::has_name(records, "timestamp")) {
            stop("missing the timestamp column in hourly climate")
        } 
        if (!tibble::has_name(records, "temperature")) {
            warning("missing the temperature column in hourly climate")
        } 
        if (!("POSIXct" %in% class(records$timestamp))) {
            stop("timestamp column should be class POSIXct")
        }
        if (sum(is.na(records$timestamp)) > 0 ) {
            stop("Missing values in the timestamp column")
        }
        dbcols <- DBI::dbListFields(conf, 'expdb_met_hourly')
        missing_cols <- dbcols[!(dbcols %in% names(records))]
        for (k in seq(along = missing_cols))
        {
            records[[missing_cols[k]]] <- NA
        }
        records <- records[,dbcols]
        DBI::dbAppendTable(conf, 'expdb_met_hourly', 
                           as.data.frame(records))
        DBI::dbDisconnect(conf)
    }
  } else {
    warning('not implemented')
  }
}


#' Get weather records from expDB
#' @param con a connection object as produced by dbConnect
#' @param name The met name
#' @param format The format of export dataset.
#' @param na The character for missing value with default NA
#' @param tz Time zone applied for hourly temperature
#' @return a data.frame for all weather records
#' @export
dbGetWeather <- function(con, name, format = 'data_frame', na = NA_character_, tz = "UTC")
{
  met_id <- getIdByUniqueIndex(con, 'expdb_met', name)
  if (sum(is.na(met_id)) > 0)
  {
    stop(sprintf('Met %s is not in the database', name))
  }
  
  sql <- sprintf('SELECT M.id, M.type, M.name, M.number, 
        M.latitude, M.longitude, F.name as filename
        FROM expdb_met M LEFT OUTER JOIN expdb_met_file F ON 
        M.[file_id]=F.[id] WHERE M.id = %s', met_id)
  met_infor <- DBI::dbGetQuery(con, sql)
  m <- DBI::dbDriver("SQLite")
  filename <- file.path(
      dirname(dbGetDBName(con)),
      met_infor$filename)
  conf <- DBI::dbConnect(m, dbname = filename)
  if (met_infor$type == 1) {
      sql <- sprintf('SELECT * FROM expdb_met_daily WHERE met_id=%s', met_id)
  } else if (met_infor$type == 2 ) {
      sql <- sprintf('SELECT * FROM expdb_met_hourly WHERE met_id=%s', met_id)
  } else {
    stop('NOT IMPLEMENTED')
  }
  res <- DBI::dbGetQuery(conf, sql)
  DBI::dbDisconnect(conf)
  res$met_id <- NULL
  if (!is.na(na)) {
    res[is.na(res)] <- na
  }
  # Only date frame for hourly data
  if (met_infor$type == 1 ) {
      
      if (format == 'weaana')
      {
          res <- weaana::createWeaAna(
              list(Name = met_infor$name,
                   Number = met_infor$number,
                   Latitude = met_infor$latitude,
                   Longitude = met_infor$longitude,
                   Records = res))
      } else if (format == 'data_frame')
      {
          res$date <- yearDay2Date(res$day, res$year)
          res$name <- met_infor$name
          res$number <- met_infor$number
          res$latitude <- met_infor$latitude
          res$longitude <- met_infor$longitude
          res <- tibble::tibble(res)
      } else if (format == 'sirius') {
          res <- res %>% 
              dplyr::select(dplyr::all_of(c('year', 'day', 'mint', 'maxt', 'rain', 'radn')))
      }
  } else {
      res$timestamp <- as.POSIXct(res$timestamp, 
                                  origin = as.POSIXct("1970-01-01 00:00.00", tz = "UTC"),
                                  tz = "UTC")
      res$timestamp <- lubridate::with_tz(res$timestamp, tz)
      res$name <- met_infor$name
      res$number <- met_infor$number
      res$latitude <- met_infor$latitude
      res$longitude <- met_infor$longitude
      res <- tibble::tibble(res)
  }
  return(res)
}


#' Get met information
#'
#' @param con a connection object as produced by dbConnect
#' @param name The met name
#' @return a data.frame for met information
#' @export
dbGetMetInfo <- function(con, name)
{
  
  sql <- sprintf('SELECT id, type, name, number, 
        latitude, longitude from expdb_met WHERE name in (%s)', 
                 paste(paste0('"', name, '"'), collapse = ','))
  met_infor <- DBI::dbGetQuery(con, sql)
  
  met_infor
}

