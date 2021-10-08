# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# utilities
# Convert year and day of year to date
#
# @param day day of year
# @param year year
yearDay2Date <- function(day, year)
{
    return (as.Date(day, origin = as.Date(paste(year - 1, '-12-31', sep = ''),
        format = '%Y-%m-%d')))
}

# Generate unique name with digest method
# @param data A data frame
generateName <- function(data)
{
    for (i in seq(length = ncol(data)))
    {
        data[[i]] <- as.character(data[[i]])
    }
    name <- as.character(apply(data, 1, function(x)
        {
            # digest(paste(
                # as.character(
                    # unclass(x)), collapse = '_'))
            paste(
                as.character(
                    unclass(x)), collapse = '_')
    }))
    return(name)
}


# Get index id by name.
# @param name The old columns
# @param alias The alias name for all columns
# @return The new name get rid of alias
checkAlias <- function(name, alias = getAlias())
{
    if (is.null(name))
    {
        return (NULL)
    }
    name <- tolower(name)
    alias_name <- names(alias)
    for (i in seq(along = alias))
    {
        pos <- name %in% alias[[i]]
        name[pos] <- alias_name[i]
    }
    return (name)
}


# Alias of column names
getAlias <- function()
{
    alias <- list(trial = c('trialcode'),
        row_spacing = c('rowspacing'),
        row = c('plot'), column = c('range'))
    return(alias)
}

# Paste a vector to format used for SQL in
# @param x a vector
sqlIn <- function(x)
{
    return(paste('in (', paste(paste('"', x, '"', sep = ''),
                    collapse = ', '), ')', sep = ''))
}

# Read xlsx files
#
# @param file The path to xlsx file
# @export
xlsxToR <- function(file)
{
    sheets <- readxl::excel_sheets(file)
    worksheets <- list()
    for (i in seq(along = sheets)) {
        tryCatch({
            sheet_i <- readxl::read_excel(file, sheets[i], na = 'NA', guess_max = 100000)
            
            sheet_i <- sheet_i[,!(is.na(names(sheet_i)))]
            
            pos <- apply(sheet_i, 1, function(x) sum(is.na(x))) != ncol(sheet_i)
            sheet_i <- sheet_i[pos,]
            worksheets[[sheets[i]]] <- sheet_i
        }, error = function(e){
            e <- as.character(e)
            if (!grepl('Skipped over all data', e))
            {
                stop(paste0('Error in sheet "', sheets[i], '": ', e))
            }
        }, warning = function(w){
            warning(paste0('Warning in sheet "', sheets[i], '": ', w))
        }, message = function(m) {
            warning(paste0('Message in sheet "', sheets[i], '": ', m))
        })
    }
    worksheets
}

# System based file path
#
# @param file The filename
# @param package Package name
system_file <- function(file, package) {
    # if (Sys.info()['sysname'] == 'Linux') {
    #     file <- paste0(file, '.gz')
    # }
    file <- system.file(file, package = package)
    
    file
}


