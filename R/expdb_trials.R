# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# experimentDB API for trials

#' Insert or Update trial into expDB
#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @return no return values
#' @export
dbAddTrials <- function(con, data)
{
    trial_alias <- list(name = c('trialcode'),
        row_spacing = c('rowspacing'))
    names(data) <- checkAlias(names(data), trial_alias)

    if (tibble::has_name(data, 'researcher'))
    {
        researcher_id <- getIdByUniqueIndex(con, 'expdb_researcher', data, 'name', 'researcher')
        if (sum(is.na(researcher_id)) > 0)
        {
            stop(paste('Researchers "', paste(data$researcher[is.na(researcher_id)], collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$researcher <- NULL
        data$researcher_id <- researcher_id
    }
    if (tibble::has_name(data, 'site'))
    {
        site_id <- getIdByUniqueIndex(con, 'expdb_site', data, 'name', 'site')
        if (sum(is.na(site_id)) > 0)
        {
            stop(paste('Sites "', paste(unique(data$site[is.na(site_id)]), collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$site <- NULL
        data$site_id <- site_id
    }

    if (tibble::has_name(data, 'met'))
    {

        met_id <- getIdByUniqueIndex(con, 'expdb_met', data, 'name', 'met')
        sum(!is.na(data$met))
        if (sum(!is.na(met_id)) != sum(!is.na(data$met)))
        {
            stop(paste('Met "', paste(data$met[is.na(met_id)], collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$met <- NULL
        data$met_id <- met_id
    }

    if (tibble::has_name(data, 'source'))
    {
        source_id <- getIdByUniqueIndex(con, 'expdb_source', data, 'name', 'source')
        if (sum(is.na(source_id)) > 0)
        {
            stop(paste('Source "',
                       paste(data$source[is.na(met_id)], collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$source <- NULL
        data$source_id <- source_id
    }

    dbInsertUpdateByRow(con, 'expdb_trial', data)
}


#' Add design for a trial
#' @param con a connection object as produced by dbConnect
#' @param data Trial design
#' @param extra_design The extra columns for design
#' @return no return values
#' @export
dbAddDesigns <- function(con, data, extra_design = NULL)
{
    trial_alias <- list(trial = c('trialcode', 'name'),
        row = c('plot'), column = c('range'),
        replicate = c('rep'))
    names(data) <- checkAlias(names(data), trial_alias)

    data$name <- generateName(data[,c('site', 'row', 'column', 'year')])
    if (tibble::has_name(data, 'genotype'))
    {
        data$genotype <- tolower(dbGenotypeCheckName(con, data$genotype))
        genotype_id <- getIdByUniqueIndex(con, 'expdb_genotype', data, 'name', 'genotype',
            ignore_case = TRUE)
        if (sum(is.na(genotype_id)) > 0)
        {
            stop(paste('Genotype "', paste(unique(data$genotype[is.na(genotype_id)]), collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$genotype <- NULL
        data$genotype_id <- genotype_id
    }
    if (tibble::has_name(data, 'site'))
    {
        site_id <- getIdByUniqueIndex(con, 'expdb_site', data, 'name', 'site')
        if (sum(is.na(site_id)) > 0)
        {
            stop(paste('Site "', paste(data$site[is.na(site_id)], collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$site <- NULL
        data$site_id <- site_id
    }
    if (tibble::has_name(data, 'trial'))
    {
        trial_id <- getIdByUniqueIndex(con, 'expdb_trial', data, 'name', 'trial')
        if (sum(is.na(trial_id)) > 0)
        {
            stop(paste('Trials "', paste(unique(data$trial[is.na(trial_id)]), collapse = ', '),
                '" are not in the database', sep = ''))
        }
        data$trial <- NULL
        data$trial_id <- trial_id
    }
    dbcols <- DBI::dbListFields(con, 'expdb_trial_design')
    e_cols <- names(data) %in% dbcols
    data_design <- data[,e_cols]
    dbInsertUpdateByRow(con, 'expdb_trial_design', data_design)

    if (!is.null(extra_design))
    {
        extra_design <- tolower(extra_design)
        unique_id <- getIdByUniqueIndex(con, 'expdb_trial_design', data_design)
        data$plot_id <- unique_id
        extra_cols <- names(data)[names(data) %in% tolower(extra_design)]
        if (length(extra_cols) > 0)
        {
            extra_design <- subset(data, select = c('plot_id', extra_cols))
            extra_design <- reshape2::melt(extra_design, id.vars = 'plot_id', measure.vars = extra_cols,
                variable.name = 'name', value.name = 'value')
            extra_design$name <- as.character(extra_design$name)
            DBI::dbWriteTable(con, 'expdb_trial_design_extra', stats::na.omit(extra_design),
                append = TRUE, row.names = FALSE)

        }
    }
}


#' Add soil for a trial
#' @param con a connection object as produced by dbConnect
#' @param data Soil profiles for trials
#' @param units a list for the unit
#' @return no return values
#' @export
dbAddTrialSoil <- function(con, data, units = list(thickness = 'cm',
    no3 = 'kg/ha', nh4 = 'kg/ha'))
{
    trial_alias <- list(trial = c('trialcode'),
        no3n = c('no3.n', 'no3-n'), nh4n = c('nh4.n', 'nh4-n'),
        db = 'bulkdensity')
    names(data) <- checkAlias(names(data), trial_alias)
    data$db <- as.numeric(data$db)
    if (tibble::has_name(data, 'no3n'))
    {
        data$no3n <- as.numeric(data$no3n)
        data$no3 <- data$no3n * (14 + 16 * 3) / 14
    }
    if (tibble::has_name(data, 'nh4n'))
    {
        data$nh4n <- as.numeric(data$nh4n)
        data$nh4 <- data$nh4n * (14 + 1 * 4) / 14
    }
    # Convert unit
    if (units$thickness == 'cm')
    {
        data$thickness <- as.numeric(data$thickness)
        data$thickness <- data$thickness * 10
    }
    if (units$no3 == 'kg/ha')
    {
        data$no3 <- data$no3 * 100 / (data$db * data$thickness)
    }
    if (units$nh4 == 'kg/ha')
    {
        data$nh4 <- data$nh4 * 100 / (data$db * data$thickness)
    }

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
    if (tibble::has_name(data, 'thickness'))
    {
        trial_ids <- unique(data$trial_id)
        data$depth <- NA
        for (i in seq(along = trial_ids))
        {
            pos <- data$trial_id %in% trial_ids[i]
            data$depth[pos] <- cumsum(data$thickness[pos])
        }
        data$from_depth <- data$depth - data$thickness
        data$to_depth <- data$depth
    }
    cols <- DBI::dbListFields(con, 'expdb_trial_soil')
    DBI::dbWriteTable(con, 'expdb_trial_soil', as.data.frame(data[,cols]), row.names = FALSE, append = TRUE)
}

#' Get trials by a groups of conditions.
#' @param con a connection object as produced by dbConnect
#' @param ... All other arguments to define range of export trials.
#' All trials will be export if there are not arguments.
#' Supported arguments include trial (or trialcode)
#' @param design Whether include design
#' @return A data.frame for selected trials
#' @export
dbGetTrials <- function(con, design = TRUE, ...)
{
    args <- list(...)

    if (is.null(args) || length(args) == 0)
    {
        where <- '1'
    } else
    {
        # Check names of trial arguments
        names(args) <- checkAlias(names(args))
        where <- paste(paste(names(args), as.character(lapply(args, function(x)
            {
                return(sqlIn(x))
            })), sep = ' '), collapse = ' AND ')
    }

    design_cols <- DBI::dbListFields(con, 'expdb_trial_design')
    design_cols <- paste(paste0('`', design_cols[!(design_cols %in%
        c('id', 'name', 'year', 'trial_id', 'genotype_id'))], '`'), collapse = ', ')
    trial_cols <- DBI::dbListFields(con, 'expdb_trial')
    trial_cols <- paste(trial_cols[!(trial_cols %in%
        c('id', 'name', 'researcher_id', 'site_id', 'met_id', 'source_id', 'notes'))], collapse = ', ')

    join_trial_met <- sprintf('SELECT T.id AS id, T.name AS trial, site_id, researcher_id,
        source_id, %s, met_id, M.name AS met FROM expdb_trial T
        LEFT OUTER JOIN expdb_met M ON T.met_id=M.id WHERE 1', trial_cols)
    join_trial_site <- sprintf('SELECT T.id AS id, trial, researcher_id,
        source_id, %s, met, S.name AS site, latitude,
        longitude, soil_id FROM (%s) T
        LEFT OUTER JOIN expdb_site S ON T.site_id=S.id WHERE 1', trial_cols, join_trial_met)
    join_trial_research <- sprintf('SELECT T.id AS id, trial, %s,
        source_id, met, site, latitude, longitude, soil_id,
        R.name AS researcher, email from (%s) T
        LEFT OUTER JOIN expdb_researcher R
        on T.researcher_id=R.id where 1', trial_cols, join_trial_site)
    join_trial_source <- sprintf('SELECT T.id AS id, trial, %s,
        met, site, latitude, longitude, soil_id,
        researcher, email,
        S.name as source from (%s) T
        LEFT OUTER JOIN expdb_source S
        on T.source_id=S.id where 1', trial_cols, join_trial_research)
    sql <- NULL
    if (design)
    {
        join_design_genotype <- sprintf('SELECT D.id, D.name, trial_id, %s,
            G.name as genotype from expdb_trial_design D
            LEFT OUTER JOIN expdb_genotype G ON D.genotype_id=G.id WHERE 1', design_cols)
        sql <- sprintf('SELECT DISTINCT %s trial_id, %s, genotype,
            trial, %s, met, site, latitude, longitude, soil_id, researcher, email
            from (%s) D
            LEFT OUTER JOIN (%s) T on D.trial_id=T.id WHERE %s',
            'D.id as plot_id, ',
            design_cols, trial_cols, join_design_genotype,  join_trial_source, where)
    } else
    {
        sql <- paste0('SELECT * FROM (', join_trial_source, ') A WHERE ', where)
    }

    trials <- DBI::dbGetQuery(con, sql)
    trials$sowing <- as.Date(trials$sowing)
    if (design)
    {
        sql <- 'SELECT * FROM expdb_trial_design_extra'
        design_extra <- DBI::dbGetQuery(con, sql)


        if (nrow(design_extra) > 0)
        {
            # Check the density
            if (sum(design_extra$name %in% 'density') > 0) {
                
                trials <- trials %>%
                    tibble::tibble() %>%
                    dplyr::filter(.data$plot_id %in% design_extra$plot_id) %>%
                    dplyr::select(!dplyr::all_of('density')) %>%
                    dplyr::left_join(
                        design_extra %>%
                            tibble::tibble() %>%
                            tidyr::spread('name', 'value') %>%
                            dplyr::mutate(density = as.numeric(.data$density)),
                        by = 'plot_id'
                    ) %>%
                    dplyr::full_join(
                        trials %>%
                            dplyr::filter(!(.data$plot_id %in% design_extra$plot_id)),
                        by = c('plot_id', 'trial_id', 'site_id',
                               'column', 'row', 'replicate',
                               'treatment', 'block', 'genotype', 'trial',
                               'year', 'sowing', 'depth', 'row_spacing',
                               'met', 'site', 'latitude', 'longitude',
                               'soil_id', 'researcher', 'email',
                               'density')
                    )
            } else {
                trials <- design_extra %>%
                    tibble::tibble() %>%
                    tidyr::spread('name', 'value') %>%
                    dplyr::right_join(trials, by = 'plot_id')
            }


        }
    }

    return(trials)
}


#' List all trials
#' @param con a connection object as produced by dbConnect
#' @return A data.frame for all trials in the data base
#' @export
dbListTrials <- function(con)
{
    sql <- 'select * from expdb_trial where 1'
    trials <- DBI::dbGetQuery(con, sql)
    return(trials)
}

