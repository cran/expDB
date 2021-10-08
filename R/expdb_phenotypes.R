# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   11:03 PM Saturday, 25 August 2012
# * Copyright: AS IS
# *

# expDB API for phenotype

#' Add design for a trial
#' @param con a connection object as produced by dbConnect
#' @param data phenotype value
#' @return no return values
#' @export
dbAddPhenotype <- function(con, data)
{
    phenotype_alias <- list(trial = c('trialcode'),
        row = c('plot'), column = c('range'),
        traits = c('trait'), nodes = c('node'))
    names(data) <- checkAlias(names(data), phenotype_alias)

    # Check nodes
    if (!tibble::has_name(data, 'nodes')) {
        data$nodes <- 'Field'
        dbAddNodes(con, 
                   data.frame(Name = 'Field', Parent = 'Experiment'))
    } else {
        data$nodes[is.na(data$nodes)] <- 'Field'
    }
    
    phe <- NULL
    phe$date <- data$date
    
    
    plot_name <- generateName(data[,c('site', 'row', 'column', 'year')])
    plot_id <- getIdByUniqueIndex(con, 'expdb_trial_design', unique(plot_name))
    if (sum(is.na(plot_id)) > 0)
    {
        missing_plot <- unique(plot_name)[is.na(plot_id)]
        print('Error plots:')
        print(unique(data[plot_name %in% missing_plot,]))
        stop('The above plots are not in the database')
    }
    phe$plot_id <- plot_id[match(plot_name, unique(plot_name))]
    
    # Check nodes
    nodes <- data %>% 
        dplyr::select(name = nodes) %>% 
        dplyr::distinct()
    
    node_id <- getIdByUniqueIndex(con, 'expdb_node', as.character(nodes$name))
    if (sum(is.na(node_id)) > 0) {
        stop(paste0('Cannot find nodes with ID "', 
                    paste(nodes$name[is.na(node_id)], 
                          collapse = ', '), '"'))
    }
    
    phe$node_id <- node_id[match(data$nodes, 
                                 as.character(nodes$name))]
    
    phe <- as.data.frame(phe, stringAsFactors = FALSE)
    
    traits <- unique(data$traits)
    trait_id <- getIdByUniqueIndex(con, 'expdb_trait',
                                   tolower(traits), ignore_case = TRUE)
    if (sum(is.na(trait_id)) > 0)
    {
        stop(paste('Trait(s) "', paste(traits[is.na(trait_id)], collapse = ', '),
                   '" are not in the database', sep = ''))
    }
    phe$trait_id <- trait_id[match(data$traits, traits)]
    
    if (!tibble::has_name(data, 'sample'))
    {
        phe$sample <- 1
    } else
    {
        phe$sample <- data$sample
    }
    
    phe$value <- data$value
    if (!('quality' %in% names(data)))
    {
        phe$quality <- 0
    } else
    {
        phe$quality <- data$quality
    }
    dbcols <- DBI::dbListFields(con, 'expdb_phenotype')
    
    phe <- phe[,match(dbcols, names(phe))]
    phe <- stats::na.omit(phe)
    phe$date <- format(phe$date, '%Y-%m-%d')
    DBI::dbWriteTable(con, 'expdb_phenotype', phe,
                      append = TRUE, row.names = FALSE)
}

#' Get phenotype values through a group of conditions
#' @param con a connection object as produced by dbConnect
#' @param traits A list of traits. All traits will be returned if NULL
#' @param direction One of 'long' or 'wide' for reshape function
#' @param tt Whether to calculate thermal time
#' @param gene Whether to get gene information
#' @param ... All other arguments to define range of export trials
#' @return a data.frame for selected phenotypic values
#' @export
dbGetPhenotype <- function(con, traits = NULL, direction = 'long',
                           tt = FALSE, gene = FALSE, ...) {
    trials <- dbGetTrials(con, ...)
    phe <- dbGetPhenotype_(
        con, trials = trials,
        traits = traits, 
        direction = direction,
        tt = tt, 
        gene = gene)
    phe
}

# Get phenotype values through a group of conditions
# @param con a connection object as produced by dbConnect
# @param trials Trials for filter
# @param traits A list of traits. All traits will be returned if NULL
# @param direction One of 'long' or 'wide' for reshape function
# @param tt Whether to calculate thermal time
# @param gene Whether to get gene information
dbGetPhenotype_ <- function(con, trials, traits = NULL, direction = 'long',
                            tt = FALSE, gene = FALSE)
{
    # Get trials
    
    if (gene)
    {
        sql <- sprintf('
SELECT G.[name] AS genotype, A.[Gene] AS gene, A.[Allele] AS allele FROM
       (SELECT A.[Gene], A.[Allele], A.[Description], G.[genotype_id]  FROM expdb_genotype_gene G
       LEFT OUTER JOIN
            (SELECT G.[name] AS Gene, A.[name] AS Allele, A.[description] AS Description, A.[id] a_id
            FROM expdb_gene_allele A LEFT OUTER JOIN expdb_gene G ON A.[gene_id] == G.[id] WHERE 1) A
       ON A.[a_id] == G.[allele_id] WHERE 1) A
LEFT OUTER JOIN expdb_genotype G ON A.[genotype_id] == G.[id]
WHERE G.[name] %s
            ', sqlIn(trials$genotype))
        genes <- DBI::dbGetQuery(con, sql)
        genes <- reshape2::dcast(genes, genotype ~ gene, value.var = 'allele')
        trials <- dplyr::left_join(trials, genes, by = 'genotype')
    }
    # Get phenotype
    where_traits <- ''
    if (!is.null(traits))
    {
        where_traits <- sprintf(' AND traits %s', sqlIn(traits))
    }
    sql <- sprintf('SELECT plot_id, node, name as traits,
        label, sample, date, value, quality
        FROM expdb_phenotype P
        LEFT OUTER JOIN expdb_trait T ON P.[trait_id]=T.id
        LEFT OUTER JOIN
             (SELECT id, name as node FROM expdb_node) PHY on P.[node_id] = PHY.id
        WHERE plot_id %s %s', sqlIn(trials$plot_id), where_traits)
    
    phenotype <- DBI::dbGetQuery(con, sql)
    
    # Generate output data.frame
    # Merge trials and phenotype
    phenotype <- merge(trials, phenotype)
    trt_cols <- dbGetTreatmentColumns(con, phenotype$plot_id)
    # Remove all id columns
    cols_names <- names(phenotype)
    phenotype <- phenotype[!(cols_names %in% cols_names[grep('_id', cols_names)])]
    
    if (nrow(phenotype) == 0)
    {
        return(phenotype)
    }
    phenotype$date <- as.Date(phenotype$date)
    trials$sowing <- as.Date(trials$sowing)
    # Calculate thermal time
    if (tt)
    {
        
        mets <- unique(phenotype$met)
        for (i in seq(along = mets))
        {
            met <- dbGetWeather(con, mets[i])
            met_date <- yearDay2Date(met$day, met$year)
            avgt <- (met$maxt + met$mint) / 2
            avgt <- ifelse(avgt > 0, avgt, 0)
            met_tt <- cumsum(avgt)
            pos <- phenotype$met %in% mets[i]
            sowing_tt <- met_tt[match(phenotype$sowing[pos], met_date)]
            obs_tt <- met_tt[match(phenotype$date[pos], met_date)]
            phenotype$tt[pos] <- obs_tt - sowing_tt
        }
    }
    
    if (direction == 'wide')
    {
        # Reshape to wide format
        phenotype$label <- NULL
        idvar <- names(phenotype)[!(names(phenotype) %in% c('value', 'traits'))]
        phenotype <- stats::reshape(phenotype, v.names = 'value',
                                    timevar = 'traits', idvar = idvar,
                                    direction = 'wide')
        names(phenotype) <- gsub('value.', '', names(phenotype))
    }
    phenotype <- phenotype %>%
        tibble::tibble() %>%
        dplyr::mutate(das = as.numeric(.data$date) - as.numeric(.data$sowing))
    
    # Convert nodes into stem and rank
    nodes <- unique(phenotype$node)
    if (sum(stringr::str_detect(nodes, '(.*)_L(.*)')) > 0) {
        phe_no_rank <- phenotype %>% 
            dplyr::filter(!stringr::str_detect(.data$node, '(.*)_L(.*)')) %>% 
            dplyr::mutate(stem = NA,
                          rank = NA)
        phe_rank <- phenotype %>% 
            dplyr::filter(stringr::str_detect(.data$node, '(.*)_L(.*)')) %>% 
            dplyr::mutate(stem = stringr::str_replace(.data$node, '(.*)_(.*)', '\\1'),
                          rank = as.numeric(stringr::str_replace(.data$node, '(.*)_L(.*)', '\\2')))
        
        phe_pos_rank <- phe_rank %>% 
            dplyr::filter(.data$rank >= 0)
        
        phe_neg_rank <- phe_rank %>% 
            dplyr::filter(.data$rank < 0)
        if (nrow(phe_neg_rank) > 0) {
            sql <- 'SELECT HE.name as node, NODE.[name] as parent 
                     FROM (SELECT node_id, parent_node_id, name FROM expdb_node_heritage HERI 
                     LEFT OUTER JOIN expdb_node N ON N.id == HERI.node_id) HE
                     LEFT OUTER JOIN expdb_node NODE ON NODE.[id]== HE.[parent_node_id] 
                     WHERE 1'
            nodes_heritage <- DBI::dbGetQuery(con, sql)
            fln <- dbGetOrganFinalLeafNumber(con) %>% 
                dplyr::mutate(node = ifelse(.data$node == 'Field', 
                                            'MainStem', .data$node)) %>% 
                dplyr::rename(parent = .data$node,
                              fln = .data$value) %>% 
                dplyr::select(!dplyr::all_of(c('traits', 'source', 'std')))
            phe_neg_rank <- phe_neg_rank %>% 
                dplyr::left_join(nodes_heritage, by = 'node') %>% 
                dplyr::left_join(fln, by = c(trt_cols, 'parent')) %>% 
                dplyr::mutate(rank = round(.data$fln) + .data$rank + 1)
        }
        
        phenotype <- dplyr::bind_rows(phe_no_rank, phe_pos_rank, phe_neg_rank)
    }
    
    
    return(phenotype)
}
