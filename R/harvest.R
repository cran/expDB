# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# Process Harvest data

#' Process quadrat (detail) harvestQuadratDetail
#' @param con a connection object as produced by dbConnect
#' @param records Phenotype records
#' @return no return values
#' @export
harvestQuadratDetail <- function(con, records)
{
    phenotype_alias <- list(trial = c('trialcode'),
                            row = c('plot'), column = c('range'))
    names(records) <- checkAlias(names(records), phenotype_alias)
    
    # Check value
    checkValue <- function(x)
    {
        x <- as.numeric(x)
        ifelse(is.na(x), 0, x)
    }
    
    # Check whether separate leaf dry weight
    if (!tibble::has_name(records, 'dryweightgreenleafsub')) {
        records$dryweightgreenleafsub <- 0
    }
    
    # Check whether separate leaf dry weight
    if (!tibble::has_name(records, 'dryweightgrain')) {
        records$dryweightgrain <- NA
    }
    
    # Adjust head and grain weight by head number and stem number
    if (tibble::has_name(records, 'headnumber')) {
        pos <- !is.na(records$headnumber)
        ratio <- (records$stemnumber[pos] / records$headnumber[pos])
        records$dryweighthead[pos] <- ratio * records$dryweighthead[pos]
        records$freshweighthead[pos] <- ratio * records$freshweighthead[pos]
    } else {
		records$headnumber <- NA
	}
    
    traits_col <- c('freshweighttotal', 'freshweight', 'stemnumber', 'freshweightgreenleafsub',
                    'freshweightgreenleafremain', 'freshweightstem', 'headnumber',
                    'freshweighthead', 'freshweightdeathleaf', 'leafareagreenleafsub', 
                    'dryweightgreenleafsub', 'dryweightgreenleafremain', 'dryweightstem', 
                    'dryweighthead', 'dryweightdeathleaf', 'dryweightgrain')
    
    pos <- records[,traits_col] %>% 
        apply(1, function(x) sum(is.na(as.numeric(unlist(x)))))
    records <- records[pos < length(traits_col),]
    
    phe <- records %>% 
        
        dplyr::mutate(
            # Convert character into numeric
            area = as.numeric(.data$harvestarea),
            area_m2 = .data$area / 10000,
            totalfreshweight = checkValue(.data$freshweighttotal),
            substemfreshweight = checkValue(.data$freshweightstem),
            subheadfreshweight = checkValue(.data$freshweighthead),
            subdeathleafdryweight = checkValue(.data$dryweightdeathleaf),
            subdeathleaffreshweight = checkValue(.data$freshweightdeathleaf),

            subleafdryweight = checkValue(.data$dryweightgreenleafremain) + 
                checkValue(.data$dryweightgreenleafsub),
            substemdryweight = checkValue(.data$dryweightstem),
            substemfreshweight = checkValue(.data$freshweightstem),
            
            subdeathleafdryweight = checkValue(.data$dryweightdeathleaf),
            subheaddryweight = checkValue(.data$dryweighthead),
            subgraindryweight = checkValue(.data$dryweightgrain),
            substemnumber = checkValue(.data$stemnumber),
            subfreshweight = checkValue(.data$freshweight),
            subsubleafarea = .data$leafareagreenleafsub,
            subsubleaffreshweight = checkValue(.data$freshweightgreenleafsub),
            subotherleaffreshweight = checkValue(.data$freshweightgreenleafremain),
            # Total dry weight of subsample
            subtotaldryweight = .data$subleafdryweight +
                .data$substemdryweight +
                .data$subdeathleafdryweight +
                .data$subheaddryweight,
            # Stem number
            stemnumber = (.data$substemnumber * .data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            headnumber = (.data$headnumber * .data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            # Leaf area index
            lai = ((.data$subsubleafarea * (.data$subsubleaffreshweight + .data$subotherleaffreshweight) /
                        .data$subsubleaffreshweight) * .data$totalfreshweight / .data$subfreshweight) / .data$area,
            # leaf fresh weight
            freshweightgreenleaf = (.data$subsubleaffreshweight + .data$subotherleaffreshweight) * 
                (.data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            # death leaf fresh weight
            freshweightdeathleaf = .data$subdeathleaffreshweight * (.data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            # stem fresh weight
            freshweightstem = .data$substemfreshweight * (.data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            # head fresh weight
            freshweighthead = .data$subheadfreshweight * (.data$totalfreshweight / .data$subfreshweight) / .data$area_m2,
            # Dry weight above ground
            dryweightaboveground = (.data$subtotaldryweight * .data$totalfreshweight / .data$subfreshweight) /
                .data$area_m2,
            # Dry weight leaf
            dryweightgreenleaf = .data$subleafdryweight * .data$dryweightaboveground / .data$subtotaldryweight,
            # Dry weight stem
            dryweightstem = .data$substemdryweight * .data$dryweightaboveground / .data$subtotaldryweight,
            # Dry weight head
            dryweighthead = .data$subheaddryweight * .data$dryweightaboveground / .data$subtotaldryweight,
            # Dry weight death leaf
            dryweightdeathleaf = .data$subdeathleafdryweight * .data$dryweightaboveground / .data$subtotaldryweight,
            # Dry weight grain
            dryweightgrain = .data$subgraindryweight * .data$dryweightaboveground / .data$subtotaldryweight,
            specificleafarea = .data$lai / .data$dryweightgreenleaf
            ) %>%
        dplyr::rename(f_stemnumber = .data$stemnumber, 
                      f_headnumber = .data$headnumber,
                      f_leafareaindex = .data$lai, 
                      f_freshweightaboveground = .data$totalfreshweight,
                      f_freshweightgreenleaf = .data$freshweightgreenleaf,
                      f_freshweightdeathleaf = .data$freshweightdeathleaf,
                      f_freshweightstem = .data$freshweightstem,
                      f_freshweighthead = .data$freshweighthead,
                      f_dryweightaboveground = .data$dryweightaboveground,
                      f_dryweightgreenleaf = .data$dryweightgreenleaf, 
                      f_dryweightstem = .data$dryweightstem, 
                      f_dryweighthead = .data$dryweighthead, 
                      f_dryweightgrain = .data$dryweightgrain,
                      f_dryweightdeathleaf = .data$dryweightdeathleaf,
                      f_specificleafarea = .data$specificleafarea)
        # Only store the selected columns
        dplyr::select(dplyr::all_of(c('year', 'site', 'column', 'row', 'date', 
               'f_stemnumber', 
               'f_headnumber', 
               'f_leafareaindex', 
               'f_freshweightaboveground',
               'f_freshweightgreenleaf',
               'f_freshweightdeathleaf',
               'f_freshweightstem',
               'f_freshweighthead',
               'f_dryweightaboveground',
               'f_dryweightgreenleaf', 
               'f_dryweightstem', 
               'f_dryweighthead', 
               'f_dryweightgrain',
               'f_dryweightdeathleaf',
               'f_specificleafarea'))) %>% 
        dplyr::mutate(o_dryweightperstem = .data$f_dryweightaboveground / .data$f_stemnumber,
			   f_dryweightabovegroundnodeadleaf = .data$f_dryweightgreenleaf + .data$f_dryweightstem + .data$f_dryweighthead,
			   f_dryweightaboveground = .data$f_dryweightgreenleaf + .data$f_dryweightstem + .data$f_dryweighthead + .data$f_dryweightdeathleaf
) %>% 
        # Convert into key value format
        tidyr::gather('traits', 'value', !dplyr::all_of(c('year', 'site', 'column', 'row', 'date'))) %>% 
        # Set the quality
        dplyr::mutate(quality = 0) %>% 
        as.data.frame(stringsAsFactors = FALSE) 
    
    invisible(dbAddPhenotype(con, phe))
}
    
# Import a normal measurement
# 
# @param con a connection object as produced by dbConnect
# @param phenotype Phenotype records
# @param traits traits
# @return no return values
defaultMeasurement <- function(con, phenotype, traits) {
    col_names <- tolower(names(phenotype))
    pos_col <- !(col_names %in% c('notes', 'comment', 'comments'))
    phenotype <- phenotype[,pos_col]
    col_names <- col_names[pos_col]
    names(phenotype) <- col_names
    # trait_name <- strsplit(phe_sheets[i], '_')
    # trait_name <- tolower(trait_name[[1]][2])
    pos <- match('date', names(phenotype))
    if (is.na(pos))
    {
        stop('No Date column was found.')
    }
    
    quality <- NULL
    if (tibble::has_name(phenotype, 'quality'))
    {
        quality <- phenotype$quality
        phenotype$quality <- NULL
    }
    
    # Check multiple column traits
    if (length(traits) == 1 & sum(tolower(traits) == 'columns') > 0) {

        idx <- seq(pos, ncol(phenotype) - 1)
        quality <- rep(quality, times = length(idx))
        # Check sample
        if (!tibble::has_name(phenotype, 'sample')) {
            phenotype$sample <- 1
        }
        phenotype <- phenotype %>%
            dplyr::select(!dplyr::all_of('traits')) %>%
            tidyr::gather('traits', 'value', dplyr::all_of(idx))
    }
    
    
    if (!tibble::has_name(phenotype, 'sample') & ncol(phenotype) > pos + 1)
    {
        # Contain multiple sample in wide direction
        samples <-  paste0('S', seq(pos + 1, ncol(phenotype)) - pos)
        names(phenotype) <- c(col_names[seq(1, pos)], samples)
        phenotype <- reshape2::melt(phenotype, id.vars = col_names[seq(1, pos)],
                          measure.vars = samples,
                          variable.name = 'sample')
        phenotype$sample <- as.numeric(gsub('S', '', as.character(phenotype$sample)))
    } else if (ncol(phenotype) == pos + 1 & !tibble::has_name(phenotype, 'sample'))
    {
        # Only a single value and no sample column
        names(phenotype) <- c(names(phenotype)[seq(1, ncol(phenotype)) - 1], 'value')
        phenotype$sample <- 1
    }
    
    if (!(is.null(quality)))
    {
        phenotype$quality <- quality
    }
    # phenotype$traits <- trait_name
    dbAddPhenotype(con, phenotype)
}

# Import the measurement of leaf size
# 
# @param con a connection object as produced by dbConnect
# @param phenotype Phenotype records
measurementLeafSize <- function(con, phenotype) {
    phenotype_alias <- list(trial = c('trialcode'),
                            row = c('plot'), column = c('range'))
    names(phenotype) <- checkAlias(names(phenotype), phenotype_alias)
    
    # Check whether have the leaf number (default == 1)
    if (!tibble::has_name(phenotype, 'm_ls_leafnumber')) {
        phenotype$m_ls_leafnumber <- 1
    }
    
    # Check other measurements
    if (!tibble::has_name(phenotype, 'm_ls_areaorgan')) {
        phenotype$m_ls_areaorgan <- NA
    }
    
    if (!tibble::has_name(phenotype, 'm_ls_freshweightorgan')) {
        phenotype$m_ls_freshweightorgan <- NA
    }
    
    if (!tibble::has_name(phenotype, 'm_ls_dryweightorgan')) {
        phenotype$m_ls_dryweightorgan <- NA
    }
 
    
    # Calculate the new variables
    p <- phenotype %>% 
        dplyr::mutate(o_leafarea = .data$m_ls_areaorgan / .data$m_ls_leafnumber,
               o_freshweightgreenleaf = .data$m_ls_freshweightorgan / .data$m_ls_leafnumber,
               o_dryweightgreenleaf = .data$m_ls_dryweightorgan / .data$m_ls_leafnumber,
               o_specificleafarea = .data$o_leafarea / .data$o_dryweightgreenleaf) %>% 
        dplyr::select(dplyr::all_of(c('year', 'site', 'column', 'row', 'node', 'date', 'o_leafarea',
               'o_freshweightgreenleaf', 'o_dryweightgreenleaf', 'o_specificleafarea'))) %>% 
        # Convert into key value format
        tidyr::gather('traits', 'value', !dplyr::all_of(c('year', 'site', 'column', 'row', 'node', 'date'))) %>% 
        # Set the quality
        dplyr::mutate(quality = 0) %>% 
        as.data.frame(stringsAsFactors = FALSE) 
    invisible(dbAddPhenotype(con, p))
}
