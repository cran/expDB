# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   2:37 PM Saturday, 15 September 2012
# * Copyright: AS IS
# *

# Data inport

#' Import data from excel file
#' @param con a connection object as produced by dbConnect
#' @param xlsx The path to excel file
#' @param ignore_genotype Ignore genotype tables when importing
#' @param ignore_trait Ignore trait table when importing
#' @param ... Other arguments. Supported arguments include
#' \itemize{
#'    \item{extra_design: Extra columns in the experiment design.}
#'    \item{tz: The time zone for the hourly climates.}
#' }
#' @return No return values
#' @export
dbImportXLSX <- function(con, xlsx, ignore_genotype = TRUE,
                         ignore_trait = TRUE, ...)
{
    other_args <- list(...)
    sheets <- xlsxToR(xlsx)
    names(sheets) <- tolower(names(sheets))

    # Import source
    if (!is.null(sheets$source))
    {
        source <- sheets$source
        if (nrow(source) > 0)
        {
            dbAddSource(con, source)
        }
    }
    # Import researcher
    if (!is.null(sheets$researchers))
    {
        researchers <- sheets$researchers
        if (nrow(researchers) > 0)
        {
            dbAddResearcher(con, researchers)
        }
    }

    # Import log
    if (!is.null(sheets$log))
    {
        log <- sheets$log
        names(log) <- tolower(names(log))
        if (nrow(log) > 0)
        {
            dbAddLog(con, log$log, format(log$date))
        }
    }

    # Import met
    if (!is.null(sheets$met))
    {
        mets <- sheets$met
        if (nrow(mets) > 0)
        {
            names(mets) <- tolower(names(mets))
            mets$number <- as.character(mets$number)
            dbAddMets(con, mets[,c('name','number','type','longitude','latitude')])
            if (!is.null(mets$filename))
            {
                
                file_paths <- file.path(dirname(xlsx), gsub('\\\\', '/', mets$filename))
                if (sum(!file.exists(file_paths)) > 0)
                {
                    cat('Met files don\'t exist:')
                    cat("\r\n")
                    cat(paste( mets$filename[!file.exists(file_paths)], collapse = '\r\n'))
                    stop('Check met files')
                }
                for (i in seq(along = mets[[1]]))
                {
                    if (mets$type[i] == "daily") {
                        records <- weaana::readWeatherRecords(file_paths[i])
                        dbAddWeather(con, records,
                            mets$name[i])
                    } else if (mets$type[i] == "hourly") {
                        
                        records <- utils::read.csv(file_paths[i], as.is = TRUE)
                        if (!tibble::has_name(records, "timestamp")) {
                            stop("missing the timestamp column in hourly climate.")
                        }
                        if (!tibble::has_name(records, "temperature")) {
                            stop("missing the temperature column in hourly climate.")
                        }
                        if (is.null(other_args$tz)) {
                            stop("Argument tz should be used to hourly temperature.")
                        }
                        
                        if (!tibble::has_name(mets, "timestampformat")) {
                           stop('The column "TimestampFormat" has to be specified for hourly weather data.') 
                        }
                        records$timestamp <- as.POSIXct(records$timestamp, tz = other_args$tz, format = mets$timestampformat[i])
                        if (sum(is.na(records$timestamp)) > 0 ) {
                            stop("Missing values in the timestamp column")
                        }
                        if (sum(is.na(records$temperature)) > 0 ) {
                            stop("Missing values in the timestamp column")
                        }
                        
                        message("The timestamp column in the weather file (", 
                                file_paths[i], 
                                ") is converted to POSIXct with format (", 
                                mets$timestampformat[i],
                                ") and timezone (", 
                                other_args$tz,
                                ").")
                        message("The first value is ", records$timestamp[1], ".")
                        
                        dbAddWeather(con, records[, c("timestamp", "temperature")],
                                     mets$name[i])
                    }
                }
            }
        }
    }

    # Import gene
    if (!ignore_genotype & !is.null(sheets$genes))
    {
        genes <- sheets$genes
        if (nrow(genes) > 0)
        {
            dbAddGene(con, genes)
            dbAddGeneAllele(con, genes)
        }
    }

    # Import genotypes
    if (!ignore_genotype & !is.null(sheets$genotypes))
    {
        genotypes <- sheets$genotypes
        if (nrow(genotypes) > 0)
        {
            dbAddGenotype(con, genotypes)
        }
    }

    # Import traits
    if (!ignore_trait & !is.null(sheets$traits))
    {
        traits <- sheets$traits
        if (nrow(traits) > 0)
        {
            dbAddTraits(con, traits)
        }
    }

    # # Import apsoil
    # if (!is.null(sheets$apsoils) & !is.null(other_args$apsoil))
    # {
    #     apsoil <- sheets$apsoils
    #     if (nrow(apsoil) > 0)
    #     {
    #         addApsoils(con, apsoil, other_args$apsoil)
    #     }
    # }

    # Import sites
    if (!is.null(sheets$sites))
    {
        sites <- sheets$sites
        if (nrow(sites))
        {
            dbAddSites(con, sites)
        }
    }

    # Import trials
    if (!is.null(sheets$trials))
    {
        trials <- sheets$trials
        trials <- trials[!is.na(trials$Sowing),]
        if (nrow(trials) > 0)
        {
            dbAddTrials(con, trials)
        }
    }

    # Import trial soils
    if (!is.null(sheets$trialsoils))
    {
        soils <- sheets$trialsoils
        if (nrow(soils) > 0)
        {
            dbAddTrialSoil(con, soils)
        }
    }

    # Import irrigation
    if (!is.null(sheets$irrigation))
    {
        irrigation <- sheets$irrigation
        if (nrow(irrigation) > 0)
        {
            dbAddIrrigatons(con, irrigation)
        }
    }

    # Import fertilization
    if (!is.null(sheets$fertilization))
    {
        fertilization <- sheets$fertilization
        if (nrow(fertilization) > 0)
        {
            dbAddFertilization(con, fertilization)
        }
    }

    # Import nodes
    if (!is.null(sheets$nodes)) {
        nodes <- sheets$nodes
        if (nrow(nodes) > 0) {
            dbAddNodes(con, nodes)
        }
    }

    # Import design
    design_sheets <- names(sheets)[grep('design', names(sheets))]
    if (length(design_sheets) > 0)
    {
        for (i in seq(along = design_sheets))
        {
            design <- sheets[[design_sheets[i]]]
            if (nrow(design) > 0)
            {
                dbAddDesigns(con, design, extra_design = other_args$extra_design)
            }
        }
    }
    
    
    # import phenotype
    phe_sheets <- names(sheets)[grep('phenotype', names(sheets))]
    if (length(phe_sheets) > 0)
    {
        for (i in seq(along = phe_sheets))
        {
            # Compatibility with old version
            phenotype <- sheets[[phe_sheets[i]]]
            
            if (nrow(phenotype) == 0)
            {
                next()
            }
            names(phenotype) <- tolower(names(phenotype))
            traits <- NULL
            if (tibble::has_name(phenotype, 'traits') | tibble::has_name(phenotype, 'trait')) {
                data_alias <- list(traits = c('trait'))
                names(phenotype) <- checkAlias(names(phenotype), data_alias)
                traits <- unique(phenotype$traits)
            }
            
            if (length(traits) == 0) {
                
                traits <- 'columns'
                old_names <- names(phenotype)
                phenotype$traits <- 'columns'
                new_names <- c('traits', old_names)
                phenotype <- phenotype[,new_names]
            }
            traits <- tolower(traits)
            # Normal dataset
            if (length(traits) == 1 & 'quadratharvest' %in% traits)
            {
                # for quadrat harvest
                harvestQuadratDetail(con, phenotype)
            } else if (length(traits) == 1 & 'leafsize' %in% traits){
                # for leaf size
                measurementLeafSize(con, phenotype)
            } else {
                # for nromal measurement
                defaultMeasurement(con, phenotype, traits)
            }
        }
    }
    
}


