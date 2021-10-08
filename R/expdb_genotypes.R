# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# experimentDB API for genotype

#' Add genotypes into expDB
#'
#' @param con a connection object as produced by dbConnect
#' @param genotypes A string vector of genotypes
#' @return No return values
#' @export
dbAddGenotype <- function(con, genotypes)
{   
    col_names <- names(genotypes)
    col_names <- tolower(col_names)
    names(genotypes) <- col_names
    
    # genotypes$name <- genotypes$name
    # genotypes$name <- as.character(unique(genotypes$name))
    # Convert all genotypes to lowercase letter
    genotype_id <- getIdByUniqueIndex(con, 'expdb_genotype', genotypes$name, ignore_case = TRUE)
    genotypes <- genotypes[is.na(genotype_id),]
    if (nrow(genotypes) > 0)
    {
        g <- table(genotypes$name)
        g <- names(g)[g > 1]
        if (length(g) > 0)
        {
            stop(paste0('Duplicated genotypes: ', paste(g, collapse = ', ')))
        }
        pos <- col_names %in% DBI::dbListFields(con, 'expdb_genotype')
        data_db <- genotypes[,pos]
        if (sum(pos) == 1)
        {
            data_db <- data.frame(name = data_db)
        }
        dbInsertUpdateByRow(con, 'expdb_genotype', data_db)
        
        # Add gene information
        sql <- 'select * from expdb_gene'
        existing_genes <- DBI::dbGetQuery(con, sql)
        cols <- col_names[col_names %in% tolower(existing_genes$name)]
        
        if (length(cols) > 0)
        {
            geno_id <- getIdByUniqueIndex(con, 'expdb_genotype', genotypes$name, ignore_case = TRUE)
            gene_id <- getIdByUniqueIndex(con, 'expdb_gene', cols, ignore_case = TRUE)
            
            
            geno_gene <- NULL
            for (j in seq(along = cols))
            {
                allele_id <- getIdByUniqueIndex(con, 'expdb_gene_allele', 
                                                as.data.frame(list(gene_id = gene_id[j],
                                                                   name = genotypes[[cols[j]]])),
                                                unique_col = c('gene_id', 'name'))
                geno_gene <- rbind(geno_gene, as.data.frame(list(genotype_id = geno_id,
                                                                 allele_id = allele_id)))
            }
            geno_gene <- stats::na.omit(geno_gene)
            existing <- DBI::dbReadTable(con, 'expdb_genotype_gene')
            pos <- apply(geno_gene, 1, paste, collapse = '') %in% 
                apply(existing, 1, paste, collapse = '')
            if (sum(!pos))
            {
                DBI::dbAppendTable(con, 'expdb_genotype_gene', geno_gene[!pos,])
            }
        }
    }
    return(invisible())
}


#' Add gene information into database
#'
#' @param con a connection object as produced by dbConnect
#' @param genes A data.frame of genes
#' @return No return values
#' @export
dbAddGene <- function(con, genes)
{
    names(genes) <- tolower(names(genes))
    genes_name <- genes$name
    
    gene_id <- getIdByUniqueIndex(con, 'expdb_gene', genes_name)
    genes_name <- unique(genes_name[is.na(gene_id)])
    if (length(genes_name) > 0)
    {
        data <- NULL
        data$name <- genes_name
        data <- as.data.frame(data, stringsAsFactors = FALSE)
        dbInsertUpdateByRow(con, 'expdb_gene', data)
    }
    return(invisible())
}

#' Add gene allele information into database
#'
#' @param con a connection object as produced by dbConnect
#' @param genes A data.frame of genes
#' @return No return values
#' @export
dbAddGeneAllele <- function(con, genes)
{
    names(genes) <- tolower(names(genes))
    genes$gene_id <- getIdByUniqueIndex(con, 'expdb_gene', genes$name)
    genes$name <- genes$allele
    dbcols <- DBI::dbListFields(con, 'expdb_gene_allele')
    genes <- genes[,c('gene_id', 'name', 'description')]
    dbInsertUpdateByRow(con, 'expdb_gene_allele', genes,
                        unique_col = c('gene_id', 'name'))
    return(invisible())
}

#' Check genotype names
#'
#' @param con a connection object as produced by dbConnect
#' @param genotype The genotype name will be checked
#' @return A vector with check genotype names
#' @export
dbGenotypeCheckName <- function(con, genotype)
{
    g_db <- DBI::dbGetQuery(con, 'SELECT * FROM expdb_genotype')
    genotype <- tolower(genotype)
    res <- genotype
    for (i in seq(nrow(g_db)))
    {
        a <- NULL
        if (tibble::has_name(g_db, 'breedingline') & sum(is.na(g_db$breedingline)) != nrow(g_db))
        {
            if (!is.na(g_db$breedingline[i]))
            {
                a <- g_db$breedingline[i]
            }
        }
        if (tibble::has_name(g_db, 'alias') & sum(is.na(g_db$alias)) != nrow(g_db))
        {
            if (!is.na(g_db$alias[i]))
            {
                a <- c(a, strsplit(g_db$alias[i], ';')[[1]])
            }
        }
        pos <- genotype %in% tolower(a)
        if (sum(pos) > 0)
        {
            res[pos] <- g_db$name[i]
        }
    }
    res
}

# #' Merge genotype with synonyms
# #'
# #' @param con a connection object as produced by dbConnect
# #' @param genotype The genotype name will be merged into
# #' @param ... Other synonyms names 
# mergeGenotype <- function(con, genotype, ...)
# {
# genotype <- 'ZIPPY'
# synonyms <- 'IGW2838'
# synonyms <- unlist(list(...))
# if (length(synonyms) == 0)
# {
# return()
# }

# synonyms <- tolower(synonyms)
# genotype <- tolower(genotype)

# genotype_id <- getIdByUniqueIndex(con, 'expdb_genotype', genotype)
# if (length(genotype_id) == 0)
# {
# stop(sprintf('%s doesn\' exist', genotype))
# }
# synonyms_id <- getIdByUniqueIndex(con, 'expdb_genotype', synonyms)
# }


#' Get the genotype information 
#'
#' @param con a connection object as produced by dbConnect
#' @param name_only Only return the name of genotypes
#' @return data.frame with genotype information or a vector with genotype name 
#' if name_only = TRUE.
#' @export
dbGetGenotype <- function(con, name_only = FALSE)
{
    if (name_only)
    {
        sql <- 'SELECT name  FROM expdb_genotype  WHERE 1'
        genotypes <- DBI::dbGetQuery(con, sql)
    } else
    {
        sql <- '
    SELECT G.`Crop` as crop, G.`name` AS genotype, A.`Gene` AS gene, A.`Allele` AS allele FROM 
        expdb_genotype G LEFT OUTER JOIN     
       (SELECT A.[Gene], A.[Allele], A.[Description], G.[genotype_id]  FROM expdb_genotype_gene G 
           LEFT OUTER JOIN 
                (SELECT G.[name] AS Gene, A.[name] AS Allele, A.[description] AS Description, A.[id] a_id 
                FROM expdb_gene_allele A LEFT OUTER JOIN expdb_gene G ON A.[gene_id] == G.[id] WHERE 1) A 
           ON A.[a_id] == G.[allele_id] WHERE 1) A
     ON A.[genotype_id] == G.[id] 
    WHERE 1'
        
        genotypes <- DBI::dbGetQuery(con, sql)
        genotypes <- genotypes %>% 
            dplyr::filter(!is.na(.data$gene)) %>%
            tidyr::spread('gene', 'allele') %>% 
            dplyr::full_join(
                genotypes %>% 
                    dplyr::filter(is.na(.data$gene)) %>% 
                    dplyr::select(dplyr::all_of(c('crop', 'genotype'))),
                by = c('genotype', 'crop')
            ) %>% tibble::tibble()
        
    }
    genotypes
}


#' Get the gene information
#' @param con a connection object as produced by dbConnect
#' @return a data.frame with all gene information
#' @export
dbGetGene <- function(con)
{
    sql <- 'SELECT gene, name AS allele, description FROM expdb_gene_allele AS A JOIN (SELECT id AS gene_id, name as gene FROM expdb_gene WHERE 1) AS G ON G.gene_id = A.gene_id WHERE 1'
    DBI::dbGetQuery(con, sql)
}

