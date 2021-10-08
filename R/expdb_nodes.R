# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   02:33 PM Monday, 20 July 2015
# * Copyright: AS IS

# Nodes in expDB

#' Add nodes into expDB
#' 
#' @param con a connection object as produced by dbConnect
#' @param data phenotype value
#' @return no return values
#' @export
dbAddNodes <- function(con, data) {
    names(data) <- tolower(names(data))
    data$parent[is.na(data$parent)] <- 'Experiment'
    expdb_nodes <- data.frame(
        name = unique(as.character(unlist(data))),
        stringsAsFactors = FALSE) %>% 
        dplyr::filter(!is.na(.data$name))
    
    dbInsertUpdateByRow(con, 'expdb_node', 
                        expdb_nodes)
    
    db_nodes <- DBI::dbReadTable(con, 'expdb_node')
    node_id <- db_nodes$id[match(data$name, db_nodes$name)]
    num <- table(node_id)
    num <- as.numeric(names(num[num > 1]))
    if (length(num) > 0) {
        stop(paste0('Has duplicated node name: "', paste(data$name[num], collapse = ', '), '"'))
    }
    parent_id <- db_nodes$id[match(data$parent, db_nodes$name)]
    
    exists_node <- DBI::dbReadTable(con, 'expdb_node_heritage')
    
    new_node <- data.frame(node_id = node_id,
                           parent_node_id = parent_id)
    
    new_heritage <-  new_node %>%
        dplyr::anti_join(exists_node, by = c('node_id', 'parent_node_id'))
    duplicated_node <- exists_node %>% 
        dplyr::select(dplyr::all_of('node_id')) %>% 
        dplyr::semi_join(new_heritage %>% 
                             dplyr::select(dplyr::all_of('node_id')), 
                         by = 'node_id')
    if (nrow(duplicated_node) > 0) {
        strs <- db_nodes$name[match(duplicated_node$node_id, db_nodes$id)]
        p_name <- duplicated_node %>% 
            dplyr::left_join(exists_node, by = 'node_id') %>% 
            dplyr::left_join(db_nodes, by = c('parent_node_id' = 'id'))
        
        stop(paste0('Duplicated node maps for: ', paste(strs, collapse = ', '),
                    ' with parent name ', paste(p_name$name, collapse = ', ')))
    }
    
    
    DBI::dbWriteTable(con, 'expdb_node_heritage', new_heritage, append = TRUE,  row.names = FALSE)
}
