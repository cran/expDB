#' # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
#' # * Created:   10:31 PM Sunday, 19 August 2012
#' # * Copyright: AS IS
#' # *
#' 
#' # experimentDB API for soils
#' 
#' #' Insert or Update soil into expDB
#' #' @param con a connection object as produced by dbConnect
#' #' @param data A data frame includes all columns
#' #' @param apsoil Path to apsoil (Soils.soils)
#' #' @export
#' addApsoils <- function(con, data, apsoil)
#' {
#'     soil_alias <- list(soilid = c('soils', 'soil'))
#'     names(data) <- checkAlias(names(data), soil_alias)
#'     
#'     res <- parseApsoil(data, apsoil)
#'     soil_data <- res$soil_data
#'     layer_data <- res$layer_data
#'     main_fields <- c('Country', 'Region', 'LocalName', 'SoilType', 
#'         'NearestTown', 'NaturalVegetation', 'State', 'ApsoilNumber', 
#'         'LocationAccuracy', 'DataSource', 'Comments', 'SummerDate',
#'         'WinterDate')
#'     table_filed <- DBI::dbListFields(con, 'expdb_apsoil')
#'     main_fields <- tolower(main_fields)    
#'     names(soil_data) <- tolower(names(soil_data))
#'     data <- cbind(data, 
#'         soil_data[,main_fields[main_fields %in% table_filed]])
#'     # Write to expdb_apsoil
#'     dbInsertUpdateByRow(con, 'expdb_apsoil', data, unique_col = 'soilid')
#'     
#'     pos <- !(names(soil_data) %in% main_fields)
#'     data_field <- names(soil_data)[pos]
#'     soil_data <- soil_data[,pos]
#'     
#'     # Write apsim soil data
#'     soil_id <- getIdByUniqueIndex(con, 'expdb_apsoil', data, 'soilid')
#'     if (sum(is.na(soil_id)) > 0)
#'     {
#'         stop(paste('Soils "', paste(unique(data$soil[is.na(soil_id)]), collapse = ', '), 
#'                 '" are not in the database', sep = ''))
#'     }
#'     trait_id <- getIdByUniqueIndex(con, 'expdb_trait', as.data.frame(list(name = data_field)))
#'     if (sum(is.na(trait_id)) > 0)
#'     {
#'         data_field <- data_field[!is.na(trait_id)]
#'         soil_data <- soil_data[,!is.na(trait_id)]
#'         trait_id <- trait_id[!is.na(trait_id)]
#'         # stop(paste('Traits "', paste(unique(data_field[is.na(trait_id)]), collapse = ', '), 
#'                 # '" are not in the database', sep = ''))
#'     }
#'     expdb_apsoil_data <- NULL
#'     expdb_apsoil_data$soil_id <- rep(soil_id, length(data_field))
#'     expdb_apsoil_data$trait_id <- rep(trait_id, nrow(soil_data))
#'     expdb_apsoil_data$value <- as.numeric(as.character(unlist(t(soil_data))))
#'     expdb_apsoil_data <- as.data.frame(expdb_apsoil_data)
#'     expdb_apsoil_data <- expdb_apsoil_data[!is.na(expdb_apsoil_data$value),]
#'     DBI::dbWriteTable(con, 'expdb_apsoil_data', expdb_apsoil_data, row.names = FALSE, append = TRUE)
#'     
#'     # Write apsim soil layer
#'     layer_data$soil_id <- soil_id[match(layer_data$Soil, data$soilid)]
#'     layer_data$from_depth <- layer_data$Depth - layer_data$Thickness
#'     layer_data$to_depth <- layer_data$Depth
#' 
#'     layer_infor <- unique(layer_data[,c('soil_id', 'from_depth', 'to_depth')])
#'     dbInsertUpdateByRow(con, 'expdb_apsoil_layer', layer_infor, 
#'         unique_col = c('soil_id', 'from_depth', 'to_depth'))
#'     
#'     # Write layer data
#'     layer_id <- getIdByUniqueIndex(con, 'expdb_apsoil_layer', 
#'         layer_infor, unique_col = c('soil_id', 'from_depth', 'to_depth'))
#'     if (sum(is.na(layer_id)) > 0)
#'     {
#'         stop(paste('Layer "', paste(unique(layer_infor[is.na(layer_id),]), collapse = ', '), 
#'                 '" are not in the database', sep = ''))
#'     }
#'     
#'     names(layer_data) <- tolower(names(layer_data))
#'     
#'     layer_data$layer_id <- layer_id[match(paste(layer_data$soil_id, layer_data$from_depth, layer_data$to_depth),
#'         paste(layer_infor$soil_id, layer_infor$from_depth, layer_infor$to_depth))]
#'     traits <- as.character(unique(layer_data$trait))
#'     trait_id <- getIdByUniqueIndex(con, 'expdb_trait', as.data.frame(list(name = tolower(traits))))
#'     if (sum(is.na(trait_id)) > 0)
#'     {
#'         stop(paste('Traits "', paste(traits[is.na(trait_id)], collapse = ', '), 
#'                 '" are not in the database', sep = ''))
#'     }
#'     layer_data$trait_id <- trait_id[match(layer_data$trait, traits)]
#'     layer_data <- layer_data[!is.na(layer_data$value),c('layer_id', 'trait_id', 'value')]    
#'     DBI::dbWriteTable(con, 'expdb_apsoil_layer_data', layer_data, row.names = FALSE, append = TRUE)
#' }
#' 
#' 
#' #' parse APSIM soil file
#' #' @param data the data object
#' #' @param apsoil file path to apsoil
#' #' 
#' #' @export
#' parseApsoil <- function(data, apsoil)
#' {
#'     if (!file.exists(apsoil))
#'     {
#'         stop(paste('File doesn\'t exist: ', apsoil, sep = ''))
#'     }
#'     
#'     apsoil <- XML::xmlInternalTreeParse(apsoil)
#'     soil_data <- NULL
#'     layer_data <- NULL
#'     for (i in seq(along = data$soilid))
#'     {
#'         soil_no <- gsub('(^.*)(No\\d+)(\\)$)', '\\2', data$soilid[i])
#'         soil_c <- XML::xpathSApply(apsoil, '//Soil', function(x) 
#'             {
#'                 atts <- XML::xmlAttrs(x)
#'                 if (length(grep(paste('.*', soil_no, '.*', sep = ''), atts['name'])) > 0)
#'                 {
#'                     return(x)
#'                 } else
#'                 {
#'                     return(NULL)
#'                 }
#'             })
#'         pos <- !as.numeric(lapply(soil_c, is.null))
#'         if (sum(pos) == 0)
#'         {
#'             stop(paste('Cannot find soil "', data$soilid[i], '"', sep = ''))
#'         }
#'         if (sum(pos) > 1)
#'         {
#'             stop(paste('Cannot find soil "', data$name[i], '"', sep = ''))
#'         }
#'         soil_c <- soil_c[[which.max(pos)]]
#'         
#'         
#'         # Parse all soil data
#'         ParseSoil <- function(xml)
#'         {
#'             soil_data <- NULL
#'             layer_data <- NULL
#'             children <- XML::xmlChildren(xml)
#'             
#'             for (i in seq(along = children))
#'             {
#'                 children_i <- XML::xmlChildren(children[[i]])
#'                 if (length(children_i) > 1)
#'                 {
#'                     parseLayer <- function(layer)
#'                     {
#'                         if (!('Thickness' %in% names(layer)))
#'                         {
#'                             stop('NA')
#'                         }
#'                         pos <- grep('Metadata|SoilCrop', names(layer))
#'                         if (length(pos) > 0)
#'                         {
#'                             layer <- layer[-pos]
#'                         }
#'                         layer <- lapply(layer, function(x)
#'                             {
#'                                 unlist(XML::xmlApply(x, XML::xmlValue))
#'                             })
#'                         pos <- unlist(lapply(layer, length)) == 1
#'                         soil_d <- as.character(unlist(layer[pos]))
#'                         names(soil_d) <- names(layer)[pos]
#'                         layer_d <- lapply(layer[!pos], as.numeric)
#'                         layer_d <- as.data.frame(do.call(cbind, layer_d),
#'                             row.names = seq(sum(!pos)))
#'                         layer_d <- reshape2::melt(layer_d, id.vars = 'Thickness', 
#'                             variable.name = 'Trait',
#'                             value.name = 'Value')
#'                         list(layer_d = layer_d, soil_d = soil_d)
#'                     }
#'                     data_i <- parseLayer(children_i)
#'                     soil_data <- c(soil_data, data_i$soil_d)
#'                     layer_data <- rbind(layer_data, data_i$layer_d)
#'                     children_i_sc <- children_i[grep('SoilCrop', names(children_i))]
#'                     for (j in seq(along = children_i_sc))
#'                     {
#'                         crop <- parseLayer(XML::xmlChildren(children_i_sc[[j]]))
#'                         crop$layer_d$Trait <- paste0(as.character(
#'                                 XML::xmlAttrs(children_i_sc[[j]], 'crop')), '_', 
#'                             crop$layer_d$Trait)
#'                         soil_data <- c(soil_data, crop$soil_d)
#'                         layer_data <- rbind(layer_data, crop$layer_d)
#'                     }
#'                 } else
#'                 {
#'                     soil_data[[names(children[i])]] <- XML::xmlValue(children[[i]])
#'                 }
#'             }
#'             return(list(soil_data = soil_data, layer_data = layer_data))
#'         }
#'         
#'         res <- ParseSoil(soil_c)
#'         c_soil_data <- res$soil_data
#'         c_soil_data <- as.data.frame(t(c_soil_data))
#'         c_layer_data <- res$layer_data
#'         c_layer_data$Soil <- data$soilid[i]
#'         c_layer_data$Thickness <- as.numeric(as.character(c_layer_data$Thickness))
#'         # Calculate depth
#'         traits <- unique(c_layer_data$Trait)
#'         for (j in seq(along = traits))
#'         {
#'             pos <- c_layer_data$Trait %in% traits[j]
#'             c_layer_data$Depth[pos] <- cumsum(c_layer_data$Thickness[pos])
#'         }
#'         soil_data <- rbind(soil_data, c_soil_data)
#'         layer_data <- rbind(layer_data, c_layer_data)
#'     }
#'     names(soil_data) <- tolower(names(soil_data))
#'     return(list(soil_data = soil_data, layer_data = layer_data))
#' }
