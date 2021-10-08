# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:51 AM Monday, 17 September 2012
# * Copyright: AS IS
# *

# Export expDB data

#' Export trials weather records to met file
#' @param con a connection object as produced by dbConnect
#' @param output The folder of output files
#' @param na The character for missing value with default NA
#' @param ... All other arguments to define range of export trials.
#' All trials will be export if there are not arguments.
#' Supported arguments include trial (or trialcode)
#' @return Write weather records into files. No return values.
#' @export
dbExportMet <- function(con, output, na = NA, ...)
{
    trials <- dbGetTrials(con, design = FALSE, ...)
    # Create the met file
    met_files <- unique(trials$met)

    for (i in seq_along(met_files))
    {
        records <- dbGetWeather(con, met_files[i], format = 'weaana', na = na)
        weaana::writeWeatherRecords(records, file = file.path(output,
                                                      sprintf('%s.met', met_files[i])))
    }
    return(trials)
}

#' apsimReplacement <- function(file) {
#'    
#'     
#'     file %>% 
#'         readLines() %>% 
#'         paste(collapse = '\n') %>% 
#'         XML::xmlParseString()
#' }
#' 
#' apsimSoil <- function(con, soil_id) {
#'     sql <- paste0('SELECT * FROM `expdb_apsoil` WHERE id = ', soil_id)
#'     soil <- DBI::dbGetQuery(con, sql)
#'     apsoil <- 'Resources/apsoil.xml'
#'     soil_no <- gsub('(^.*)(No\\d+)(\\)$)', '\\2', soil$soilid)
#'     
#'     apsoil %>% 
#'         readLines() %>% 
#'         paste(collapse = '\n') %>% 
#'         XML::xmlParseString()
#' }
#' 
#' 
#' apsimMicroClimate <- function() {
#'     XML::xmlParseString('<MicroClimate>
#'     <Name>MicroClimate</Name>
#'     <air_pressure>1010</air_pressure>
#'     <soil_emissivity>0.96</soil_emissivity>
#'     <sun_angle>15</sun_angle>
#'     <soil_heat_flux_fraction>0.4</soil_heat_flux_fraction>
#'     <night_interception_fraction>0.5</night_interception_fraction>
#'     <windspeed_default>3</windspeed_default>
#'     <refheight>2</refheight>
#'     <albedo>0.15</albedo>
#'     <emissivity>0.96</emissivity>
#'     <gsmax>0.01</gsmax>
#'     <r50>200</r50>
#'     <a_interception>0</a_interception>
#'     <b_interception>1</b_interception>
#'     <c_interception>0</c_interception>
#'     <d_interception>0</d_interception>
#'     <soil_albedo>0.3</soil_albedo>
#'     </MicroClimate>')
#' }
#' 
#' apsimSurfaceOrganicMatter <- function() {
#'     
#'     som_xml <- '
#' <SurfaceOrganicMatter>
#'           <Name>SurfaceOrganicMatter</Name>
#'     <ResidueTypes>
#'     <Name>ResidueTypes</Name>
#'     <LoadFromResource>ResidueTypes</LoadFromResource>
#'     </ResidueTypes>
#'     <Pools>
#'     <Pool>
#'     <PoolName>wheat_stubble</PoolName>
#'     <ResidueType>Wheat</ResidueType>
#'     <Mass>0</Mass>
#'     <CNRatio>80</CNRatio>
#'     <CPRatio>0</CPRatio>
#'     <StandingFraction>0</StandingFraction>
#'     </Pool>
#'     </Pools>
#'     <PoolName>wheat_stubble</PoolName>
#'     <type>Wheat</type>
#'     <mass>0</mass>
#'     <standing_fraction>0</standing_fraction>
#'     <cpr>0</cpr>
#'     <cnr>80</cnr>
#'     <CriticalResidueWeight>2000</CriticalResidueWeight>
#'     <OptimumDecompTemp>20</OptimumDecompTemp>
#'     <MaxCumulativeEOS>20</MaxCumulativeEOS>
#'     <CNRatioDecompCoeff>0.277</CNRatioDecompCoeff>
#'     <CNRatioDecompThreshold>25</CNRatioDecompThreshold>
#'     <TotalLeachRain>25</TotalLeachRain>
#'     <MinRainToLeach>10</MinRainToLeach>
#'     <CriticalMinimumOrganicC>0.004</CriticalMinimumOrganicC>
#'     <DefaultCPRatio>0</DefaultCPRatio>
#'     <DefaultStandingFraction>0</DefaultStandingFraction>
#'     <StandingExtinctCoeff>0.5</StandingExtinctCoeff>
#'     <FractionFaecesAdded>0.5</FractionFaecesAdded>
#' </SurfaceOrganicMatter>'
#'     xmlParseString(som_xml)
#' }
#' 
#' #' Export trials to APSIMX file
#' #'
#' #' @param con a connection object as produced by dbConnect
#' #' @param trials A data.frame for trials
#' #' @param output The folder of output files
#' #' @param met_folder Folder to store met files
#' #' All trials will be export if there are not arguments.
#' #' Supported arguments include trial (or trialcode)
#' #' @export
#' dbExportAPSIMX <- function(con, trials, output, met_folder = 'mets')
#' {
#'     library(expDB)
#'     # Check trials
#'     if (nrow(trials) == 0)
#'     {
#'         stop('No trials are selected.')
#'     }
#'     trials <- trials %>%
#'         mutate(
#'             croptype = 'Wheat',
#'             plant = 'wheat')
#' 
#'     trials <- trials %>%
#'         mutate(
#'             sowing_date = as.POSIXct(format(sowing, '%Y-%m-%d')),
#'             sowing_str = format(sowing_date, format = '%Y-%m-%dT%H:%M:%S'),
#'             depth = ifelse(is.na(depth), 50, depth),
#'             row_spacing = ifelse(is.na(row_spacing), 250, row_spacing),
#'             density = ifelse(is.na(density), 120, density),
#'             sow_action = paste0(
#'                 '[', plant, ']', '.Sow(cultivar:"', genotype,
#'                 '", population:', density, ', depth:', depth,
#'                 ', rowSpacing:', row_spacing, ');')
#'         )
#'     library(XML)
#' 
#'     # Create simulation only for each trial. All genotypes and other
#'     # management will be treated as factors
#'     unique_trials <- trials %>%
#'         select(trial, sowing_date, croptype, plant, met) %>%
#'         distinct() %>%
#'         mutate(
#'             metfile = paste0(met_folder, met, '.met'),
#'             start = format(sowing_date - 2 * 24 * 60 * 60,
#'                            format = '%Y-%m-%dT%H:%M:%S'),
#'             end = format(min(sowing_date + 210 * 24 * 60 * 60, Sys.time() - 2 * 24 * 60 * 60),
#'                          format = '%Y-%m-%dT%H:%M:%S')
#'         )
#' 
#'     # Create doc and "Simulations" Node
#'     sims_list <- list()
#'     # Generate simulation
#'     for (i in seq(length = nrow(unique_trials))) {
#'         # Generate factors
#'         trials_i <- unique_trials %>%
#'             slice(i) %>%
#'             left_join(trials,
#'                       by = c("trial", "sowing_date",
#'                              "croptype", "plant", "met")) %>% 
#'             distinct()
#'         sow_operations <- lapply(seq(length = nrow(trials_i)),
#'                              function(x) {
#'                                  list(
#'                                      Name = trials_i$genotype[x],
#'                                      Operation = c(
#'                                          Date = trials_i$sowing_str[x],
#'                                          Action = trials_i$sow_action[x])
#'                                  )
#'                                  })
#'         names(sow_operations) <- rep('Operations', length(sow_operations))
#'         Factors <- list(
#'             Factor = c(list(
#'                 Name = 'Cultivar'),
#'                 sow_operations,
#'                 Specifications = '[Operations]'))
#' 
#'         # Generate simulation node
#'         sim_name <- unique_trials$trial[i]
#'         sims_list[[i]] <- list(
#'             Name = sim_name,
#'             Factors = Factors,
#'             Simulation = list(
#'                 Weather = c(
#'                     FileName = unique_trials$metfile[i]),
#'                 Clock = c(
#'                     StartDate = unique_trials$start[i],
#'                     EndDate = unique_trials$end[i]),
#'                 Summary = c(),
#'                 SoilArbitrator = c(),
#'                 Zone = list(
#'                     Name = 'Field',
#'                     Irrigation = c(),
#'                     Fertiliser = c(),
#'                     Soil = apsimSoil(con, trials_i$soil_id[1]),
#'                     SurfaceOrganicMatter = apsimSurfaceOrganicMatter(),
#'                     Plant = c(
#'                         Name = unique_trials$plant[i],
#'                         ResourceName = unique_trials$plant[i],
#'                         CropType = unique_trials$croptype[i]),
#'                     Operations = list(
#'                         Operation = c(Date = trials_i$sowing_str[1],
#'                                       Action = trials_i$sow_action[1])
#'                     ),
#'                     MicroClimate = apsimMicroClimate(),
#'                     Report = list(
#'                         VariableNames = c('[Clock].Today',
#'                                           'Wheat.Phenology.CurrentPhaseName',
#'                                           'Wheat.Phenology.Stage'),
#'                         EventNames = c('[Clock].DoReport'))
#'                 )))
#'     }
#'     names(sims_list) <- rep('Experiment', length(sims_list))
#'     doc <- newXMLDoc()
#'     sims <- newXMLNode(
#'         'Simulations',
#'         newXMLNode('Name', 'Simulations'),
#'         Replacements = apsimReplacement('Resources/wheat.xml'),
#'         newXMLNode(
#'             'DataStore',
#'             newXMLNode('Name', 'DataStore'),
#'             newXMLNode('AutoExport', 'true')),
#'         namespaceDefinitions =
#'             c('xsi' = "http://www.w3.org/2001/XMLSchema-instance"),
#'         parent = doc)
#'     listToAPSIMX(sims, sims_list)
#'     text <- as(doc, "character")
#'     writeLines(text, output)
#' 
#'     # Export met
#'     library(weaana)
#'     for (i in seq(length = nrow(unique_trials)))
#'     {
#'         records <- dbGetWeather(con, unique_trials$met[i],
#'                                 format = 'weaana')
#'         writeWeatherRecords(records,
#'                             file = file.path(dirname(output), unique_trials$metfile[i]))
#'     }
#' }
#' 
#' 
#' 
#' #' Convert list to APSIMX file
#' #'
#' #' @param node node
#' #' @param sublist list
#' listToAPSIMX <- function(node, sublist){
#'     for(i in seq(length = length(sublist))){
#'         if (names(sublist)[i] != "Name") {
#'             child <- newXMLNode(
#'                 names(sublist)[i],
#'                 parent = node)
#'         }
#'         if (typeof(sublist[[i]]) == "list"){
#'             node_name <- ifelse(
#'                 is.null(sublist[[i]]$Name),
#'                 names(sublist)[i],
#'                 sublist[[i]]$Name)
#'             newXMLNode('Name', node_name, parent = child)
#'             
#'             
#'             listToAPSIMX(child, sublist[[i]])
#'             
#'         } else {
#'             if ('XMLInternalNode' %in% class(sublist[[i]])) {
#'                 addChildren(child, kids = xmlChildren(sublist[[i]]))
#'                 next
#'             }
#'             if (names(sublist)[i] == 'Name') {
#'                 next
#'             }
#'             node_names <- names(sublist[[i]])
#'             if (length(sublist[[i]]) == 0) {
#'                 newXMLNode('Name', names(sublist)[i], parent = child)
#'             } else if (is.null(node_names) & xmlName(child) %in% c('VariableNames', 'EventNames', 'Specifications')){
#'                 print(xmlName(child))
#'                 node_names <- rep('string', length = length(sublist[[i]]))
#'             } else if (is.null(node_names)) {
#'                 print(xmlName(child))
#'                 next
#' 
#'             } else if ("Name" %in% node_names){
#'             } else if (names(sublist)[i] %in% c('Operation', 'Area', 'Slope')) {
#'             } else {
#'                 newXMLNode('Name', names(sublist)[i], parent = child)
#'             }
#'             for (j in seq(length = length(sublist[[i]]))) {
#'                 newXMLNode(node_names[j], sublist[[i]][j], parent = child)
#'             }
#'         }
#'     }
#' }
