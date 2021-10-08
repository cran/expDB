# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:02 PM Monday, 05 June 2017
# * Copyright: AS IS


#' Estimation of stem number per unit area
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected stem number
#' @export
dbGetFieldStemNumber <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetStemTillerNumber(
        con,
        trials,
        c('F_StemNumber', 'F_TillerNumber',
          'P_StemNumber', 'P_TillerNumber'))
}

#' Estimation of tiller number per unit area
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected tiller number
#' @export
dbGetFieldTillerNumber <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetStemTillerNumber(
        con,
        trials,
        c('F_TillerNumber', 'F_StemNumber',
          'P_TillerNumber', 'P_StemNumber'))
}

#' Estimation of stem number per plant
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected stem number for individual plant
#' @export
dbGetPlantStemNumber <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetStemTillerNumber(
        con,
        trials,
        c('P_StemNumber', 'P_TillerNumber',
          'F_StemNumber', 'F_TillerNumber')
    )
}

#' Estimation of tiller number per plant
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected tiller number for individual plant
#' @export
dbGetPlantTillerNumber <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetStemTillerNumber(
        con,
        trials,
        c('P_TillerNumber', 'P_StemNumber',
          'F_TillerNumber', 'F_StemNumber')
    )
}

# Estimation of stem/tiller number per unit area or plant
#
# @param con a connection object as produced by dbConnect
# @param trials A data.frame to specify trials
# @param traits_order A list of traits for stem and tiller numbers.
dbGetStemTillerNumber <- function(
    con, trials,
    traits_order) {
    
    trt_cols <- dbGetTreatmentColumns(con, trials$plot_id)
    
    # Find the population
    population <- dbGetFieldPopulation(con, trials = trials) %>%
        dplyr::select_(.dots = c(trt_cols, 'value')) %>%
        dplyr::rename(population = .data$value)
    
    # Get observations for all traits
    obs <- dbGetPhenotype_(
        con, trials = trials,
        traits = traits_order,
        tt = FALSE, gene = FALSE) %>%
        # join with population
        dplyr::left_join(population, by = trt_cols) %>%
        # convert into P_StemNumber
        dplyr::mutate(value = ifelse(
            .data$traits == 'F_StemNumber', .data$value / .data$population,
            ifelse(.data$traits == 'F_TillerNumber', (.data$value + .data$population) / .data$population,
                   ifelse(.data$traits == 'P_TillerNumber', .data$value + 1, .data$value))
        )) %>%
        # calculate the target
        dplyr::mutate(
            target_trt = traits_order[1],
            value = ifelse(
                .data$target_trt == 'F_StemNumber', .data$value * .data$population,
                ifelse(.data$target_trt == 'F_TillerNumber',
                       (.data$value - 1) * .data$population,
                       ifelse(.data$target_trt == 'P_TillerNumber',
                              .data$value - 1, .data$value))
            )) %>%
        dplyr::select(!dplyr::all_of('target_trt')) %>%
        # Calculate the average
        dplyr::mutate(traits = factor(.data$traits, levels = traits_order)) %>%
        dplyr::arrange(.data$traits) %>%
        dplyr::group_by_(.dots = c(trt_cols, 'date', 'traits')) %>%
        dplyr::summarise(std = stats::sd(.data$value),
                  value = mean(.data$value)) %>%
        dplyr::mutate(source = as.character(.data$traits)) %>%
        dplyr::ungroup() %>%
        # Convert traits and only keep the first one
        dplyr::group_by_(.dots = c(trt_cols, 'date')) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(traits = traits_order[1])
    obs
}


#' Get the dry weight per stem
#' 
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' 
#' @return A data.frame for selected dry weight per stem
#' @export
dbGetDryWeightPerStem <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    trials <- dbGetTrials(con)
    
    # Average the zadoks score
    trt_cols <- c(dbGetTreatmentColumns(con, trials$plot_id), 'date', 'das', 'tt')
    
    
    # if O_DryWeightPerStem is recorded
    # Get observations
    obs <- dbGetPhenotype_(con, trials = trials,
        traits = 'O_DryWeightPerStem',
        tt = TRUE, gene = FALSE) %>%
        dplyr::group_by_(.dots = trt_cols) %>%
        dplyr::summarise(value = mean(.data$value)) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(source = 'observation')
    # if O_DryWeightPerStem is recorded, then calculated from dry weight above ground and stem number
    trials2 <- trials %>%
        dplyr::anti_join(obs, by = intersect(names(trials), names(obs)))
    dry_weight <- dbGetPhenotype_(con, trials = trials2,
        traits = 'F_DryWeightAboveGround',
        tt = TRUE, gene = FALSE) %>%
        dplyr::group_by_(.dots = trt_cols) %>%
        dplyr::summarise(dw = mean(.data$value))
    
    stem_number <- dbGetFieldStemNumber(con, trials2) %>% 
        dplyr::rename(sn = .data$value) %>% 
        dplyr::select(!dplyr::all_of(c('std', 'traits', 'source'))) 
    obs2 <- stem_number %>% 
        dplyr::left_join(dry_weight, by = intersect(names(stem_number), names(dry_weight))) %>% 
        dplyr::mutate(value = .data$dw / .data$sn) %>% 
        dplyr::select(!dplyr::all_of(c('sn', 'dw'))) %>% 
        dplyr::mutate(source = 'estimation')
    dplyr::full_join(obs, obs2, by = intersect(names(obs), names(obs2)))
}
