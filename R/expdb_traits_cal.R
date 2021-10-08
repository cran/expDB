# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   08:10 PM Thursday, 23 July 2015
# * Copyright: AS IS

# Functions to get values of trait

# Get the unique columns for a treatment
# @param con a connection object as produced by dbConnect
# @param plot_id id for plot
dbGetTreatmentColumns <- function(con, plot_id) {
    sql <- paste0('SELECT name FROM expdb_trial_design_extra ',
                  'WHERE plot_id in (',
                  paste(unique(plot_id), collapse = ', '),
                  ')')
    design_extra <- DBI::dbGetQuery(con, sql)
    unique(c('year', 'site', 'trial', 'genotype',
             'density', 'row_spacing',
             'replicate', 'treatment',
             'block', unique(design_extra$name)))
}


#' Get the final leaf number
#'
#' The final leaf number is first retrived from trait
#' "O_FinalLeafNumber", then calculated from trait
#' "O_HaunIndex" if "O_FinalLeafNumber" is not observed.
#' Final leaf number equals the maximum value of "O_HaunIndex", which
#' should be an integer.
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' @param ... Arguments to specific trials.
#' @return A data.frame for selected final leaf number
#' @export
dbGetOrganFinalLeafNumber <- function(con, trials = NULL, ...) {
    
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }

    fln <- dbGetPhenotype_(con, trials = trials,
                           traits = 'O_FinalLeafNumber')
    trials_fln <- trials %>%
        dplyr::semi_join(fln, by = intersect(names(trials), names(fln)))

    trt_cols <- dbGetTreatmentColumns(con, trials_fln$plot_id)
    fln_avg <- fln %>%
        dplyr::group_by_(.dots = c(trt_cols, 'node')) %>%
      dplyr::summarise(std = stats::sd(.data$value),
                  value = mean(.data$value), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::mutate(traits = 'O_FinalLeafNumber',
               source = 'O_FinalLeafNumber')
    trials_no_fln <- trials %>%
      dplyr::anti_join(fln, by = intersect(names(trials), names(fln)))

    if (nrow(trials_no_fln) > 0) {
        trt_cols <- dbGetTreatmentColumns(con, trials$plot_id)
        haun_index <- dbGetPhenotype_(
            con,
            trials = trials,
            traits = 'O_HaunIndex')
        fln2 <- haun_index %>%
          dplyr::group_by_(.dots = c('column', 'row', 'site', 'year', 'sample', 'node')) %>%
          dplyr::filter(sum(.data$value == max(.data$value)) > 1) %>% 
            dplyr::filter(.data$value == max(.data$value),
                   .data$date == max(.data$date)) %>%
            # filter(value == round(value)) %>%
          dplyr::group_by_(.dots = c(trt_cols, 'node')) %>%
          dplyr::summarise(std = stats::sd(.data$value),
                      value = mean(.data$value), .groups = "drop") %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::mutate(traits = 'O_FinalLeafNumber',
                   source = 'O_HaunIndex')
        fln_avg <- dplyr::full_join(fln_avg, fln2, by = intersect(names(fln_avg), names(fln2)))
    }
    fln_avg
}






#' Get Haun Index
#'
#' The Haun Index is retrived from trait "O_HaunIndex", extending the final observationa
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' @param avg Whether to calculate the average value
#' @param ... Arguments to specific trials.
#' @return A data.frame for selected Haun Index
#' @export
dbGetOrganHaunIndex <- function(con, trials = NULL, avg = TRUE, ...) {
    
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }

    trt_cols <- c(dbGetTreatmentColumns(con, trials$plot_id), 'sowing', 'das', 'tt', 'node')
    trt_cols3 <- trt_cols[
        !(trt_cols %in% c('replicate', 'sample', 'treatment', 'block', 'das', 'tt'))]


    haun_index <- dbGetPhenotype_(
        con,
        trials = trials,
        traits = 'O_HaunIndex', tt = TRUE)
    hi_check_last_obs <- function(df) {
        # assign('df', df, .GlobalEnv)
        # stop()
        res <- df %>%
          tidyr::expand(.data$replicate, .data$sample) %>%
          dplyr::left_join(df, by = c('replicate', 'sample')) %>%
          dplyr::arrange(.data$replicate, .data$sample, .data$date) %>%
            tidyr::fill_(fill_cols = 'value')
        res
    }

    haun_index <- haun_index %>%
      dplyr::group_by_(.dots = trt_cols3) %>%
      dplyr::filter(date == max(.data$date)) %>%
      dplyr::do(hi_check_last_obs(.data)) %>%
      dplyr::filter(!is.na(.data$trial)) %>%
      dplyr::full_join(
            haun_index %>%
              dplyr::group_by_(.dots = trt_cols3) %>%
              dplyr::filter(.data$date < max(.data$date)) %>%
              dplyr::ungroup()
            , by = names(haun_index)
        )


    if (avg) {

        hi_avg <- haun_index %>%
          dplyr::group_by_(.dots = c(trt_cols3, 'date', 'das', 'tt')) %>%
          dplyr::summarise(
                sd = stats::sd(.data$value)
                , value = mean(.data$value)
            ) %>%
          dplyr::group_by_(.dots = trt_cols3)
        haun_index <-  hi_avg %>%
          dplyr::filter(.data$value > max(.data$value) * 0.95) %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::full_join(hi_avg %>% 
                             dplyr::filter(.data$value <= max(.data$value) * 0.95),
                      by = names(hi_avg))
    }

    haun_index
}





#' Estimation of plant populations
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected field population
#' @export
dbGetFieldPopulation <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    trt_cols <- dbGetTreatmentColumns(con, trials$plot_id)
      
    # Get observations
    obs <- dbGetPhenotype_(
        con, trials = trials,
        traits = 'F_Population',
        tt = FALSE, gene = FALSE) %>%
        dplyr::select(!dplyr::all_of('date'))
    population <- trials %>%
      dplyr::select(dplyr::all_of(c('column', 'row', 'site', 'year'))) %>%
      dplyr::left_join(obs, by = c('year', 'site', 'column', 'row')) %>%
      dplyr::filter(!is.na(.data$traits)) %>%
      dplyr::group_by_(.dots = trt_cols) %>%
      dplyr::summarise(std = stats::sd(.data$value),
                       value = mean(.data$value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(source = 'observation')
    # Find values from establishment count
    est_trial <- trials %>%
      dplyr::anti_join(
            population %>%
                dplyr::select(!dplyr::all_of(c('value', 'std'))),
            by = trt_cols)
    # Get observations
    est_count <- dbGetPhenotype_(
        con, trials = trials,
        traits = 'M_EstablishmentCount',
        tt = FALSE, gene = FALSE) %>%
      dplyr::mutate(traits = 'F_Population',
               value = .data$value / .data$row_spacing * 1000) %>%
      dplyr::group_by_(.dots = trt_cols) %>%
      dplyr::summarise(std = stats::sd(.data$value),
                       value = mean(.data$value)) %>%
      dplyr::ungroup() %>%
        dplyr::mutate(source = 'establishmentcount')

    # using density for other trials
    other_trials <- est_trial %>%
      dplyr::anti_join(
            est_count %>%
              dplyr::select(!dplyr::all_of(c('value', 'std'))),
            by = trt_cols) %>%
      dplyr::mutate(traits = 'F_Population',
               value = .data$density) %>%
      dplyr::group_by_(.dots = trt_cols) %>%
      dplyr::summarise(std = stats::sd(.data$value),
                       value = mean(.data$value)) %>%
      dplyr::ungroup()  %>%
      dplyr::mutate(source = 'density')
    dplyr::bind_rows(population, est_count, other_trials)

}




#' Estimation of flowering time
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected flowering time
#' @export
dbGetPlantFlowering <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetZadoksStage(con, trials, 65)
}

#' Estimation of heading time
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected heading time
#' @export
dbGetPlantHeading <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetZadoksStage(con, trials, 55)
}



#' Estimation of stem elongation
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected stem elongation stage
#' @export
dbGetPlantStemElongation <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }
    dbGetZadoksStage(con, trials, 31)
}

#' Estimation of maturity
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param ... Arguments pass to dbGetTrials
#' @return A data.frame for selected maturity time
#' @export
dbGetFieldMaturity <- function(con, trials = NULL, ...) {
    if (is.null(trials)) {
        trials <- dbGetTrials(con, ...)
    }

    # Average the zadoks score
    trt_cols <- dbGetTreatmentColumns(con, trials$plot_id)

    # Get observations
    obs <- dbGetPhenotype_(
        con, trials = trials,
        traits = 'F_HeadYellowing',
        tt = TRUE, gene = FALSE) %>%
      dplyr::group_by_(.dots = c(trt_cols, 'value')) %>%
      dplyr::summarise(
            date = as.Date(mean(.data$date)),
            tt = mean(.data$tt), .groups = "drop") %>%
      dplyr::ungroup()


    res <- obs %>%
      dplyr::count_(var = trt_cols) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$n > 1) %>%
      dplyr::left_join(obs, by = trt_cols) %>%
      dplyr::group_by_(.dots = trt_cols) %>%
      dplyr::summarize(
            date = as.Date(stats::approx(.data$value, .data$date, 95, rule = 2)$y,
                           origin = '1970-1-1'),
            tt = stats::approx(.data$value, .data$tt, 95, rule = 2)$y, .groups = "drop")


}





#' Obtain the key phenology stage
#'
#' @param con a connection object as produced by dbConnect
#' @param trials A data.frame to specify trials. If not NULL, other arguments
#' will be ignored.
#' @param key_stage The key zadoks stage
#' @return A data.frame for selected Zadoks stage
#' @export
dbGetZadoksStage <- function(con, trials, key_stage) {
    # Average the zadoks score
    trt_cols <- c(dbGetTreatmentColumns(con, trials$plot_id), 'sowing', 'column', 'row')

    # Get observations
    obs <- dbGetPhenotype_(
        con, trials = trials,
        traits = 'P_ZadoksScore',
        tt = TRUE, gene = FALSE) %>%
      dplyr::group_by(.dots = c(trt_cols, 'value')) %>%
      dplyr::summarise(
            date = as.Date(mean(.data$date)),
            tt = mean(.data$tt), .groups = "drop") %>%
      dplyr::ungroup()

    res <- list()
    for (i in seq(along = key_stage)) {
        # If key stage is recorded
        res1 <- obs %>%
          dplyr::group_by_(.dots = trt_cols) %>%
          dplyr::filter(abs(.data$value - key_stage[i]) < .Machine$double.eps) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(method = 'observed') %>% 
          dplyr::mutate(value = key_stage[i])

    
    
        # Estimation of heading time: Observations are around key stage
        approx_stage <- function(df) {
            tryCatch({
                date <- as.Date(stats::approx(df$value, df$date, key_stage[i])$y,
                        origin = '1970-1-1')
                tt <- stats::approx(df$value, df$tt, key_stage[i])$y
                res <- data.frame(date = date, tt = tt)
                return(res)
            }, error = function(e) {
                # assign('df', df, .GlobalEnv)
                # stop(e)
            })
        }
    
        phenotype2 <- obs %>%
          dplyr::group_by_(.dots = trt_cols) %>%
          dplyr::filter(!(abs(.data$value - key_stage[i]) < .Machine$double.eps)) %>%
          dplyr::filter(min(.data$value) < key_stage[i], max(.data$value) > key_stage[i]) 
        if (nrow(phenotype2) > 0) {
            res2 <- phenotype2 %>% dplyr::do(approx_stage(.data)) %>%
              dplyr::mutate(
                    value = key_stage[i]) %>%
              dplyr::ungroup() %>% 
              dplyr::mutate(method = 'approx')
            res12 <- dplyr::full_join(res1, res2, by = c(trt_cols, 'date', 'tt', 'value', 'method'))
            
        } else {
            res12 <- res1
        }
            
        
            
        # linear fit for out of range
        phenotype3 <- obs %>%
          dplyr::group_by(.dots = trt_cols) %>%
          dplyr::filter(!(abs(.data$value - key_stage[i]) < .Machine$double.eps)) %>%
          dplyr::filter(min(.data$value) > key_stage[i] | max(.data$value) < key_stage[i]) %>% 
          dplyr::filter(dplyr::min_rank(abs(.data$value - key_stage[i])) == 1) %>% 
          dplyr::left_join(trials, by = trt_cols) %>% 
          dplyr::anti_join(res12 %>% dplyr::select(trt_cols), by = trt_cols) %>% 
          dplyr::mutate(date = as.Date(round(as.numeric(.data$date)), origin = '1970-01-01'))
            
        if (nrow(phenotype3) > 0) {
            warning('The key stage is estimated by linear regression and may not be accurate')
            phenotype2_nom <- res12 %>% 
              dplyr::rename(key_state_tt = .data$tt) %>% 
              dplyr::select(!dplyr::all_of(c('value', 'date'))) %>% 
              dplyr::left_join(obs, by = trt_cols) %>% 
              dplyr::mutate(tt_nom = .data$tt - .data$key_state_tt) 
            lm_tt <- stats::lm(tt_nom ~ value, phenotype2_nom)
            
            # get the coefficient
            slope <- summary(lm_tt)$coef[2,1]
            intercept <- summary(lm_tt)$coef[1,1]
            
            # Filter 
            
            # Estimation of heading times for met file
            mets <- phenotype3 %>% 
                magrittr::use_series(met) %>% unique()
            res3 <- list()
            for (j in seq(along = mets))
            { # i <- 1
                met <- dbGetWeather(con,  mets[j]) %>%
                  dplyr::mutate(avgt = (.data$maxt + .data$mint) / 2,
                           tt = ifelse(.data$avgt > 0, .data$avgt, 0),
                           cum_tt = cumsum(.data$tt))
                phe_i <- phenotype3 %>% 
                  dplyr::filter(met %in% mets[j])
                
                tt_to_key_stage <- intercept + slope * phe_i$value
                
                # Calculate the cummulative thermal time at key stage
                key_stage_tt <- met$cum_tt[match(phe_i$date, met$date)] - tt_to_key_stage
      
                # Find the date of stage
                key_date <- unlist(lapply(
                    key_stage_tt, 
                    function(x) {
                        pos <- which.min(abs(x - met$cum_tt))
                        if (length(pos) == 0) return(NA)
                        met$date[pos]
                        }))
                key_date <- as.Date(key_date, origin = '1970-1-1')
                sowing_tt <- met$cum_tt[match(phe_i$sowing, met$date)]
                
                res3[[j]] <- phe_i %>% 
                  dplyr::ungroup() %>%
                  dplyr::select_(.dots = trt_cols) %>%
                  dplyr::mutate(date = key_date,
                           tt = key_stage_tt - sowing_tt, 
                           value = key_stage[i])
                
            }
            res3 <- dplyr::bind_rows(res3) %>% 
              dplyr::mutate(method = 'fitted')
            
            res[[i]] <- dplyr::full_join(res12, res3, by = c(trt_cols, 'date', 'tt', 'value', 'method'))
        } else {
            res[[i]] <- res12
        }
        
        
    }
    res <- dplyr::bind_rows(res)
    res$das <- as.numeric(res$date) - as.numeric(res$sowing)
    res
}

