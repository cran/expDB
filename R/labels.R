# # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# # * Created:   04:21 PM Monday, 28 April 2014
# # * Copyright: AS IS
# # *
# 
# 
# # Create labels with barcode
# #
# # @param designs A data.frame of design file, which has to include several columns
# # @param measure_date Date of measurement
# # @param site_idx Index of site
# # @param measure_traits trait list
# # @param sample_num Number of sample in each plot
# # @param measure_traits_map a trait map
# # @return A data.frame for generated barcodes
# # @export
# createBarcode <- function(
#     designs,
#     measure_date,
#     site_idx,
#     measure_traits,
#     sample_num = 1,
#     measure_traits_map = NULL) {
#     # Convert into small case to avoid problems
#     names(designs) <- tolower(names(designs))
#     if (is.null(measure_traits_map)) {
#         measure_traits_map <- utils::read.csv(system_file('data/dbmeasuretraits.csv', package = 'expDB'), as.is = TRUE)
#     }
#     names(measure_traits_map) <- tolower(names(measure_traits_map))
#     measure_trait_idx <- measure_traits_map$measurementindex[
#         match(measure_traits, measure_traits_map$name)]
# 
#     bc_plot <- designs %>%
#         dplyr::mutate(Barcode = paste0(
#             format(.data$measure_date, '%y%m%d'),
#             .data$site_idx,
#             formatC(.data$row, width = 2, flag = '0'),
#             formatC(.data$column, width = 2, flag = '0')
#         ))
# 
#     bc <- expand.grid(
#         Barcode = bc_plot$Barcode,
#         measure_trait = measure_trait_idx,
#         sample = seq(length = sample_num),
#         stringsAsFactors = FALSE
#     ) %>%
#         dplyr::left_join(bc_plot, by = 'Barcode') %>%
#         dplyr::left_join(measure_traits_map,
#                   by = c('measure_trait' = 'measurementindex')) %>%
#         dplyr::mutate(
#             Barcode = paste0(
#                 .data$Barcode,
#                 formatC(.data$measure_trait, width = 2, flag = '0'),
#                 .data$sample)) %>%
#         dplyr::rename(MeasureTrait = .data$name) %>%
#         dplyr::mutate(MeasureDate = .data$measure_date,
#                Researcher = .data$researcher) %>%
#         dplyr::arrange(.data$column, .data$row)
#     bc
# }
# 
# 
# 
# # Generate labels for avery products
# #
# # @param labels A character vector will be used to generate labels
# # @param file the file name to export
# # @param product The product ID of Avery
# # @return No return values. Generated PDF file for all labels.
# # @export
# averyLabel <- function(
#     labels, file,
#     product = 'L7163')
# {
#     names(labels) <- tolower(names(labels))
#     # Total number of labels
#     num_label <- nrow(labels)
#     # Check the labels
#     if (num_label == 0)
#     {
#         stop('NO label specified')
#     }
#     # Read teh AveryDB
#     averydb <- utils::read.csv(system_file('data/dbavery.csv',
#         package = 'expDB'), as.is = TRUE)
# 
#     # Check the product ID
#     if (!(product %in% averydb$ID))
#     {
#         stop(paste0('Product ID ', product, ' don\'t support.'))
#     }
#     # Get the design
#     a_design <- averydb[averydb$ID == product,]
# 
#     # Define the page size (A4)
#     page_height <- 297
#     page_width <- 210
# 
#     # Calculate label width and height
#     a_design$LabelHeight <- (page_height - a_design$TopBorder -
#         a_design$BottomBorder -
#         a_design$MarginRow * (a_design$LabelRows - 1)) /
#         a_design$LabelRows
#     a_design$LabelWidth <- (page_width - a_design$LeftBorder -
#         a_design$RightBorder -
#         a_design$MarginCol * (a_design$LabelCols - 1)) /
#         a_design$LabelCols
# 
#     # load the library
#     # page_border <- 0.25 * 25.4
#     # Printed labels
#     num_printed <- 1
# 
#     # Create all labels
#     # Calculate total pages
#     pages <- ceiling(num_label / (a_design$LabelRows * a_design$LabelCols))
#     grDevices::pdf(file, width = page_width / 25.4, height = page_height / 25.4)
#     oldpar <- par(no.readonly = TRUE)    # code line i
#     on.exit(par(oldpar))            
#     op <- graphics::par(mar = rep(0, 4))
#     for (k in seq(length = pages))
#     {
#         grid::grid.newpage()
#         grid::grid.rect()
#         for (i in seq(length = a_design$LabelRows))
#         {
#             for (j in seq(length =  a_design$LabelCols))
#             {
#                 # Check if all labels is printed
#                 if (num_printed > num_label)
#                 {
#                     break
#                     return(NULL)
#                 }
# 
#                 # Calculate the viewport
#                 vp_x <- grid::unit(
#                     # Left border
#                     a_design$LeftBorder +
#                     # Page border
#                     # page_border +
#                     # Size for labels in the left
#                     a_design$LabelWidth * (j - 1) +
#                     # Middle of this label
#                     a_design$LabelWidth / 2 +
#                     # Margins between labels in the left
#                     a_design$MarginCol * (j - 1),
#                     'mm')
#                 vp_y <- grid::unit(
#                     # Print from the top
#                     page_height - (
#                     # Top border
#                     a_design$TopBorder +
#                     # Page border
# 
#                     # Size for labels in the top
#                     a_design$LabelHeight * (i - 1) +
#                     # Middle of this label
#                     a_design$LabelHeight / 2 +
#                     # Margins between labels in the top
#                     a_design$MarginRow * (i - 1)),
#                     'mm')
# 
#                 # Create viewport
#                 vp <- grid::viewport(vp_x, vp_y,
#                                      grid::unit(a_design$LabelWidth, 'mm'),
#                                      grid::unit(a_design$LabelHeight, 'mm'))
# 
#                 grid::pushViewport(vp)
#                 grid::grid.rect()
# 
# 
#                 # keep a 3 mm border
#                 # canvas
#                 canvas_width <- a_design$LabelWidth - 6
#                 canvas_height <- a_design$LabelHeight - 6
#                 vp <- grid::viewport(
#                     0.5, 0.5,
#                     grid::unit(canvas_width, 'mm'),
#                     grid::unit(canvas_height, 'mm'))
# 
#                 grid::pushViewport(vp)
#                 
#                 #reate label
#                 do.call(eval(parse(text = paste0('label_', product))),
#                         args = list(labels = labels, num_printed = num_printed,
# 						canvas_width = canvas_width, canvas_height = canvas_height))
#                 
#                 grid::popViewport()
#                 grid::popViewport()
#                 num_printed <- num_printed + 1
#             }
#         }
#     }
#     graphics::par(op)
#     grDevices::dev.off()
# }
# 
# label_L7651 <- function(labels, num_printed, canvas_width, canvas_height) {
#     bc_img <- zint_barcode(
#         labels$barcode[num_printed],
#         height = 100,
#         border = 0,
#         is_show_text = TRUE)
#     vp_img <- grid::viewport(
#         x = grid::unit(0.5, "npc"),
#         y = grid::unit(0.6, "npc"),
#         width = grid::unit(1, "npc"),
#         height = grid::unit(0.8, "npc")
#     )
#     grid::pushViewport(vp_img)
#     #grid.rect()
#     grid::grid.raster(bc_img[[1]], 
#                 width = grid::unit(1, 'npc'),
#                 height = grid::unit(1, 'npc'))
#     grid::popViewport()
#     vp_txt <- grid::viewport(
#         x = grid::unit(0.5, "npc"),
#         y = grid::unit(0.1, "npc"),
#         width = grid::unit(1, "npc"),
#         height = grid::unit(0.2, "npc")
#     )
#     grid::pushViewport(vp_txt)
#     grid::grid.text(label = labels$label[num_printed],
#               gp = grid::gpar(cex = 0.3))
#     grid::popViewport()
# }
# 
# label_L7163 <- function(labels, num_printed, canvas_width, canvas_height) {
#     
#     #grid.rect()
#     # Barcode
#     bc_img <- zint_barcode(
#         labels$barcode[num_printed],
#         height = 30,
#         border = 0)
#     vp_img <- grid::viewport(
#         grid::unit(0.25 * canvas_width, 'mm'),
#         grid::unit(3 * 1 / 4 * canvas_height, 'mm'),
#         grid::unit(1 / 2 * canvas_width, 'mm'),
#         grid::unit(1 / 2 * canvas_height, 'mm'))
#     grid::pushViewport(vp_img)
#     #grid.rect()
#     grid::grid.raster(bc_img[[1]])
#     grid::popViewport()
#     
#     # column
#     vp_row <- grid::viewport(
#         grid::unit(0.5 / 8 * canvas_width, 'mm'),
#         grid::unit((1 / 4) * canvas_height, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'))
#     grid::pushViewport(vp_row)
#     grid::grid.rect()
#     grid::grid.text(paste0('Col\n', labels$column[num_printed]))
#     grid::popViewport()
#     
#     
#     
#     # row
#     vp_col <- grid::viewport(
#         grid::unit(3 * 0.5 / 8 * canvas_width, 'mm'),
#         grid::unit((1 / 4) * canvas_height, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'))
#     grid::pushViewport(vp_col)
#     grid::grid.rect()
#     grid::grid.text(paste0('Row\n', labels$row[num_printed]))
#     grid::popViewport()
#     
#     
#     
#     # replicate
#     vp_rep <- grid::viewport(
#         grid::unit(5 * 0.5 / 8 * canvas_width, 'mm'),
#         grid::unit((1 / 4) * canvas_height, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'))
#     grid::pushViewport(vp_rep)
#     grid::grid.rect()
#     grid::grid.text(paste0('Rep\n', labels$replicate[num_printed]))
#     grid::popViewport()
#     
#     # Sample
#     vp_sam <- grid::viewport(
#         grid::unit(7 * 0.5 / 8 * canvas_width, 'mm'),
#         grid::unit((1 / 4) * canvas_height, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'),
#         grid::unit(0.5 / 4 * canvas_width, 'mm'))
#     grid::pushViewport(vp_sam)
#     grid::grid.rect()
#     grid::grid.text(paste0('Sam\n', labels$sample[num_printed]))
#     grid::popViewport()
#     
#     # Traits
#     vp_traits <- grid::viewport(
#         grid::unit(0.75 * canvas_width, 'mm'),
#         10 / 12,
#         0.5,
#         2 / 6)
#     grid::pushViewport(vp_traits)
#     #grid.rect()
#     grid::grid.text(labels$measuretrait[num_printed],
#               gp = grid::gpar(cex = 1.1))
#     grid::popViewport()
#     
#     # TrialCode
#     vp_trialcode <- grid::viewport(
#         grid::unit(0.75 * canvas_width, 'mm'),
#         7 / 12,
#         0.5,
#         1 / 6)
#     grid::pushViewport(vp_trialcode)
#     #grid.rect()
#     grid::grid.text(labels$trial[num_printed])
#     grid::popViewport()
#     
#     
#     # Genotype
#     vp_genotype <- grid::viewport(
#         grid::unit(0.75 * canvas_width, 'mm'),
#         5 / 12,
#         0.5,
#         1 / 6)
#     grid::pushViewport(vp_genotype)
#     #grid.rect()
#     grid::grid.text(labels$genotype[num_printed])
#     grid::popViewport()
#     
#     # Site
#     vp_site <- grid::viewport(
#         grid::unit(0.75 * canvas_width, 'mm'),
#         3 / 12,
#         0.5,
#         1 / 6)
#     grid::pushViewport(vp_site)
#     #grid.rect()
#     grid::grid.text(labels$site[num_printed])
#     grid::popViewport()
#     
#     # Researcher and date
#     vp_res_date <- grid::viewport(
#         grid::unit(0.75 * canvas_width, 'mm'),
#         1 / 12,
#         0.5,
#         1 / 6)
#     grid::pushViewport(vp_res_date)
#     #grid.rect()
#     grid::grid.text(paste0(
#         'by ',
#         labels$researcher[num_printed],
#         ' on ',
#         labels$measuredate[num_printed]),
#         x = 0.9,
#         gp = grid::gpar(cex = 0.5),
#         hjust = 1)
#     grid::popViewport()
# }
