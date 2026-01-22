# To review pull request for productivity_anomaly:
# 1. Run chunk 'set paths'
# 2. Run chunk 'run workflow'
# 3. Run chunk 'plotting functions'
## These functions are from ecodata::plot_productivity_anomaly.
## Changes: 
##  a. replaced ecodata::productivity_anomaly with workflow generated
##  b. added "_workflow" to plot titles
# 4. Run chunk 'generate comparison plots'
## I left some comments with things I saw
# 5. Visually compare to ecodata version of plots
## Figure 36 in 2024 reports


# 1. set paths --------

input_survey_bio_epu <- "~/EDAB_Datasets/Workflows/surveyBiologicalByEPUData.rds"
input_survey_bio <- "~/EDAB_Datasets/Workflows/surveyBiologicalData.rds"
input_static_lw_table <- "~/EDAB_Datasets/Workflows/df_lw.rda"
inputPathSpecies <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath <- "~/EDAB_Indicators/"
input_static_length_convert <- "~/EDAB_Datasets/Workflows/df_lconv.rda"

# 2. run workflow ------------------

# run time ~1 min

source(here::here("data-raw/workflow_productivity_anomaly.R"))

test_productivity_anomaly <- workflow_productivity_anomaly(
  input_survey_bio_epu = input_survey_bio_epu,
  input_survey_bio = input_survey_bio,
  input_static_lw_table = input_static_lw_table,
  inputPathSpecies = inputPathSpecies,
  input_static_length_convert = input_static_length_convert,
  outputPath = outputPath
)

# 3. plotting functions ---------

# filter for species plotted in SOE reports

# test_productivity_anomaly <- test_productivity_anomaly |> 
#   dplyr::filter(Var %in% c("Acadian redfish -rs-Assessment","American plaice -rs-Assessment",
#                            "Atlantic wolffish -rs-Assessment", "Haddock -rs-Assessment",
#                            "Pollock -rs-Assessment", "White hake -rs-Assessment",
#                            "Winter flounder -rs-Assessment", "Yellowtail flounder -rs-Assessment"  
#                            ))
                                

# using plot_productivity_anomaly function from ecodata
# replacing ecodata productivity_anomaly with the indicator generated above

plot_productivity_anomaly <- function(shadedRegion = NULL,
                                      report="MidAtlantic",
                                      plottype = "region",
                                      varName = "anomaly",
                                      EPU = NULL) {
  
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
                               report=report)
  
  # this should be added to plot_setip
  leg_font_size <- 6
  
  # base data object
  prod_dat <- test_productivity_anomaly
  
  # council-level filtering
  if (plottype == "council") {
    
    if (report == "MidAtlantic") {
      prod_dat <- prod_dat |>
        dplyr::filter(Jurisdiction %in% c("MAFMC", "JOINT"))
      
    } else if (report == "NewEngland") {
      prod_dat <- prod_dat |>
        dplyr::filter(Jurisdiction %in% c("NEFMC", "JOINT"))
      
    } else {
      stop("Invalid report value")
    }
  }
  
  
  # determine EPU filtering logic
  filterEPUs <- NULL
  
  if (varName == "anomaly") {
    
    if (plottype == "region") {
      
      if (report == "MidAtlantic") {
        filterEPUs <- "MAB"
        
      } else if (report == "NewEngland") {
        
        if (!(EPU %in% c("GB","GOM"))) {
          stop("For NewEngland the epu must be either 'GB' or 'GOM'")
        }
        
        filterEPUs <- EPU
      }
      
    } else if (plottype == "council") {
      
      # council-level anomaly plots should use EPU == "All"
      filterEPUs <- "All"
    }
  }
  
  
  
  
  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  
  if (varName == "assessment") {
    fix <- prod_dat |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-") |>
      dplyr::filter(Var == "rs_anom") |>
      dplyr::group_by(Time) |>
      dplyr::summarise(
        Total = sum(Value, na.rm = TRUE),
        Count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Total = ifelse(Count < max(Count), NA, Total)
      ) |>
      tidyr::complete(
        Time = seq(min(Time), max(Time), by = 1),
        fill = list(Total = NA)
      )
    
    prod <- prod_dat |>
      tidyr::separate(Var, into = c("Stock", "Var"), sep = "-") |>
      dplyr::filter(Var == "rs_anom") |>
      dplyr::mutate(Stock = toupper(Stock))
    
    
    # code for generating plot object p
    # ensure that setup list objects are called as setup$...
    # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
    # xmin = setup$x.shade.min , xmax = setup$x.shade.max
    #
    p <-
      ggplot2::ggplot(prod, ggplot2::aes(x = Time)) +
      ggplot2::geom_bar(data = prod |> dplyr::filter(Value > 0),
                        ggplot2::aes(y = Value, fill = Stock),
                        stat = "identity") +
      ggplot2::geom_bar(data = prod |> dplyr::filter(Value < 0),
                        ggplot2::aes(y = Value, fill = Stock),
                        stat = "identity") +
      ggplot2::geom_line(data = fix, ggplot2::aes(x = Time, y = Total),
                         linewidth = 1) +
      ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
      ggplot2::xlab("") +
      ggplot2::ylab("Recruitment Anomaly") +
      ggplot2::ggtitle(" Recruitment Anomaly from Stock Assessments") +
      #ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
      ecodata::theme_ts()+
      ggplot2::theme(axis.title   = ggplot2::element_text(size = 10),
                     axis.text    = ggplot2::element_text(size = 10),
                     plot.title   = ggplot2::element_text(size = 12),
                     #legend.text  = element_text(size = leg_font_size),
                     legend.title = ggplot2::element_blank(),
                     legend.text=ggplot2::element_text(size=6))
    
  }
  
  if (varName == "anomaly") {
    
    bar_dat <- prod_dat |>
      dplyr::filter(grepl('_Survey',Var))
    
    if (!is.null(filterEPUs)) {
      bar_dat <- bar_dat |>
        dplyr::filter(EPU == filterEPUs)
    }
    
    bar_dat <- bar_dat |>
      tidyr::separate(Var, into = c("Var", "Survey"), sep = "_") |>
      dplyr::mutate(
        Var = gsub("^NM LME\\s+", "", Var)
      )
    
    
    p <- plot_stackbarcpts_single(
      YEAR = bar_dat$Time,
      var2bar = bar_dat$Var,
      x = bar_dat$Value,
      titl = if (plottype == "council") {
        paste0(report, " council from survey data")
      } else {
        paste0(EPU, " from survey data")
      },
      xlab = "",
      ylab = "Small fish per large fish biomass (anomaly)",
      height = 5.5,
      width = 9,
      filt = FALSE,
      leg_font_size = leg_font_size,
      label = "",
      y.text = 10,
      aggregate = TRUE
    )
  }
  
  
  if (varName == "assessment" && !is.null(EPU) && EPU == "GOM") {
    p <- "Assessment variable includes GB and GOM. See plot for EPU = GB."
  }
  
  return(p)
  
}

attr(plot_productivity_anomaly,"report") <- c("MidAtlantic","NewEngland")
attr(plot_productivity_anomaly,"varName") <- c("anomaly","assessment")
attr(plot_productivity_anomaly,"EPU") <- c(NULL,"MAB","GB","GOM")
attr(plot_productivity_anomaly,"plottype") <- c("region","council")



#' anomaly stacked barchart. needs to be reworked
#' @noRd
plot_stackbarcpts_single <- function(YEAR, var2bar,
                                     x, xlab, ylab,
                                     titl,
                                     file_suffix,
                                     leg_font_size = leg_font_size,
                                     remove_leg = FALSE,
                                     leg_ncol = 1,
                                     wcpts = TRUE,
                                     wdashed = TRUE,
                                     height = 5.5,
                                     width = 8,
                                     filt = TRUE,
                                     label = label,
                                     y.text = y.text,
                                     aggregate = FALSE) {
  
  dat2bar <- data.frame(YEAR, var2bar,x)
  
  if (filt == TRUE){mab_species <-  list("SUMMER FLOUNDER","SCUP","BLACK SEA BASS","BLUEFISH",
                                         "NORTHERN SHORTFIN SQUID", "LONGFIN SQUID", "ATLANTIC MACKEREL",
                                         "BUTTERFISH","ATLANTIC SURFCLAM", "OCEAN QUAHOG", "TILEFISH",
                                         "BLUELINE TILEFISH","SPINY DOGFISH", "GOOSEFISH")
  dat2plot <- dat2bar |>
    tidyr::gather(variable, value, -YEAR, -var2bar) |>
    dplyr::mutate(
      var2bar = gsub("^[A-Z]{2}\\s+LME\\s+", "", var2bar),
      var2bar = gsub("_", " ", var2bar),
      var2bar = gsub(pattern      = "Atl.",
                     replacement  = "ATLANTIC",
                     x            = var2bar),
      var2bar = gsub(pattern      = "Atl",
                     replacement  = "ATLANTIC",
                     x            = var2bar),
      var2bar = gsub(pattern      = "NS and combined",
                     replacement  = "",
                     x            = var2bar),
      var2bar = gsub(pattern      = "YT",
                     replacement  = "Yellowtail",
                     x            = var2bar),
      var2bar = gsub(pattern      = " GoM",
                     replacement  = " GOM",
                     x            = var2bar),
      var2bar = gsub(pattern      = " by EPU",
                     replacement  = "",
                     x            = var2bar)) |>
    dplyr::filter(var2bar %in% mab_species)
  } else if (filt == FALSE){
    dat2plot <- dat2bar |>
      tidyr::gather(variable, value, -YEAR, -var2bar) |>
      dplyr::mutate(
        var2bar = gsub("^[A-Z]{2}\\s+LME\\s+", "", var2bar),
        var2bar = gsub("_", " ", var2bar),
        
        var2bar = gsub(pattern      = "Atl.",
                       replacement  = "ATLANTIC",
                       x            = var2bar),
        var2bar = gsub(pattern      = "Atl",
                       replacement  = "ATLANTIC",
                       x            = var2bar),
        var2bar = gsub(pattern      = "NS and combined",
                       replacement  = "",
                       x            = var2bar),
        var2bar = gsub(pattern      = "YT",
                       replacement  = "Yellowtail",
                       x            = var2bar),
        var2bar = gsub(pattern      = " GoM",
                       replacement  = " GOM",
                       x            = var2bar),
        var2bar = gsub(pattern      = " by EPU",
                       replacement  = "",
                       x            = var2bar))
  }
  if (aggregate){
    agg <- dat2plot |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(
        Total = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::complete(
        YEAR = seq(min(YEAR), max(YEAR), by = 1),
        fill = list(Total = NA)
      )
    
  }
  
  p <-
    ggplot2::ggplot(dat2plot,
                    ggplot2::aes(x = YEAR)) +
    ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value > 0),
                      ggplot2::aes(y = value, fill = var2bar),
                      stat = "identity") +
    ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value < 0),
                      ggplot2::aes(y = value, fill = var2bar),
                      stat = "identity") +
    {if(aggregate) ggplot2::geom_line(data = agg,ggplot2::aes(x = YEAR, y = Total),
                                      linewidth = 1)} +
    ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(titl) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts()+
    ggplot2::theme(axis.title   = ggplot2::element_text(size = 12),
                   axis.text    = ggplot2::element_text(size = 12),
                   plot.title   = ggplot2::element_text(size = 15),
                   legend.text  = ggplot2::element_text(size = leg_font_size),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::annotate("text", label = label, x = 1980, y = y.text,size = 8, colour = "black")
  
  
  
  if(remove_leg) p <- p + theme(legend.position = "none")
  
  return(p)
}

# 4. generate comparison plots ---------

# Generate individual plots
p1 <- plot_productivity_anomaly(report = "MidAtlantic")
p2 <- ecodata::plot_productivity_anomaly(report = "MidAtlantic")

# Combine
comparison_plot <- cowplot::plot_grid(p1, p2, ncol = 1)

# Save to file
ggplot2::ggsave(
  filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/MA_fig36a.png",
  plot = comparison_plot,
  width = 8,      # adjust as needed
  height = 10,    # adjust as needed
  dpi = 300       # publication quality
)
# SOE 2024 didn't have Atlantic Cod in this plot
# I'm not sure why.
# Do we want to remove them?

p3 <- plot_productivity_anomaly(report = "MidAtlantic", varName = 'assessment')
p4 <- ecodata::plot_productivity_anomaly(varName = 'assessment')

comparison_plot <- cowplot::plot_grid(p3, p4, ncol = 1)

ggplot2::ggsave(
  filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/MA_fig36b.png",
  plot = comparison_plot,
  width = 8,      # adjust as needed
  height = 10,    # adjust as needed
  dpi = 300       # publication quality
)

p5 <- plot_productivity_anomaly(report = "NewEngland", EPU = "GOM")
p6 <- ecodata::plot_productivity_anomaly(report = "NewEngland", EPU = "GOM")

comparison_plot <- cowplot::plot_grid(p5, p6, ncol = 1)

ggplot2::ggsave(
  filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/NE_fig36a.png",
  plot = comparison_plot,
  width = 8,      # adjust as needed
  height = 10,    # adjust as needed
  dpi = 300       # publication quality
)
# Summer flounder is not in workflow but is in ecodata
# Fixed that issue but now BSB and Butterfish were added which are not in ecodata

p7 <- plot_productivity_anomaly(report = "NewEngland", varName = 'assessment')
p8 <- ecodata::plot_productivity_anomaly(report = "NewEngland", varName = 'assessment')
comparison_plot <- cowplot::plot_grid(p7, p8, ncol = 1)

ggplot2::ggsave(
  filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/NE_fig36b.png",
  plot = comparison_plot,
  width = 8,      # adjust as needed
  height = 10,    # adjust as needed
  dpi = 300       # publication quality
)
# workflow doesn't have Atlantic Cod but ecodata does
# should I add them?


# 5. Trawlr comparisons ----------

#' ## ecodata::get_productivity_anomaly --------
#' # trawlr files
#' load("~/EDAB_Dev/grezlik/trawlr_files/dat_spec_rec_epu_forSOE.Rdata")
#' load("~/EDAB_Dev/grezlik/trawlr_files/dat_spec_rec_forSOE.Rdata")
#' `AssessFishProdAnomaly - Sarah Gaichas - NOAA Federal` <- readRDS("~/EDAB_Dev/grezlik/trawlr_files/AssessFishProdAnomaly - Sarah Gaichas - NOAA Federal.rds")
#' 
#' #Select and rename
#' epu_rec_anom <- dat_spec_rec_epu_forSOE %>%
#'   dplyr::select(Time, EPU = Region, Value, Units, -Source,Var) %>%
#'   dplyr::filter(!Time == "2020")
#' 
#' #Select, rename, and bind
#' productivity_anomaly1 <- dat_spec_rec_forSOE %>%
#'   dplyr::select(-Source) %>%
#'   dplyr::mutate(EPU = "All",
#'                 Var = paste("NE LME",Var)) %>%
#'   rbind(.,epu_rec_anom) %>%
#'   as.data.frame()%>%
#'   tibble::as_tibble() %>%
#'   dplyr::select(Time, Var, Value, EPU, Units) %>%
#'   dplyr::mutate(Var = paste0(Var, "_Survey"))
#' 
#' 
#' prod_assess<- `AssessFishProdAnomaly - Sarah Gaichas - NOAA Federal`
#' prod_assess1<- prod_assess %>%
#'   tidyr::separate(StockName, into= c("Stock", "Region"), sep = "-") %>%
#'   dplyr::mutate(EPU = dplyr::recode(Region, " Gulf of Maine / Georges Bank" = "NE",
#'                                     " Gulf of Maine / Cape Hatteras" = "ALL",
#'                                     " Mid" = "MA",
#'                                     " Atlantic Coast" = "ALL",
#'                                     " Georges Bank" = "NE",
#'                                     " Northwestern Atlantic Coast" = "ALL",
#'                                     " Gulf of Maine" = "NE",
#'                                     " Southern New England / Mid" = "MA",
#'                                     " Cape Cod / Gulf of Maine" = "NE")) %>%
#'   tidyr::pivot_longer(cols = c("spawners_biom_lag0", "spawners_biom_lag0_anom",
#'                                "recruits_abund_lead1","recruits_abund_lead1_anom",
#'                                "rs","rs_anom","logr_abund_anom",
#'                                "logs_biom_anom","logrs_anom"),
#'                       names_to = "Var", values_to = "Value") %>%
#'   dplyr::mutate(Var = paste0(Stock, "-", Var, "-Assessment"),
#'                 Time = YEAR,
#'                 Units = c("NA")) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::select(Time, Var, Value, EPU, Units)
#' 
#' trawlr_productivity_anomaly<- rbind(productivity_anomaly1, prod_assess1)
#' 
#' ## plot_productivity_anomaly ----------
#' # using plot_productivity_anomaly function from ecodata
#' # replacing ecodata productivity_anomaly with the indicator generated above
#' 
#' plot_productivity_anomaly <- function(shadedRegion = NULL,
#'                                       report="MidAtlantic",
#'                                       varName = "anomaly",
#'                                       EPU = "MAB") {
#'   
#'   # generate plot setup list (same for all plot functions)
#'   setup <- ecodata::plot_setup(shadedRegion = shadedRegion,
#'                                report=report)
#'   
#'   # this should be added to plot_setip
#'   leg_font_size <- 6
#'   
#'   # which report? this may be bypassed for some figures
#'   if (report == "MidAtlantic") {
#'     if (varName == "anomaly") {
#'       filterEPUs <- "MAB"
#'     } else {
#'       filterEPUs <- c("MA")
#'     }
#'   } else {
#'     if (varName == "anomaly") {
#'       if (!(EPU %in% c("GB","GOM"))) {
#'         stop("For NewEngland the epu must be either 'GB' or 'GOM'")
#'       }
#'       filterEPUs <- EPU
#'     } else {
#'       filterEPUs <- c("NE")
#'     }
#'   }
#'   
#'   
#'   # optional code to wrangle ecodata object prior to plotting
#'   # e.g., calculate mean, max or other needed values to join below
#'   
#'   if (varName == "assessment") {
#'     fix<- trawlr_productivity_anomaly |>
#'       tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  |>
#'       dplyr::filter(EPU == filterEPUs,
#'                     Var == "rs_anom") |>
#'       dplyr::group_by(Time) |>
#'       dplyr::summarise(Total = sum(Value, na.rm = T),
#'                        Count = dplyr::n()) |> # SG add a count of species
#'       dplyr::mutate(Totalold = ifelse(Total == 0, NA, Total),
#'                     Total = ifelse(Count < max(Count), NA, Total)) |>
#'       dplyr::filter(!is.na(Total))
#'     
#'     prod<- trawlr_productivity_anomaly |>
#'       tidyr::separate(Var, into = c("Stock", "Var"), sep = "-")  |>
#'       dplyr::filter(EPU == filterEPUs,
#'                     Var == "rs_anom") |>
#'       dplyr::mutate(Stock = toupper(Stock))
#'     
#'     # code for generating plot object p
#'     # ensure that setup list objects are called as setup$...
#'     # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
#'     # xmin = setup$x.shade.min , xmax = setup$x.shade.max
#'     #
#'     p <-
#'       ggplot2::ggplot(prod, ggplot2::aes(x = Time)) +
#'       ggplot2::geom_bar(data = prod |> dplyr::filter(Value > 0),
#'                         ggplot2::aes(y = Value, fill = Stock),
#'                         stat = "identity") +
#'       ggplot2::geom_bar(data = prod |> dplyr::filter(Value < 0),
#'                         ggplot2::aes(y = Value, fill = Stock),
#'                         stat = "identity") +
#'       ggplot2::geom_line(data = fix, ggplot2::aes(x = Time, y = Total),
#'                          linewidth = 1) +
#'       ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
#'       ggplot2::xlab("") +
#'       ggplot2::ylab("Recruitment Anomaly") +
#'       ggplot2::ggtitle(paste0(filterEPUs," Recruitment Anomaly from Stock Assessments_trawlr")) +
#'       #ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
#'       ecodata::theme_ts()+
#'       ggplot2::theme(axis.title   = ggplot2::element_text(size = 10),
#'                      axis.text    = ggplot2::element_text(size = 10),
#'                      plot.title   = ggplot2::element_text(size = 12),
#'                      #legend.text  = element_text(size = leg_font_size),
#'                      legend.title = ggplot2::element_blank(),
#'                      legend.text=ggplot2::element_text(size=6))
#'     
#'   }
#'   
#'   if (varName == "anomaly") {
#'     bar_dat <- trawlr_productivity_anomaly |>
#'       dplyr::filter(EPU == filterEPUs) |>
#'       tidyr::separate(Var, into = c("Var", "Survey"), sep = "_")
#'     
#'     adjustAxes <-
#'       ggplot2::theme(axis.title   = ggplot2::element_text(size = 10),
#'                      axis.text    = ggplot2::element_text(size = 10),
#'                      plot.title   = ggplot2::element_text(size = 15))
#'     
#'     p <- plot_stackbarcpts_single(YEAR = bar_dat$Time,
#'                                   var2bar = bar_dat$Var,
#'                                   x = bar_dat$Value,
#'                                   titl = paste0(EPU, " from survey data_trawlr"),
#'                                   xlab = "",
#'                                   ylab = "Small fish per large fish biomass (anomaly)",
#'                                   height = 5.5,
#'                                   width = 9,
#'                                   filt = FALSE,
#'                                   leg_font_size = leg_font_size,
#'                                   label = "",
#'                                   y.text = 10,
#'                                   aggregate = TRUE)
#'     
#'     
#'     
#'   }
#'   
#'   if (varName == "assessment" & EPU == "GOM") {
#'     p <- "Assessment variable includes GB and GOM. See plot for EPU = GB."
#'   }
#'   
#'   return(p)
#'   
#' }
#' 
#' attr(plot_productivity_anomaly,"report") <- c("MidAtlantic","NewEngland")
#' attr(plot_productivity_anomaly,"varName") <- c("anomaly","assessment")
#' attr(plot_productivity_anomaly,"EPU") <- c("MAB","GB","GOM")
#' 
#' 
#' #' anomaly stacked barchart. needs to be reworked
#' #' @noRd
#' plot_stackbarcpts_single <- function(YEAR, var2bar,
#'                                      x, xlab, ylab,
#'                                      titl,
#'                                      file_suffix,
#'                                      leg_font_size = leg_font_size,
#'                                      remove_leg = FALSE,
#'                                      leg_ncol = 1,
#'                                      wcpts = TRUE,
#'                                      wdashed = TRUE,
#'                                      height = 5.5,
#'                                      width = 8,
#'                                      filt = TRUE,
#'                                      label = label,
#'                                      y.text = y.text,
#'                                      aggregate = FALSE) {
#'   
#'   dat2bar <- data.frame(YEAR, var2bar,x)
#'   
#'   if (filt == TRUE){mab_species <-  list("SUMMER FLOUNDER","SCUP","BLACK SEA BASS","BLUEFISH",
#'                                          "NORTHERN SHORTFIN SQUID", "LONGFIN SQUID", "ATLANTIC MACKEREL",
#'                                          "BUTTERFISH","ATLANTIC SURFCLAM", "OCEAN QUAHOG", "TILEFISH",
#'                                          "BLUELINE TILEFISH","SPINY DOGFISH", "GOOSEFISH")
#'   dat2plot <- dat2bar |>
#'     tidyr::gather(variable, value, -YEAR, -var2bar) |>
#'     dplyr::mutate(var2bar = gsub(pattern      = "_",
#'                                  replacement  = " ",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = "Atl.",
#'                                  replacement  = "ATLANTIC",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = "Atl",
#'                                  replacement  = "ATLANTIC",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = "NS and combined",
#'                                  replacement  = "",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = "YT",
#'                                  replacement  = "Yellowtail",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = " GoM",
#'                                  replacement  = " GOM",
#'                                  x            = var2bar),
#'                   var2bar = gsub(pattern      = " by EPU",
#'                                  replacement  = "",
#'                                  x            = var2bar)) |>
#'     dplyr::filter(var2bar %in% mab_species)
#'   } else if (filt == FALSE){
#'     dat2plot <-
#'       dat2bar |>
#'       tidyr::gather(variable, value, -YEAR, -var2bar) |>
#'       dplyr::mutate(var2bar = gsub(pattern      = "_",
#'                                    replacement  = " ",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = "Atl.",
#'                                    replacement  = "ATLANTIC",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = "Atl",
#'                                    replacement  = "ATLANTIC",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = "NS and combined",
#'                                    replacement  = "",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = "YT",
#'                                    replacement  = "Yellowtail",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = " GoM",
#'                                    replacement  = " GOM",
#'                                    x            = var2bar),
#'                     var2bar = gsub(pattern      = " by EPU",
#'                                    replacement  = "",
#'                                    x            = var2bar))
#'   }
#'   if (aggregate){
#'     agg <- dat2plot |>
#'       dplyr::group_by(YEAR) |>
#'       dplyr::summarise(Total = sum(value, na.rm = T)) |>
#'       dplyr::mutate(Total = ifelse(Total == 0, NA, Total)) |>
#'       dplyr::filter(!is.na(Total))
#'   }
#'   
#'   p <-
#'     ggplot2::ggplot(dat2plot,
#'                     ggplot2::aes(x = YEAR)) +
#'     ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value > 0),
#'                       ggplot2::aes(y = value, fill = var2bar),
#'                       stat = "identity") +
#'     ggplot2::geom_bar(data = dat2plot |> dplyr::filter(value < 0),
#'                       ggplot2::aes(y = value, fill = var2bar),
#'                       stat = "identity") +
#'     {if(aggregate) ggplot2::geom_line(data = agg,ggplot2::aes(x = YEAR, y = Total),
#'                                       linewidth = 1)} +
#'     ggplot2::geom_hline(size = 0.3, ggplot2::aes(yintercept = 0)) +
#'     ggplot2::xlab(xlab) +
#'     ggplot2::ylab(ylab) +
#'     ggplot2::ggtitle(titl) +
#'     ggplot2::guides(fill = ggplot2::guide_legend(ncol = leg_ncol)) +
#'     ecodata::theme_ts()+
#'     ggplot2::theme(axis.title   = ggplot2::element_text(size = 12),
#'                    axis.text    = ggplot2::element_text(size = 12),
#'                    plot.title   = ggplot2::element_text(size = 15),
#'                    legend.text  = ggplot2::element_text(size = leg_font_size),
#'                    legend.title = ggplot2::element_blank()) +
#'     ggplot2::annotate("text", label = label, x = 1980, y = y.text,size = 8, colour = "black")
#'   
#'   
#'   
#'   if(remove_leg) p <- p + theme(legend.position = "none")
#'   
#'   return(p)
#' }
#' 
#' ## plot comparisons -------------
#' 
#' # Generate individual plots
#' p1 <- plot_productivity_anomaly(report = "MidAtlantic")
#' 
#' # Combine
#' comparison_plot <- cowplot::plot_grid(trawlr_p1, p2, ncol = 1)
#' 
#' # Save to file
#' ggplot2::ggsave(
#'   filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/trawlr_ecodata_MA_fig36a.png",
#'   plot = comparison_plot,
#'   width = 8,      # adjust as needed
#'   height = 10,    # adjust as needed
#'   dpi = 300       # publication quality
#' )
#' 
#' 
#' trawlr_p3 <- plot_productivity_anomaly_trawlr(report = "MidAtlantic", varName = 'assessment')
#' 
#' comparison_plot <- cowplot::plot_grid(trawlr_p3, p4, ncol = 1)
#' 
#' ggplot2::ggsave(
#'   filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/trawlr_ecodata_fig36b.png",
#'   plot = comparison_plot,
#'   width = 8,      # adjust as needed
#'   height = 10,    # adjust as needed
#'   dpi = 300       # publication quality
#' )
#' 
#' trawlr_p5 <- plot_productivity_anomaly_trawlr(report = "NewEngland", EPU = "GOM")
#' 
#' comparison_plot <- cowplot::plot_grid(trawlr_p5, p6, ncol = 1)
#' 
#' ggplot2::ggsave(
#'   filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/trawlr_ecodata_fig36a.png",
#'   plot = comparison_plot,
#'   width = 8,      # adjust as needed
#'   height = 10,    # adjust as needed
#'   dpi = 300       # publication quality
#' )
#' # Summer flounder is not in workflow but is in ecodata
#' # Fixed that issue but now BSB and Butterfish were added which are not in ecodata
#' 
#' trawlr_p7 <- plot_productivity_anomaly(report = "NewEngland", varName = 'assessment')
#' comparison_plot <- cowplot::plot_grid(trawlr_p7, p8, ncol = 1)
#' 
#' ggplot2::ggsave(
#'   filename = "/home/mgrezlik/Maxwell.Grezlik/Rprojects/READ_EDAB_SOE_Workflows/data-raw/productivity_anomaly_comparison_plots/trawlr_ecodata_NE_fig36b.png",
#'   plot = comparison_plot,
#'   width = 8,      # adjust as needed
#'   height = 10,    # adjust as needed
#'   dpi = 300       # publication quality
#' )
#' # workflow doesn't have Atlantic Cod but ecodata does
#' # should I add them?