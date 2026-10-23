### Data pulls

# pull all northeast stock
northeast_stocks <- stocksmart::stock_assessment_summary |>
  dplyr::filter(grepl("Northeast", regional_ecosystem)) |>
  dplyr::distinct(stock_name, stock_id, itis, jurisdiction, science_center)


all_bio <- NULL
all_mort <- NULL
# loop over all stocks ato pull timeseries of b/bmsy and f/fmsy
for (istock in 1:nrow(northeast_stocks)) {
  species_stock <- northeast_stocks[istock, ]
  bio <- stocksmart::get_reference_points(
    stock = species_stock$stock_id,
    ref_point = "b_over_bmsy"
  ) |>
    dplyr::mutate(stock = species_stock$stock_name)
  fmort <- stocksmart::get_reference_points(
    stock = species_stock$stock_id,
    ref_point = "f_over_fmsy"
  ) |>
    dplyr::mutate(stock = species_stock$stock_name)

  # store all data in tidy format
  all_bio <- rbind(all_bio, bio)
  all_mort <- rbind(all_mort, fmort)
}

# add regional management column
bio_regional <- all_bio |>
  dplyr::mutate(
    region = dplyr::case_when(
      stringr::str_detect(
        stock,
        "Atlantic surfclam|Ocean quahog|Summer flounder|Scup|Black sea bass|Atlantic mackerel|Butterfish|Bluefish|tilefish|Tilefish|squid"
      ) ~
        "MAFMC",
      stringr::str_detect(
        stock,
        "Atlantic cod|Haddock|Yellowtail flounder|Ocean pout|Windowpane|Pollock|American plaice|Witch flounder|Winter flounder|Atlantic wolffish|White hake|Acadian redfish|Atlantic halibut|Red deepsea crab|Sea scallop|Atlantic herring|Silver hake|Red hake|Offshore hake|Atlantic salmon|skate"
      ) ~
        "NEFMC",
      stringr::str_detect(stock, "Spiny dogfish|Goosefish") ~ "NEFMC/MAFMC",
      TRUE ~ "Other" # Default fallback
    )
  )

# species, stock, and last assessment year columns
bio_species <- bio_regional |>
  tidyr::separate_wider_delim(
    cols = stock,
    delim = " - ",
    names = c("species", "stock")
  ) |> # split stock into species and stock columns
  dplyr::filter(species != "Atlantic menhaden") |> #remove menhaden
  dplyr::group_by(species) |>
  dplyr::mutate(
    stock = dplyr::case_when(
      dplyr::n_distinct(stock) == 1 ~ "UNIT",
      TRUE ~ stock
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(species) |> # add column for last assessment year = YES or NO
  dplyr::mutate(
    last_assessment = dplyr::if_else(
      assessment_year == max(assessment_year, na.rm = TRUE),
      "YES",
      "NO"
    )
  ) |>
  dplyr::ungroup()

#Add guild column from ecodata::species_groupings
species_groupings <- ecodata::species_groupings |>
  dplyr::mutate(
    COMNAME = stringr::str_to_sentence(COMNAME),
    COMNAME = dplyr::case_when(
      COMNAME %in% c("Crab, red deepsea", "Crab,red deepsea") ~
        "Red deepsea crab",
      COMNAME == "Longfin squid" ~ "Longfin inshore squid",
      TRUE ~ COMNAME
    )
  ) |>
  dplyr::rename(species = COMNAME) |>
  dplyr::select(species, SOE.24) |>
  dplyr::distinct()

bio_species_guild <- bio_species |>
  dplyr::left_join(species_groupings, by = "species")

#Add year column and fill in missing assessment years
bio_expanded <- bio_species_guild |>
  dplyr::group_by(species) |>
  dplyr::mutate(year = assessment_year) |>
  tidyr::complete(year = 2002:2025) |>
  dplyr::mutate(
    assessment_year = zoo::na.locf(assessment_year, na.rm = FALSE)
  ) |>
  dplyr::arrange(species, year) |>
  dplyr::ungroup()

##############################################
## create time series of msys ----

### first pull msys
msys <- stocksmart::stock_assessment_summary |>
  dplyr::filter(science_center == "NEFSC") |>
  dplyr::select(
    stock_name,
    common_name,
    stock_area,
    jurisdiction,
    assessment_year,
    review_result,
    fmsy,
    f_unit,
    bmsy,
    b_unit
  ) |>
  # remove rejected assessments
  dplyr::filter(review_result == "Full acceptance") |>
  dplyr::select(-review_result) |>
  # make NA's real values to propagate in gap filling
  dplyr::mutate(
    fmsy = ifelse(is.na(fmsy), "NA", fmsy),
    bmsy = ifelse(is.na(bmsy), "NA", bmsy)
  )

### pad out to full time series
filled_msys <- msys |>
  dplyr::mutate(year = assessment_year) |>
  # Group by stock-level metadata to keep them associated with the stock
  dplyr::group_by(
    stock_name,
    common_name,
    stock_area,
    jurisdiction,
  ) |>
  # Generate a continuous sequence of years for each stock up to end_year
  tidyr::complete(year = min(year):2025) |>
  # Forward-fill assessment metrics within each group
  tidyr::fill(
    fmsy,
    f_unit,
    bmsy,
    b_unit,
    assessment_year,
    .direction = "down"
  ) |>
  dplyr::ungroup()

## get actual f and b data ----
## actual f and b data from the most recent assessment
actual_fb <- stocksmart::stock_assessment_data |>
  dplyr::filter(
    stock_name %in% filled_msys$stock_name,
    metric %in% c("Fmort", "Abundance")
  ) |>
  dplyr::select(
    stock_name,
    year,
    value,
    metric,
    description,
    assessment_year
  ) |>
  dplyr::group_by(stock_name) |>
  dplyr::mutate(last_assessment = max(assessment_year)) |>
  dplyr::ungroup() |>
  dplyr::filter(assessment_year == last_assessment) |>
  dplyr::select(-c(assessment_year, last_assessment))

## join actual f and b data with filled msys data ----
joined_dat <- dplyr::left_join(
  actual_fb,
  filled_msys,
  by = c("stock_name", "year")
) |>
  dplyr::filter(!is.na(common_name)) |>
  dplyr::mutate(
    b_bmsy = dplyr::case_when(
      metric == "Abundance" & bmsy != "NA" ~ value / as.numeric(bmsy),
      TRUE ~ NA_real_
    ),
    f_fmsy = dplyr::case_when(
      metric == "Fmort" & fmsy != "NA" ~ value / as.numeric(fmsy),
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::select(-stock_name)

## create f_fmsy and b_bmsy datasets ----
f_fmsys <- joined_dat |>
  dplyr::filter(!is.na(f_fmsy)) |>
  dplyr::select(-c(bmsy, b_unit, b_bmsy))
b_bmsys <- joined_dat |>
  dplyr::filter(!is.na(b_bmsy)) |>
  dplyr::select(-c(fmsy, f_unit, f_fmsy))

## convert b_bmsy units ----
b_bmsy_converted <- b_bmsys |>
  dplyr::mutate(
    bmsy = as.numeric(bmsy),
    needs_conversion = b_unit %in%
      c(
        "Thousand Metric Tons",
        "thousand mt (female biomass only)",
        "mt (meat weight only)",
        "mt SSB"
      ),
    b_msy_corrected = dplyr::if_else(needs_conversion, bmsy * 1000, bmsy),
    b_unit_corrected = dplyr::if_else(needs_conversion, "Metric Tons", b_unit)
  ) |>
  dplyr::select(-needs_conversion)

## drop years where there is a mismatch in bmsy units (few years as kg/tow, or million pups for spiny dogfish) ----
# List of species with bmsy values in kg/tow that need to be dropped
target_species <- c(
  "Haddock",
  "Pollock",
  "Scup",
  "Sea scallop",
  "Spiny dogfish"
)
target_units <- c("kg/tow", "kg / Tow", "Million Pups")

b_bmsy_dropyears <- b_bmsy_converted |>
  dplyr::mutate(
    b_msy_corrected = dplyr::if_else(
      common_name %in% target_species & b_unit %in% target_units,
      NA_real_, # Set to NA if condition is met
      b_msy_corrected # Keep original value otherwise
    )
  )

## recalculate b/b_msy with converted bmsy ----
b_bmsy_final <- b_bmsy_dropyears |>
  dplyr::mutate(
    b_bmsy_corrected = dplyr::case_when(
      metric == "Abundance" & !is.na(b_msy_corrected) ~ value / b_msy_corrected,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::rename(b_over_bmsy = b_bmsy_corrected, species = common_name) |>
  dplyr::select(-c(bmsy, b_unit, b_bmsy))

######## B/Bmsy ########
#Join to all years b_over_bmsy data
bio_all <- bio_expanded |>
  dplyr::left_join(
    b_bmsy_final |>
      dplyr::select(species, year, b_over_bmsy_fill = b_over_bmsy),
    by = c("species", "year")
  ) |>
  dplyr::mutate(b_over_bmsy = dplyr::coalesce(b_over_bmsy, b_over_bmsy_fill)) |>
  dplyr::select(-b_over_bmsy_fill)

write.csv(bio_all, "data-raw/b_bmsy_all_years.csv", row.names = FALSE)

####### F/FMSY ##########

mort_regional <- all_mort |>
  dplyr::mutate(
    region = dplyr::case_when(
      stringr::str_detect(
        stock,
        "Atlantic surfclam|Ocean quahog|Summer flounder|Scup|Black sea bass|Atlantic mackerel|Butterfish|Bluefish|tilefish|Tilefish|squid"
      ) ~
        "MAFMC",
      stringr::str_detect(
        stock,
        "Atlantic cod|Haddock|Yellowtail flounder|Ocean pout|Windowpane|Pollock|American plaice|Witch flounder|Winter flounder|Atlantic wolffish|White hake|Acadian redfish|Atlantic halibut|Red deepsea crab|Sea scallop|Atlantic herring|Silver hake|Red hake|Offshore hake|Atlantic salmon|skate"
      ) ~
        "NEFMC",
      stringr::str_detect(stock, "Spiny dogfish|Goosefish") ~ "NEFMC/MAFMC",
      TRUE ~ "Other" # Default fallback
    )
  )

# species, stock, and last assessment year columns
mort_species <- mort_regional |>
  tidyr::separate_wider_delim(
    cols = stock,
    delim = " - ",
    names = c("species", "stock")
  ) |> # split stock into species and stock columns
  dplyr::filter(species != "Atlantic menhaden") |> #remove menhaden
  dplyr::group_by(species) |>
  dplyr::mutate(
    stock = dplyr::case_when(
      dplyr::n_distinct(stock) == 1 ~ "UNIT",
      TRUE ~ stock
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(species) |> # add column for last assessment year = YES or NO
  dplyr::mutate(
    last_assessment = dplyr::if_else(
      assessment_year == max(assessment_year, na.rm = TRUE),
      "YES",
      "NO"
    )
  ) |>
  dplyr::ungroup()

#Add guild column from ecodata::species_groupings
mort_species_guild <- mort_species |>
  dplyr::left_join(species_groupings, by = "species")

#######################################################
# LINE PLOTS

######### F/Fmsy ##########

# Filter out missing values
mort_clean <- mort_regional |>
  dplyr::filter(!is.na(f_over_fmsy))

# Plot
ggplot2::ggplot(
  mort_clean,
  ggplot2::aes(x = assessment_year, y = f_over_fmsy)
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray",
    size = 0.8
  ) +
  ggplot2::geom_point(color = "black", size = 2) +
  ggplot2::stat_summary(fun = mean, geom = "line", color = "red", size = 1) +
  ggplot2::stat_summary(fun = mean, geom = "point", color = "red", size = 2.5) +
  ggplot2::facet_wrap(~region, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title = "F/Fmsy for Managed Species",
    x = NULL,
    y = "F/Fmsy"
  ) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "plain", hjust = 0),
    legend.title = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 12, color = "black"),
    axis.text = ggplot2::element_text(size = 11, color = "black"),
    panel.border = ggplot2::element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.line = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "gray90", color = "black"),
    strip.text = ggplot2::element_text(size = 11, face = "bold")
  )

######## B/Bmsy #########

# Filter out missing values
bio_clean <- bio_regional |>
  dplyr::filter(!is.na(b_over_bmsy))

# Plot
ggplot2::ggplot(bio_clean, ggplot2::aes(x = assessment_year, y = b_over_bmsy)) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray",
    size = 0.8
  ) +
  ggplot2::geom_point(color = "black", size = 2) +
  ggplot2::stat_summary(fun = mean, geom = "line", color = "red", size = 1) +
  ggplot2::stat_summary(fun = mean, geom = "point", color = "red", size = 2.5) +
  ggplot2::facet_wrap(~region, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title = "B/Bmsy for Managed Species",
    x = NULL,
    y = "B/Bmsy"
  ) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "plain", hjust = 0),
    legend.title = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 12, color = "black"),
    axis.text = ggplot2::element_text(size = 11, color = "black"),
    panel.border = ggplot2::element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.line = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "gray90", color = "black"),
    strip.text = ggplot2::element_text(size = 11, face = "bold")
  )
