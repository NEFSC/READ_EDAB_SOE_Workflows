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

######### B/BMSY ##########

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

# add guild column

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

# add guild column

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
