# dir.create(here::here("data-raw/temp"))

comdat_path <- '~/EDAB_Datasets/Workflows/commercial_comdat.rds'
input_path_species <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
menhaden_path <- "~/EDAB_Datasets/Workflows/menhadenEOF.rds"
outputPathDataSets <- "~/EDAB_Indicators"

source(here::here("data-raw/workflow_comdat.R"))

workflow_comdat(comdat_path = comdat_path,
                  input_path_species = input_path_species,
                  menhaden_path = menhaden_path,
                  outputPathDataSets = outputPathDataSets)

# compare to ecodata::comdat

new_comdat <- workflow_comdat(comdat_path = comdat_path,
                                   input_path_species = input_path_species,
                                   menhaden_path = menhaden_path,
                                   outputPathDataSets = outputPathDataSets)

new_comdat <- new_comdat |> 
                  dplyr::mutate(source = 'workflow')

old_comdat <- ecodata::comdat |> 
                  dplyr::mutate(source = 'ecodata')

comdat_compare <- dplyr::bind_rows(new_comdat, old_comdat)

library(stringr)
library(dplyr)
library(ggplot2)
library(ecodata)


# Filter for relevant variables, keeping the source distinct
total_landings <- comdat_compare |>
  dplyr::filter(
    str_detect(Var, "Landings"),
    !str_detect(Var, "Seafood|US only"), # Exclude seafood-only and US-only aggregates
    Time >= 1982
  ) |>
  dplyr::mutate(
    feeding.guild = str_extract(Var, "^\\w+"),
    grouping = "Total"
  )

setup <- ecodata::plot_setup(shadedRegion = NULL, report = 'MidAtlantic')

managed_landings <- comdat_compare |>
  dplyr::filter(
    str_detect(Var, "managed species - Landings"),
    !str_detect(Var, "US only"),
    Time >= 1982,
    str_detect(Var, paste0("JOINT|", setup$council_abbr))
  ) |>
  dplyr::mutate(
    feeding.guild = str_extract(Var, "^\\w+"),
    grouping = "Council Managed"
  )

# Prepare data for the "guild" plot
guilddat <- dplyr::bind_rows(total_landings, managed_landings) |>
  dplyr::filter(!feeding.guild %in% c("Apex", "Other", "Landings")) |>
  dplyr::group_by(Time, EPU, source, feeding.guild, grouping) |>
  dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds))


plot_guild_landings <- guilddat |>
  ggplot(aes(x = Time, y = Value, color = source, linetype = grouping)) +
  geom_line(linewidth = setup$lwd) +
  facet_grid(feeding.guild ~ EPU, scales = "free_y")



# Prepare data for the "total" plot
totdat <- guilddat |>
  dplyr::group_by(Time, EPU, source, grouping) |>
  dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
  dplyr::rename(Var = grouping)

plot_total_landings <- totdat |>
  ggplot(aes(x = Time, y = Value, color = source, linetype = Var)) +
  geom_line(linewidth = setup$lwd) +
  geom_point(aes(shape = Var), size = setup$pcex) +
  facet_wrap(~EPU, scales = "free_y")

land_ylabdat <- expression("Landings (10"^3 * " metric tons)")

## DATA WRANGLING FOR REVENUE ----

# Filter for relevant variables, keeping the source distinct
total_revenue <- comdat_compare |>
  dplyr::filter(
    str_detect(Var, "Revenue"),
    !str_detect(Var, "managed|US only"),
    Time >= 1982
  ) |>
  dplyr::mutate(
    feeding.guild = str_extract(Var, "^\\w+"),
    grouping = "Total"
  )

managed_revenue <- comdat_compare |>
  dplyr::filter(
    str_detect(Var, "managed species - Revenue"),
    !str_detect(Var, "US only"),
    Time >= 1982,
    str_detect(Var, paste0("JOINT|", setup$council_abbr))
  ) |>
  dplyr::mutate(
    feeding.guild = str_extract(Var, "^\\w+"),
    grouping = "Council Managed"
  )

# Prepare data for the "guild" plot
guilddat <- bind_rows(total_revenue, managed_revenue) |>
  dplyr::filter(!feeding.guild %in% c("Apex", "Other", "Revenue")) |>
  dplyr::group_by(Time, EPU, source, feeding.guild, grouping) |>
  dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
  dplyr::mutate(feeding.guild = factor(feeding.guild, levels = setup$feeding.guilds))

plot_guild_revenue <- guilddat |>
  ggplot(aes(x = Time, y = Value, color = source, linetype = grouping)) +
  geom_line(linewidth = setup$lwd) +
  facet_grid(feeding.guild ~ EPU, scales = "free_y")

# Prepare data for the "total" plot
totdat <- guilddat |>
  dplyr::group_by(Time, EPU, source, grouping) |>
  dplyr::summarise(Value = sum(Value, na.rm = TRUE) / 1000, .groups = "drop") |>
  dplyr::rename(Var = grouping)

rev_ylabdat <- expression("Revenue (10"^6 * " USD)")

plot_total_revenue <- totdat |>
  ggplot(aes(x = Time, y = Value, color = source, linetype = Var)) +
  geom_line(linewidth = setup$lwd) +
  geom_point(aes(shape = Var), size = setup$pcex) +
  facet_wrap(~EPU, scales = "free_y")

## PLOTTING ----

# Apply common aesthetics
p_tot_rev <- plot_total_revenue +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(y = rev_ylabdat, x = NULL) +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ecodata::theme_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())

p_guild_rev <- plot_guild_revenue +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(y = rev_ylabdat, x = NULL) +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ecodata::theme_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())

p_tot_land <- plot_total_landings +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(y = land_ylabdat, x = NULL) +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ecodata::theme_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())

p_guild_land <- plot_guild_landings +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), expand = c(0.01, 0.01)) +
  labs(y = land_ylabdat, x = NULL) +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ecodata::theme_facet() +
  theme(legend.position = "bottom", legend.title = element_blank())

# look at plots
p_tot_rev
p_guild_rev
p_tot_land
p_guild_land
