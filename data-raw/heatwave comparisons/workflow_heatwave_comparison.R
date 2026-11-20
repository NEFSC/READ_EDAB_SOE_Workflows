source(here::here('data-raw', 'workflow_heatwave.R'))
library(dplyr)
library(ggplot2)

current.detrend <- workflow_heatwave(
  input_path_bot = "//nefscdata/EDAB_Dev/jcaracappa/glorys_SOE_test/SOE_Indicators/GLORYS_bottom_temp_daily_epu_1993-01-01_2026-08-29.csv",
  input_path_surf = "//nefscdata/EDAB_Indicators/SOE_ecodata/SOE_2027/OISST_daily_epu_heatwave_1981_2026_2026-11-12.csv",
  output_path_indicators = here::here(),
  detrend = T
)

data <- current.detrend |>
  rename(new.detrend = 'Value') |>
  left_join(ecodata::heatwave) |>
  rename(old = 'Value') |>
  tidyr::gather(
    key = "type",
    value = "value",
    new.detrend,
    old
  ) |>
  mutate(value = ifelse(value == 0, NA, value)) |>
  filter(EPU == 'MAB')

ggplot(data, aes(x = Time, y = value, color = type)) +
  geom_line() +
  geom_point() +
  facet_grid(Var ~ EPU, scale = 'free', labeller = label_wrap_gen())
ggsave(here::here('tests', 'heatwave_comparison.png'), width = 12, height = 12)
