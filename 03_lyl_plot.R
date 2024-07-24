
library(tidyverse)

#
# 1. Load data ----------------------------------------------------------------
#

lyl <- read.csv("life_years_lost.csv")

#
# 2. Prepare data -------------------------------------------------------------
#

lyl_total <- 
  lyl %>%
  select(
    smi, sex, period, 
    starts_with("smi_lyl_95"), 
  ) %>%
  mutate(
    lyl_type = "Total"
  ) %>%
  rename(
    smi_lyl = smi_lyl_95,
    smi_lyl_lower = smi_lyl_95_lower,
    smi_lyl_upper = smi_lyl_95_upper
  )

lyl_nat <- 
  lyl %>%
  select(
    smi, sex, period, 
    starts_with("smi_lyl_nat"), 
  ) %>%
  mutate(
    lyl_type = "Natural"
  ) %>%
  rename(
    smi_lyl = smi_lyl_nat_95,
    smi_lyl_lower = smi_lyl_nat_95_lower,
    smi_lyl_upper = smi_lyl_nat_95_upper
  )

lyl_unn <- 
  lyl %>%
  select(
    smi, sex, period, 
    starts_with("smi_lyl_unn"), 
  ) %>%
  mutate(
    lyl_type = "Unnatural"
  ) %>%
  rename(
    smi_lyl = smi_lyl_unn_95,
    smi_lyl_lower = smi_lyl_unn_95_lower,
    smi_lyl_upper = smi_lyl_unn_95_upper
  )

lyl_long <- 
  bind_rows(
    lyl_total, 
    lyl_nat,
    lyl_unn
  ) %>%
  mutate(
    period_start = as.numeric(substring(period, 1,4)),
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Major depression")),
    lyl_type = factor(lyl_type, levels = c("Total", "Natural", "Unnatural"))
  )

#
# 3. Create plot ---------------------------------------------------------------
#

# Define colours
plot_purple <- rgb(110, 94, 156, max = 255)
plot_yellow <- rgb(253, 175, 23, max = 255)
plot_pink <- rgb(210, 14, 140, max = 255)

ggplot(
  data = lyl_long,
  aes(x = period_start, y = smi_lyl, col = lyl_type)
) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = smi_lyl_lower, ymax = smi_lyl_upper, fill = lyl_type), 
    alpha = 0.2, color = NA) +
  facet_grid(vars(sex), vars(smi)) +
  xlab("Year*") + 
  ylab("Life Years Lost") + 
  theme_bw()+
  theme(strip.background =element_rect(fill=NA)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c(plot_purple, plot_yellow, plot_pink))+ 
  scale_fill_manual(values = c(plot_purple, plot_yellow, plot_pink))

ggsave("fig2_life_years_lost.pdf", width = 9, height = 5.66)
