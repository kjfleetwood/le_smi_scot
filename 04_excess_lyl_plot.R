
library(tidyverse)

#
# 1. Load data ----------------------------------------------------------------
#

lyl_tab <- read.csv("life_years_lost.csv")

#
# 2. Prepare data -------------------------------------------------------------
#

# - Define period start = year at the start of the period
# - Convert smi to factor
lyl_tab <-
  lyl_tab %>%
  mutate(
    period_start = as.numeric(substring(period, 1,4)),
    smi = factor(smi, levels = c("Schizophrenia", "Bipolar disorder", "Major depression"))
  )

#
# 3. Create plot --------------------------------------------------------------
#

# Define colours
plot_purple <- rgb(110, 94, 156, max = 255)

# Create plot of life years lost vs Scottish population by SMI and sex
ggplot(data = lyl_tab, aes(x = period_start, y = smi_lyl_pop)) +
  geom_ribbon(
    aes(
      ymin = smi_lyl_pop_lower,
      ymax = smi_lyl_pop_upper,
    ),
    fill = plot_purple, alpha = 0.2
  ) + 
  geom_line(colour = plot_purple, linewidth = 0.8) +
  facet_grid(vars(sex), vars(smi)) +
  ylim(0, 15) +
  theme_bw() +
  theme(strip.background =element_rect(fill=NA)) +
  xlab("Year*") +
  ylab("Excess Life Years Lost") 

ggsave("fig3_excess_lyl.pdf", width = 8.6, height = 5.66)