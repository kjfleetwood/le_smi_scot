library(tidyverse)

#
# 1. Load data ----------------------------------------------------------------
#

survival_plot_data <- read.csv("survival_plot_data.csv")

#
# 2. Prepare data -------------------------------------------------------------
#

survival_plot_35 <- 
  survival_plot_data %>%
  mutate(
    smi = 
      factor(
        smi, 
        levels = 
          c("Scottish population",
            "Schizophrenia",
            "Bipolar disorder",
            "Major depression")
        )
  )

#
# 3. Create plot --------------------------------------------------------------
#

# Define colours
plot_purple <- rgb(110, 94, 156, max = 255)
plot_yellow <- rgb(253, 175, 23, max = 255)
plot_pink <- rgb(210, 14, 140, max = 255)
plot_blue <- rgb(16, 113, 184, max = 255)

ggplot(
  data = survival_plot_35, 
  aes(x = age, y = survival, col = smi)
) +
  facet_grid(cols = vars(sex), rows = vars(period)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = "Age (years)",
    y = "Proportion of people alive"
  ) +
  theme_bw(base_size = 15)  +
  theme(strip.background =element_rect(fill=NA))+
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c(plot_purple, plot_yellow, plot_pink, plot_blue))+ 
  scale_fill_manual(values = c(plot_purple, plot_yellow, plot_pink, plot_blue))

ggsave("fig4_survival.pdf", width = 9, height = 8)
