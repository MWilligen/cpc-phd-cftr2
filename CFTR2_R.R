library(tidyverse)
library("readxl")
library(ggpubr)

setwd("~/R/CFTR2")

std <- function(x) sd(x)/sqrt(length(x))

CFTR2_data <- read_excel("~/R/CFTR2/CFTR2_Bigdatasheet.xlsx", sheet = "-Bkgn", trim_ws = TRUE)
CFTR2_data <- CFTR2_data %>% unite(Mutant_name, AA, Number, Mutation, sep = "", remove = FALSE)

mean_2hours <- CFTR2_data %>%
  filter(`Chase Time` == 2) %>%
  group_by(Mutant_name, Domain, `Chase Time`) %>%
  summarise(
    mean_g = mean(`C/(C+B)`, na.rm = TRUE),
    mean_N1 = mean(`NBD1 25PK`, na.rm = TRUE),
    mean_N2 = mean(`NBD2 25PK`, na.rm = TRUE),
    mean_T1 = mean(`TMD1 25PK`, na.rm = TRUE),
    mean_T2L = mean(`TMD2 25PK Late`, na.rm = TRUE),
    mean_T2E = mean(`TMD2 25PK Early`, na.rm = TRUE)
    )

Graph <- ggplot(mean_2hours) +
            geom_point(mapping = aes(x = mean_N1, y = mean_T1, color = Domain), size = 5) +
            geom_smooth(mapping = aes(x = mean_N1, y = mean_T1), method = lm, se = FALSE, color = "black") +
            labs(x = "NBD1 N1a fragment (120' Chase)", y = "TMD1 N1a fragment (120' Chase)") +
            theme_bw()

Graph + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')