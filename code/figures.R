#' figures.R
#'
#' What this file does:
#' Create your main figures in this script
#'

# --- Instal libraries --- #

install.packages("ggpubr")


# --- Load Libraries --- #
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(textstem)
library(vader)
library(lubridate)
library(stm)
library(tokenizers)
library(ggplot2)
library(gapminder)
library(ggpubr)
library(googledrive)


# --- Load Data --- #

TV_COMBINED_STATE_WW_PLOT <- read_rds("output/TV_COMBINED_STATE_WW_PLOT.rds")

# --- TV_COMBINED_STATE_WW_PLOT --- #

A <- TV_COMBINED_STATE_WW_PLOT %>%
    ggplot(aes(x = Date, 
               y = pos_neg_ratio,
               color = AVG_Sentiment)
    ) +
    geom_line() +
    theme_bw() +
    geom_vline(xintercept = as.POSIXct(as.Date("2021-05-09")), linetype=4)

saveRDS(A, file="output/A.rds")
ggsave("output/A.pdf")

# --- TV_COMBINED_SECRET_WW_PLOT --- #

TV_COMBINED_SECRET_WW_PLOT <- read_rds("output/TV_COMBINED_SECRET_WW_PLOT.rds")

B <- TV_COMBINED_SECRET_WW_PLOT %>%
    ggplot(aes(x = Date, 
               y = pos_neg_ratio,
               color = AVG_Sentiment)
    ) +
    geom_line() +
    theme_bw() +
    geom_vline(xintercept = as.POSIXct(as.Date("2021-05-09")), linetype=4)
    



saveRDS(B, file="output/B.rds")
ggsave("output/B.pdf")


# --- TV_COMBINED_ALIEN_WW_PLOT --- #

TV_COMBINED_ALIEN_WW_PLOT <- read_rds("output/TV_COMBINED_ALIEN_WW_PLOT.rds")

C <- TV_COMBINED_ALIEN_WW_PLOT %>%
    ggplot(aes(x = Date, 
               y = pos_neg_ratio,
               color = AVG_Sentiment)
    ) +
    geom_line() +
    theme_bw() +
    geom_vline(xintercept = as.POSIXct(as.Date("2021-05-09")), linetype=4)

saveRDS(C, file="output/C.rds")
ggsave("output/C.pdf")


#Combining for better representation


PRODUCTFITMODEL <- ggarrange(A, B, C,
    labels = c("State Farm:No Fit","Secret lab:fit","Alienware:fit"),
    ncol = 2, nrow = 2)

PRODUCTFITMODEL2 <- annotate_figure(PRODUCTFITMODEL,
                    top = text_grob("Visualizing the difference in effect of product fit on sentiment",
                                    color = "black", face = "bold", size = 14),
                    left = text_grob("Dotted line is the start of MSI", color = "blue", rot = 90, face = "italic", size = 12),
                    right = text_grob("Table 1, Right to left: Graph 1A/1B/1C", color = "blue", rot = 90, face = "italic", size = 12))

PRODUCTFITMODEL2

saveRDS(PRODUCTFITMODEL2, file="output/PRODUCTFITMODEL2.rds")
ggsave("output/PRODUCTFITMODEL2.pdf")


