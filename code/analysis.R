#' analysis.R
#'
#' What this file does:
#' Do your main analysis in this script
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


# --- Analysis VADER --- #

### --- ALIENWARE TWITTER DATA --- ###

### -- Running VADER PRE EVENT DATA--- ###
T_PRE_ALIEN_WW_TIDY <- readRDS('output/T_PRE_ALIEN_WW_TIDY.rds')

V_PRE_ALIEN_WW <-
    vader_df(T_PRE_ALIEN_WW_TIDY$text)

V_PRE_ALIEN_WW_PREP <-
    V_PRE_ALIEN_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_PRE_ALIEN_WW_IV <- V_PRE_ALIEN_WW_PREP %>% #book
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.integer(id))

### --- Combining classification with the T_PRE_ALIEN_TIDY --- ###

TV_PRE_ALIEN_TIDY <-
    T_PRE_ALIEN_WW_TIDY %>%
    mutate(id = as.integer(id)) %>%
    inner_join(V_PRE_ALIEN_WW_IV, by = "id")

### --- Group and counting --- ###

TV_PRE_ALIEN_WW_PLOT <- 
    TV_PRE_ALIEN_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- creating the mean-compound values --- ###
TV_PRE_ALIEN_TEST <- V_PRE_ALIEN_WW_PREP %>%
    select(id,compound)

T_PRE_ALIEN_WW_TIDY_DATE_ID <- T_PRE_ALIEN_WW_TIDY %>%
    select(id, Date)

TV_PRE_ALIEN_WW_AVG <- T_PRE_ALIEN_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_ALIEN_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))

TV_PRE_ALIEN_WW_PLOT2 <- TV_PRE_ALIEN_WW_PLOT %>%
    inner_join(TV_PRE_ALIEN_WW_AVG, by = "Date")

saveRDS(TV_PRE_ALIEN_WW_PLOT2, file='output/TV_PRE_ALIEN_WW_PLOT2.rds')

############AFTER###############
T_AFT_ALIEN_WW_TIDY <- readRDS('output/T_AFT_ALIEN_WW_TIDY.rds')


### -- Running VADER --- ###

V_AFT_ALIEN_WW <-
    vader_df(T_AFT_ALIEN_WW_TIDY$text)

V_AFT_ALIEN_WW_PREP <-
    V_AFT_ALIEN_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_AFT_ALIEN_WW_IV <- V_AFT_ALIEN_WW_PREP %>% #book
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.integer(id))

### --- Combining classification with the T_AFT_ALIEN_TIDY --- ###

TV_AFT_ALIEN_TIDY <-
    T_AFT_ALIEN_WW_TIDY %>%
    mutate(id = as.integer(id)) %>%
    inner_join(V_PRE_ALIEN_WW_IV, by = "id")

### --- Group and counting --- ###

TV_AFT_ALIEN_WW_PLOT <- 
    TV_AFT_ALIEN_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- creating the mean-compound values --- ###
TV_AFT_ALIEN_TEST <- V_AFT_ALIEN_WW_PREP %>%
    select(id,compound)

T_AFT_ALIEN_WW_TIDY_DATE_ID <- T_AFT_ALIEN_WW_TIDY %>%
    select(id, Date)

TV_AFT_ALIEN_WW_AVG <- T_AFT_ALIEN_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_ALIEN_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))

TV_AFT_ALIEN_WW_PLOT2 <- TV_AFT_ALIEN_WW_PLOT %>%
    inner_join(TV_AFT_ALIEN_WW_AVG, by = "Date")

saveRDS(TV_AFT_ALIEN_WW_PLOT2, file='output/TV_AFT_ALIEN_WW_PLOT2.rds')

###---combining to create 1 data set ---###

TV_COMBINED_ALIEN_WW_PLOT2 <-
    rbind(TV_PRE_ALIEN_WW_PLOT2, TV_AFT_ALIEN_WW_PLOT2)

TV_COMBINED_ALIEN_WW_PLOT <-
    rename(TV_COMBINED_ALIEN_WW_PLOT2, AVG_Sentiment = mean_compound)

saveRDS(TV_COMBINED_ALIEN_WW_PLOT, file='output/TV_COMBINED_ALIEN_WW_PLOT.rds')

#could probably saved time by merging the 2 data sets at the start


### --- STATE FARM TWITTER DATA --- ###

# --- Analysis VADER --- #

### -- Running VADER PRE EVENT DATA--- ###

T_PRE_STATE_WW_TIDY <- read_rds('output/T_PRE_STATE_WW_TIDY.rds')

#Should be ready now for vader, V for vader 
V_PRE_STATE_WW <-
    vader_df(T_PRE_STATE_WW_TIDY$text)

V_PRE_STATE_WW_PREP <-
    V_PRE_STATE_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_PRE_STATE_WW_IV <- V_PRE_STATE_WW_PREP %>% 
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.integer(id))

### --- Combining classification with the T_PRE_STATE_TIDY --- ###

TV_PRE_STATE_TIDY <-
    T_PRE_STATE_WW_TIDY %>%
    mutate(id = as.integer(id)) %>%
    inner_join(V_PRE_STATE_WW_IV, by = "id")

### --- Group and counting --- ###

TV_PRE_STATE_WW_PLOT <- 
    TV_PRE_STATE_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- Creating the mean-compound values --- ###

TV_PRE_STATE_TEST <- V_PRE_STATE_WW_PREP %>%
    select(id,compound)

T_PRE_STATE_WW_TIDY_DATE_ID <- T_PRE_STATE_WW_TIDY %>%
    select(id, Date)

TV_PRE_STATE_WW_AVG <- T_PRE_STATE_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_STATE_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))

TV_PRE_STATE_WW_PLOT2 <- TV_PRE_STATE_WW_PLOT %>%
    inner_join(TV_PRE_STATE_WW_AVG, by = "Date")

saveRDS(TV_PRE_STATE_WW_PLOT2, file='output/TV_PRE_STATE_WW_PLOT2.rds')


##############AFTER##########
T_AFT_STATE_WW_TIDY <- readRDS('output/T_AFT_STATE_WW_TIDY.rds')

#Should be ready now for vader, V for vader 
V_AFT_STATE_WW <-
    vader_df(T_AFT_STATE_WW_TIDY$text)

V_AFT_STATE_WW_PREP <-
    V_AFT_STATE_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_AFT_STATE_WW_IV <- V_AFT_STATE_WW_PREP %>% #book
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.integer(id))

### --- Combining classification with the T_AFT_STATE_TIDY --- ###

TV_AFT_STATE_TIDY <-
    T_AFT_STATE_WW_TIDY %>%
    mutate(id = as.integer(id)) %>%
    inner_join(V_PRE_STATE_WW_IV, by = "id")

### --- Group and counting --- ###

TV_AFT_STATE_WW_PLOT <- 
    TV_AFT_STATE_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- creating the mean-compound values --- ###
TV_AFT_STATE_TEST <- V_AFT_STATE_WW_PREP %>%
    select(id,compound)

T_AFT_STATE_WW_TIDY_DATE_ID <- T_AFT_STATE_WW_TIDY %>%
    select(id, Date)

TV_AFT_STATE_WW_AVG <- T_AFT_STATE_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_STATE_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))

TV_AFT_STATE_WW_PLOT2 <- TV_AFT_STATE_WW_PLOT %>%
    inner_join(TV_AFT_STATE_WW_AVG, by = "Date")

saveRDS(TV_AFT_STATE_WW_PLOT2, file='output/TV_AFT_STATE_WW_PLOT2.rds')

###---combining to create 1 data set ---###

TV_COMBINED_STATE_WW_PLOT2 <-
    rbind(TV_PRE_STATE_WW_PLOT2, TV_AFT_STATE_WW_PLOT2)

TV_COMBINED_STATE_WW_PLOT <-
    rename(TV_COMBINED_STATE_WW_PLOT2, AVG_Sentiment = mean_compound)


saveRDS(TV_COMBINED_STATE_WW_PLOT, file='output/TV_COMBINED_STATE_WW_PLOT.rds')


### --- SECRET LABS TWITTER DATA --- ###

T_PRE_SECRET_WW <- readRDS('output/T_PRE_SECRET_WW_TIDY.rds')

#Should be ready now for vader, V for vader 
V_PRE_SECRET_WW <-
    vader_df(T_PRE_STATE_WW_TIDY$text)

V_PRE_SECRET_WW_PREP <-
    V_PRE_SECRET_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_PRE_SECRET_WW_IV <- V_PRE_SECRET_WW_PREP %>% #book
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.character(id))

### --- Combining classification with the T_PRE_SECRET_TIDY --- ###

TV_PRE_SECRET_TIDY <-
    T_PRE_SECRET_WW_TIDY %>%
    mutate(id = as.character(id)) %>%
    inner_join(V_PRE_SECRET_WW_IV, by = "id")

### --- Group and counting --- ###

TV_PRE_SECRET_WW_PLOT <- 
    TV_PRE_SECRET_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- creating the mean-compound values --- ###
TV_PRE_SECRET_TEST <- V_PRE_SECRET_WW_PREP %>%
    select(id,compound)

T_PRE_SECRET_WW_TIDY_DATE_ID <- T_PRE_SECRET_WW_TIDY %>%
    select(id, Date)

TV_PRE_SECRET_WW_AVG <- T_PRE_SECRET_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_SECRET_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))


TV_PRE_SECRET_WW_PLOT2 <- TV_PRE_SECRET_WW_PLOT %>%
    inner_join(TV_PRE_SECRET_WW_AVG, by = "Date")

saveRDS(TV_PRE_SECRET_WW_PLOT2, file='output/TV_PRE_SECRET_WW_PLOT2.rds')

############AFTER###############

T_AFT_SECRET_WW <- readRDS('output/T_AFT_SECRET_WW_TIDY.rds')

#Should be ready now for vader, V for vader 
V_AFT_SECRET_WW <-
    vader_df(T_AFT_SECRET_WW_TIDY$text)

V_AFT_SECRET_WW_PREP <-
    V_AFT_SECRET_WW %>%
    rowid_to_column("id") %>% 
    filter(word_scores != 'ERROR')

V_AFT_SECRET_WW_IV <- V_AFT_SECRET_WW_PREP %>% #book
    mutate(vader_class = case_when(compound < -0.05 ~ "negative",
                                   compound > 0.05 ~ "positive",
                                   TRUE ~ "neutral")) %>%
    select(id,vader_class) %>%
    mutate(id = as.integer(id))

### --- Combining classification with the T_AFT_SECRET_TIDY --- ###

TV_AFT_SECRET_TIDY <-
    T_AFT_SECRET_WW_TIDY %>%
    mutate(id = as.character(id)) %>%
    inner_join(V_PRE_SECRET_WW_IV, by = "id")

TV_AFT_SECRET_WW_PLOT <- 
    TV_AFT_SECRET_TIDY %>%
    group_by(Date, vader_class) %>%
    count() %>%
    ungroup %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

### --- creating the mean-compound values --- ###
TV_AFT_SECRET_TEST <- V_AFT_SECRET_WW_PREP %>%
    select(id,compound)

T_AFT_SECRET_WW_TIDY_DATE_ID <- T_AFT_SECRET_WW_TIDY %>%
    select(id, Date)

TV_AFT_SECRET_WW_AVG <- T_AFT_SECRET_WW_TIDY_DATE_ID %>%
    mutate(id = as.integer(id)) %>%
    inner_join(TV_PRE_SECRET_TEST, by = "id") %>%
    group_by(Date)%>%summarize(mean_compound=mean(compound))

#there is probably a more efficient way to do this, 
#this was the way I know.

TV_AFT_SECRET_WW_PLOT2 <- TV_AFT_SECRET_WW_PLOT %>%
    inner_join(TV_AFT_ALIEN_WW_AVG, by = "Date")

saveRDS(TV_AFT_SECRET_WW_PLOT2, file='output/TV_AFT_SECRET_WW_PLOT2.rds')

###---combining to create 1 data set ---###

TV_COMBINED_SECRET_WW_PLOT2 <-
    rbind(TV_PRE_SECRET_WW_PLOT2, TV_AFT_SECRET_WW_PLOT2)

TV_COMBINED_SECRET_WW_PLOT <-
    rename(TV_COMBINED_SECRET_WW_PLOT2, AVG_Sentiment = mean_compound)

saveRDS(TV_COMBINED_SECRET_WW_PLOT, file='output/TV_COMBINED_SECRET_WW_PLOT.rds')




