#' data_cleaning.R
#'
#' What this file does:
#' Do any data cleaning in this script
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

# --- Data Cleaning --- #

### --- ALIENWARE TWITTER DATA --- ###

# --- wORLDWIDE --- #

# --- Pre-event ---#
T_PRE_ALIEN_WW <- read_rds('data/MILEDATA/T_PRE_ALIEN_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_PRE_ALIEN_WW_SELECT <-
    T_PRE_ALIEN_WW %>% filter(is_retweet == "FALSE") %>% 
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)

#Erasing text that has no meaning
T_PRE_ALIEN_WW_TIDY <- 
    T_PRE_ALIEN_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_PRE_ALIEN_WW_TIDY, file="output/T_PRE_ALIEN_WW_TIDY.rds")

####------ AFTER ------####

T_AFT_ALIEN_WW <- read_rds('data/MILEDATA/T_AFT_ALIEN_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_AFT_ALIEN_WW_SELECT <-
    T_AFT_ALIEN_WW %>% filter(is_retweet == "FALSE") %>% 
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)

#Erasing text that has no meaning
T_AFT_ALIEN_WW_TIDY <- 
    T_AFT_ALIEN_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_AFT_ALIEN_WW_TIDY, file="output/T_AFT_ALIEN_WW_TIDY.rds")




### --- STATE FARM TWITTER DATA --- ###

T_PRE_STATE_WW <- read_rds('data/MILEDATA/T_PRE_STATE_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_PRE_STATE_WW_SELECT <-
    T_PRE_STATE_WW %>% filter(is_retweet == "FALSE") %>% 
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)

#Erasing text that has no meaning
T_PRE_STATE_WW_TIDY <- 
    T_PRE_STATE_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_PRE_STATE_WW_TIDY, file="output/T_PRE_STATE_WW_TIDY.rds")

############AFTER###############
T_AFT_STATE_WW <- read_rds('data/MILEDATA/T_AFT_STATE_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_AFT_STATE_WW_SELECT <-
    T_AFT_STATE_WW %>% filter(is_retweet == "FALSE") %>% 
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)

#Erasing text that has no meaning
T_AFT_STATE_WW_TIDY <- 
    T_AFT_STATE_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_AFT_STATE_WW_TIDY, file="output/T_AFT_STATE_WW_TIDY.rds")



### --- SECRET LABS TWITTER DATA --- ###

T_PRE_SECRET_WW <- read_rds('data/MILEDATA/T_PRE_SECRET_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_PRE_SECRET_WW_SELECT <-
    T_PRE_SECRET_WW %>% filter(is_retweet == "FALSE") %>% 
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)


#Erasing text that has no meaning
T_PRE_SECRET_WW_TIDY <- 
    T_PRE_SECRET_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_PRE_SECRET_WW_TIDY, file="output/T_PRE_SECRET_WW_TIDY.rds")

###################AFTER########################################

T_AFT_SECRET_WW <- read_rds('data/MILEDATA/T_AFT_SECRET_WW.rds')

#English only  +  deleting RT because of the insane amount of giveaway retweets
T_AFT_SECRET_WW_SELECT <-
    T_AFT_SECRET_WW %>% filter(is_retweet == "FALSE") %>% #deleted RT because of the insane amounth of giveaway retweets
    select(text, created_at,lang) %>%
    rownames_to_column("text_id") %>%
    filter(lang == "en") %>%
    mutate(Date = ymd_hms(as.character(created_at)),
           Date = floor_date(Date, unit = "day")) %>%
    select(text_id, text, Date)

#Erasing text that has no meaning
T_AFT_SECRET_WW_TIDY <- 
    T_AFT_SECRET_WW_SELECT %>%
    mutate(text = str_remove_all(text, "https\\S*"),
           text = str_remove_all(text, "http\\S*"),
           text = str_remove_all(text, "t.co*"),
           text = str_remove_all(text, "@\\S*"),
           text = str_remove_all(text, "amp"),
           text = str_remove_all(text, "&S*"),
           text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
           text = str_replace_all(text, "<a(.*?)>", " "),
           text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
           text = str_replace_all(text, "&#[:digit:]+;", " "),
           text = str_remove_all(text, "<[^>]*>"),
           text = str_remove_all(text, "[:digit:]"),
           text = str_squish(text),
           text = str_trim(text),
           text = str_remove_all(text, "RT")) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

saveRDS(T_AFT_SECRET_WW_TIDY, file="output/T_AFT_SECRET_WW_TIDY.rds")

