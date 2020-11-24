##########libraries#############


library(tidyverse) 
library(readxl)
library(here)
library(zoo)


##########1#############

responses_1 <- read_xlsx("raw_data/Reaesrch project - questionnaire responses (anon).xlsx",
                        sheet = "Pre Section 1 (About you)",
                        col_names = TRUE) %>% 
    select(-1) %>% 
    head(-1)


colnames(responses_1)[str_detect(colnames(responses_1),"\\.\\.\\.")] <- NA

colnames(responses_1) <- colnames(responses_1) %>% 
    zoo::na.locf(na.rm = FALSE) 

names(responses_1) <- paste(names(responses_1), responses_1[1, ], sep = "_")

tidy_data_1 <-
    responses_1 %>% 
    slice(-1) %>% 
    rowid_to_column("survey_id") %>%
    pivot_longer(
        cols = -survey_id,
        names_to = c("question"
                     , "answer"
        ),
        names_sep = "_",
        values_to = "value",
        values_drop_na = TRUE
    ) %>% 
    mutate(survey_time = "pre",
           survey_set = "about_you") %>%
    relocate(c(survey_time, survey_set), .after = survey_id) %>% 
    filter(value == 1) %>% 
    select(-value)



##########2#############

responses_2 <- read_xlsx("raw_data/Reaesrch project - questionnaire responses (anon).xlsx",
                        sheet = "Pre Section 2 (Your beliefs)",
                        col_names = TRUE) %>% 
    select(-1) %>% 
    head(-1)


colnames(responses_2)[str_detect(colnames(responses_2),"\\.\\.\\.")] <- NA

colnames(responses_2) <- colnames(responses_2) %>% 
    zoo::na.locf(na.rm = FALSE) 

names(responses_2) <- paste(names(responses_2), responses_2[1, ], sep = "_")

tidy_data_2<- responses_2 %>% 
    slice(-1) %>% 
    rowid_to_column("survey_id") %>%
    pivot_longer(
        cols = -survey_id,
        names_to = c("question"
                     , "answer"
        ),
        names_sep = "_",
        values_to = "value",
        values_drop_na = TRUE
    ) %>% 
    mutate(survey_time = "pre",
           survey_set = "your_beliefs") %>%
    relocate(c(survey_time, survey_set), .after = survey_id) %>% 
    filter(value == 1) %>% 
    select(-value) %>% 
    view()


##########3#############

responses_3 <- read_xlsx("raw_data/Reaesrch project - questionnaire responses (anon).xlsx",
                        sheet = "Post Section 1 (About you)",
                        col_names = TRUE) %>% 
    select(-1) %>% 
    head(-1)


colnames(responses_3)[str_detect(colnames(responses_3),"\\.\\.\\.")] <- NA

colnames(responses_3) <- colnames(responses_3) %>% 
    zoo::na.locf(na.rm = FALSE) 

names(responses_3) <- paste(names(responses_3), responses_3[1, ], sep = "_")

tidy_data_3 <- responses_3 %>% 
    slice(-1) %>% 
    rowid_to_column("survey_id") %>%
    pivot_longer(
        cols = -survey_id,
        names_to = c("question"
                     , "answer"
        ),
        names_sep = "_",
        values_to = "value",
        values_drop_na = TRUE
    ) %>% 
    mutate(survey_time = "post",
           survey_set = "about_you") %>%
    relocate(c(survey_time, survey_set), .after = survey_id) %>% 
    filter(value == 1) %>% 
    select(-value)


##########4#############

responses_4 <- read_xlsx("raw_data/Reaesrch project - questionnaire responses (anon).xlsx",
                         sheet = "Post Section 2 (Your beliefs)",
                         col_names = TRUE) %>% 
    select(-1) %>% 
    head(-1) %>% 
    view()


colnames(responses_4)[str_detect(colnames(responses_4),"\\.\\.\\.")] <- NA

colnames(responses_4) <- colnames(responses_4) %>% 
    zoo::na.locf(na.rm = FALSE) 

names(responses_4) <- paste(names(responses_4), responses_4[1, ], sep = "_")

tidy_data_4<- responses_4 %>% 
    slice(-1) %>% 
    rowid_to_column("survey_id") %>%
    pivot_longer(
        cols = -survey_id,
        names_to = c("question"
                     , "answer"
        ),
        names_sep = "_",
        values_to = "value",
        values_drop_na = TRUE
    ) %>% 
    mutate(survey_time = "post",
           survey_set = "your_beliefs") %>%
    relocate(c(survey_time, survey_set), .after = survey_id) %>% 
    filter(value == 1) %>% 
    select(-value)

######Combined data#####

rbind(tidy_data_1, tidy_data_2, tidy_data_3, tidy_data_4) %>% 
    # view() %>% 
        pivot_wider(names_from = question, values_from = answer) %>% 
    unnest(-survey_id:-survey_set) %>% 
    write_csv("clean_data/summary_data_wide.csv")
    
# test_1 <-tidy_data_1 %>% 
#     # view() %>% 
#     pivot_wider(names_from = question, values_from = answer) %>% 
#     unnest(-survey_id:-survey_set) %>% 
#     view()
# 
# 
# test_2 <-tidy_data_2 %>% 
#     # view() %>% 
#     pivot_wider(names_from = question, values_from = answer) %>% 
#     unnest(-survey_id:-survey_set) %>% 
#     view()
# 
# test_3 <-tidy_data_3 %>% 
#     # view() %>% 
#     pivot_wider(names_from = question, values_from = answer) %>% 
#     unnest(-survey_id:-survey_set) %>% 
#     view()
# 
# test_4 <-tidy_data_4 %>% 
#     # view() %>% 
#     pivot_wider(names_from = question, values_from = answer) %>% 
#     unnest(-survey_id:-survey_set) %>% 
#     view()