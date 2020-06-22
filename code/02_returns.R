library(tidyverse)
library(readxl)


# German Federal Elections District Level Data ----------------------------
# Goal: Unify data for all federal elections in one long, tidy data set.

# Source: https://www.bundeswahlleiter.de/dam/jcr/ce2d2b6a-f211-4355-8eea-355c98cd4e47/btw_kerg.zip


# 0. To Do ----------------------------------------------------------------

# - Include 1949 election (although different electoral system)

# 1.1 Read and clean 1953 - 2002 -------------------------------------------

state_codes <- tibble(state_no = c(901L, 902L, 903L, 904L, 905L, 906L, 907L, 908L, 909L, 
                                       910L, 999L, 911L, 912L, 913L, 914L, 915L, 916L),
                      state = c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY",
                               "SL", "Bund", "BE", "MV", "BB", "ST", "TH", "SN"))

fls1 <- list.files("data/btw_kerg", full.names = T)[2:15]

# Import headers separately 
h1 <- fls1 %>% 
  map(~.x %>% 
         read.csv2(F, skip = 5, nrows = 2, fileEncoding = "latin1", stringsAsFactors = T) %>%
         t() %>%
         as_tibble(.name_repair = "unique") %>%
         set_names(c("party", "vote_type")) %>% 
         mutate(key = str_c("X", 1:n())) %>% 
        slice(-(1:3))
        )

# Import data
d1 <- fls1 %>% 
  map(~ .x %>% 
        read_csv2(skip = 7, 
                  col_names = F,
                  locale = locale(encoding = "latin1")) %>% 
        rename(district_no = X1, district_name = X2, state = X3) %>% 
        filter(!is.na(district_name)) %>% 
        gather("key", "votes", -(1:3)) %>% 
        mutate_at(c("district_no", "votes"), as.integer) %>% 
        mutate_at("state", as.character) %>% 
        mutate(year = str_extract(.x, "[0-9]+"))
        )


# Join header data and bind
btw1 <- d1 %>% 
  map2(h1, 
       ~.x %>% 
         left_join(.y, "key")
       ) %>% 
  bind_rows() %>% 
  mutate(state = ifelse(state %in% 901:916, 
                        stringi::stri_replace_all_regex(state, 
                                                        state_codes$state_no, 
                                                        state_codes$state, 
                                                        F),
                        state),
         district_name = str_replace_all(district_name, "\u0096", "-")
  ) %>% 
  select(-key)


# 1.2 Read and clean 2005-2017 ---------------------------------------------


enc <- c(rep("latin1", 3), "UTF-8")

# Separate header import
h3 <- list.files("data/btw_kerg", full.names = T)[16:19] %>% 
  map2(enc, ~.x %>% 
         read.csv2(F, skip = 5, nrows = 3, fileEncoding = .y, stringsAsFactors = T) %>%
         t() %>% 
         as_tibble(.name_repair = "unique") %>%
         set_names(c("party", "vote_type", "time")) %>%
         mutate_all(~ na_if(.x, "")) %>% 
         fill(party, vote_type, time) %>%
         mutate(key = str_c("X", 1:n())) %>% 
         slice(-(1:3))
  )

# Data import
d3 <- list.files("data/btw_kerg", full.names = T)[16:19] %>% 
  map2(enc, 
       ~ .x %>% 
         read_csv2(skip = 8, 
                   col_names = F,
                   locale = locale(encoding = .y)) %>% 
         rename(district_no = X1, district_name = X2, state_code = X3) %>% 
         filter(!is.na(district_name)) %>% 
         gather("key", "votes", -(1:3)))

state_codes2 <- 
tibble(state =      c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "MV", "BB", "ST", "TH", "SN", "Bund"),
       state_code = c(1,    2,    3,    4,    5,    6,    7,    8,    9,    10,   11,   13,   12,   15,   16,   14,   99))

# Join header data
btw3 <- 
  d3 %>%
  map2(h3, 
       ~ .x %>% 
         left_join(.y) %>% 
         select(-key)) %>% 
  set_names(c(2005, 2009, 2013, 2017)) %>% 
  bind_rows(.id = "year") %>% 
  filter(time != "Vorperiode", 
         !(party == "Wahlberechtigte" & vote_type == "Zweitstimmen"), 
         !(party == "Wähler" & vote_type == "Zweitstimmen")) %>% 
  left_join(state_codes2) %>% 
  select(-time, -state_code)


# 2. Unify and further clean data ------------------------------------------
sdn <- 
  tibble(
    district_no = c(999L, 999L, 999L, 901L, 913L, 902L, 903L, 904L, 912L, 915L, 
                    911L, 905L, 914L, 906L, 916L, 907L, 909L, 908L, 910L, 999L), 
    district_name = c("Bundesgebiet ohne Berlin", "BUNDESGEBIET OHNE BERLIN", 
                     "Bundesgebiet", "Schleswig-Holstein", 
                     "Mecklenburg-Vorpommern", "Hamburg", "Niedersachsen", 
                     "Bremen", "Brandenburg", "Sachsen-Anhalt", "Berlin", 
                     "Nordrhein-Westfalen", "Sachsen", "Hessen", "Thüringen", 
                     "Rheinland-Pfalz", "Bayern", "Baden-Württemberg", 
                     "Saarland", "Bundesgebiet")
    )

# Party name synonyms
pns <- 
  read_csv2("data/party_names.csv") %>% 
  bind_cols(.[1:3]) %>% 
  gather("key", "alt_names", -(1:3)) %>% 
  select(-key) %>% 
  filter(!is.na(alt_names))


btw_returns <- bind_rows(btw1, btw3) %>%
  filter(!is.na(party),
         votes != 0,
         !is.na(votes)) %>% 
  mutate(state = ifelse(district_no %in% state_codes$state_no,
                        stringi::stri_replace_all_regex(district_no, 
                                                        state_codes$state_no, 
                                                        state_codes$state, 
                                                        F),
                        state),
         state = ifelse(district_no == 99 & district_name == "Bundesgebiet",
                        "Bund",
                        state),
         district_no = ifelse(district_name %in% sdn$district_name,
                              stringi::stri_replace_all_regex(district_name, 
                                                              sdn$district_name, 
                                                              sdn$district_no, 
                                                              F),
                              district_no),
         votes = as.integer(votes), 
         vote_type = ifelse(party %in% c("Wahlberechtigte", "Wähler"), "", vote_type)
         ) %>% 
  left_join(pns, by = c("party" = "alt_names")) %>% 
  select(year, district_no, district_name, state, party_name_short, party_wiki_id, vote_type, votes)

# Export
write_rds(btw_returns, "btw_returns.rds", compress = "gz")
write_csv(btw_returns, "btw_returns.csv")



