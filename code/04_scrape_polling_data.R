# https://www.wahlrecht.de/umfragen/index.htm

library(tidyverse)
library(lubridate)
library(rvest)


# Read main page
main_page <- "https://www.wahlrecht.de/umfragen/index.htm" %>% 
  read_html()

institutes <- main_page %>% 
  html_nodes(".in") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_remove("\\.htm")

# read institute-wise pages
in_13_l <- 
  str_c("https://www.wahlrecht.de/umfragen/", 
        institutes, 
        ".htm") %>% 
  set_names(institutes) %>%
  map(read_html)

# Extract, harmonize and bind institute tables
in_13_df <- in_13_l %>%
  map(~ .x %>% 
        html_node(".wilko") %>% 
        html_table(dec = ",", 
                   fill = T) %>% 
        as_tibble(.name_repair = "universal") %>%
        set_names(c("date_publish", .[1, ][-1])) %>% # unify 1st col name
        .[!(names(.) == "" | is.na(names(.)))] %>% # remove empty cols
        mutate(date_publish = dmy(date_publish, quiet = T)) %>% 
        filter(!is.na(date_publish))
      ) %>% 
  bind_rows(.id = "institute") 

default_method <- # Institutes always use the same Method
  tibble(institute = institutes, 
         method = c("F", rep("T", 5), "O", "O"))

# Further cleaning
in_13 <- in_13_df %>%
  filter(!str_detect(Zeitraum, "Bundestagswahl")) %>% # remove rows with election outcomes
  separate(Zeitraum,
           into = c("date_start", "date_end"),
           sep = "–") %>% 
  mutate_at(c("date_start", "date_end"), 
            ~ .x %>% 
              str_c(year(date_publish)) %>% 
              dmy() %>% 
              if_else(. > date_publish, . - years(1), .) # If surveys are fielded in previous year, make sure year is correct
            ) %>% 
  mutate_if(~ any(str_detect(.x, "%")), # Convert percentages to numeric
            ~ .x %>% 
              str_replace(",", ".") %>% 
              str_remove("%") %>% 
              as.double()
            ) %>% 
  left_join(default_method) %>% 
  mutate(n = Befragte %>% 
           str_extract("[\\d\\.]+") %>%
           str_remove("\\.") %>% 
           as.integer()) %>% 
  filter(!(is.na(n) & Befragte != "?")) %>% 
  select(-Befragte) %>% 
  rename(union = "CDU/CSU", spd = SPD, greens = "GRÜNE", fdp = FDP, 
         left = LINKE, afd = AfD, others = Sonstige, 
         nonvoter_undecided = "Nichtwähler/Unentschl.", pirates = PIRATEN, 
         fw = FW)

write_rds(in_13, "data/polling_data.rds", compress = "gz")


# -------------------------------------------------------------------------
in_13 %>% 
  select(-nonvoter_undecided) %>% 
  gather("party", "vote_share", union:others, pirates:fw) %>% 
  filter(!is.na(vote_share), date_start > dmy("1.1.2020")) %>% 
  ggplot(aes(x = date_end, y = vote_share, col = party)) + 
  geom_linerange(aes(xmin = date_start, xmax = date_end), alpha = .5, size = 1.5) +
  facet_wrap(~ institute)
