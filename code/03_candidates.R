library(tidyverse)
library(tabulizer)
library(rvest)

# BTW Candidates -----------------------------------------------------
# Goal: Get the names of all candidates for each federal election

# Source '17 (pdf): https://upload.wikimedia.org/wikipedia/de/d/da/Landeslisten_und_Wahlkreiskandidaten_2017.pdf
# Source '17 (html): https://web.archive.org/web/20170926042133/https://www.bundeswahlleiter.de/bundestagswahlen/2017/wahlbewerber/bund-99/a.html
# Source '13 (pdf): https://web.archive.org/web/20130921060913/http://www.bundeswahlleiter.de/de/bundestagswahlen/BTW_BUND_13/veroeffentlichungen/BTW13_Sonderheft_Internet.pdf
# Source '09 (pdf): https://web.archive.org/web/20091007000338if_/http://www.bundeswahlleiter.de:80/de/bundestagswahlen/BTW_BUND_09/wahlbewerber/wahlbewerber/tab10.pdf
# Source '05 (csv): https://web.archive.org/web/20050905162714if_/http://www.bundeswahlleiter.de/bundestagswahl2005/downloads/alphabetcsv.csv
# Source '02 (html): https://web.archive.org/web/20021218083353/http://www.bundeswahlleiter.de/bundestagswahl2002/deutsch/wahlbewerber2002/bund_land/uebersicht/btw2002/alpha_a_btw2002.htm


# 1. Import data by election ----------------------------------------------
dc <- list()

# 1.1 2002 ----------------------------------------------------------------
htm02 <- 
  str_c("https://web.archive.org/web/20021218083353/http://www.bundeswahlleiter.de/bundestagswahl2002/deutsch/wahlbewerber2002/bund_land/uebersicht/btw2002/alpha_", 
        c(letters[-(24:26)], "xyz"), 
        "_btw2002.htm"
        ) %>% 
  map(safely(read_html))

dc$y2002 <- htm02 %>% 
  map(~.x$result %>% 
        html_node(xpath = "/html/body/center/table/tr[2]/td[2]/center/table[2]") %>% 
        html_table(fill = T) %>%
        set_names(c("name", "yob", "party", "district", "liste")) %>% 
        slice(-1) %>% 
        as_tibble()
      ) %>% 
  bind_rows() %>%
  mutate(liste = str_replace(liste, "\xfc", "ü")) %>% 
  separate(liste, c("list_state", "list_position"), sep = "Platz") %>% 
  mutate_all(str_squish) %>% 
  filter(party != "")

# 1.2 2005 ----------------------------------------------------------------
dc$y2005 <- 
  read_csv2("data/btw05_wahlbewerber.csv", 
            locale = locale(encoding = "latin1")) %>% 
  set_names("name", "occupation", "yob", "birthplace", "address", "postal_code", 
            "residence", "party", "district", "list_state", "list_position") %>% 
  mutate(postal_code = formatC(postal_code, 4, format = "d", flag = 0),
         yob = as.character(yob),
         district = as.character(district),
         list_position = as.character(list_position))


# 1.3 2009 ----------------------------------------------------------------
dc09 <- extract_tables("data/btw09_wahlbewerber.pdf", 
                       area = list(c(120, 30, 730, 555))) 

dc$y2009 <- dc09 %>% 
  map(~ .x %>% 
        as_tibble(.name_repair = "unique") %>% 
        filter(!str_length(...1) %in% 0:1 &
                 !apply(., 1, function(row) {any(row == "Name, Vorname(n)")})) %>% 
        select_if(~sum(.x != "") != 0)
      ) %>%
  {.[[60]] <- .[[60]] %>% separate(...6, c("a", "b"), " ", fill = "left"); .} %>% 
  map(~ .x %>% 
        set_names(c("name", "yob", "party", "district", "list_state", "list_position"))) %>% 
  bind_rows()

x <- which(dc$y2009$party == "")
dc$y2009[x + 1, "name"] <- str_c(dc$y2009$name[x], dc$y2009$name[x + 1], sep = " ")
dc$y2009 <- dc$y2009[-x, ]
rm(x)


# 1.4 2013 ----------------------------------------------------------------

dc13 <- extract_tables("data/btw13_wahlbewerber.pdf", 
                     pages = 276:337, 
                     area = list(c(111, 33, 750, 205)), 
                     method = "stream")
dc$y2013 <- dc13 %>% 
  map(~ .x %>% 
        as_tibble(.name_repair = "unique") %>% 
        filter(!str_length(...1) %in% 0:1 &
                 !apply(., 1, function(row) {any(row == "Name, Vorname(n)")})) %>% 
        select_if(~sum(.x != "") != 0)  %>% 
        set_names(c("name", "yob", "party", "district", "list_state", "list_position"))
  ) %>% 
  bind_rows()


# 1.5 2017 ----------------------------------------------------------------
htm17 <- 
  str_c("https://web.archive.org/web/20170926042133/https://www.bundeswahlleiter.de/bundestagswahlen/2017/wahlbewerber/bund-99/",
        letters[-24], 
        ".html") %>% 
  map(safely(read_html))


dc$y2017 <- htm17 %>% 
  map(~.x$result %>% 
      html_node(".tablesaw") %>% 
        html_table() %>% 
        as_tibble()) %>% 
  bind_rows() %>% 
  set_names("name", "yob", "party", "candidacy") %>% 
  mutate(district = candidacy %>% 
           str_extract("Wahlkreis [0-9]+") %>% 
           str_extract("[0-9]+"),
         list_state = candidacy %>% 
           str_extract("Land \\D+ \\(") %>% 
           str_remove_all("(Land | \\()"),
         list_position = candidacy %>% 
           str_extract("\\d+\\)") %>% 
           str_remove("\\)"),
         yob = as.character(yob)) %>% 
  select(-candidacy)


# 2. Bind data ------------------------------------------------------------
cds <- read_table("
  state_code  state_name
  BB          Brandenburg     
  BE          Berlin                
  BW          Baden-Württemberg                
  BY          Bayern           
  HB          Bremen                
  HE          Hessen               
  HH          Hamburg                
  MV          Mecklenburg-Vorpommern
  NI          Niedersachsen         
  NW          Nordrhein-Westfalen   
  RP          Rheinland-Pfalz       
  SH          Schleswig-Holstein
  SL          Saarland               
  SN          Sachsen        
  ST          Sachsen-Anhalt    
  TH          Thüringen")

pns <- 
  read_csv2("data/party_names.csv") %>% 
  bind_cols(.[1:3]) %>% 
  gather("key", "alt_names", -(1:3)) %>% 
  select(-key) %>% 
  filter(!is.na(alt_names))


candidates <- dc %>%  
  bind_rows(.id = "year") %>% 
  mutate(year = year %>% 
           str_remove("y") %>% 
           as.integer(),
         list_state = stringi::stri_replace_all_fixed(list_state, 
                                                      cds$state_name, 
                                                      cds$state_code, 
                                                      F),
         list_state = na_if(list_state, "")) %>% 
  mutate_at(c("yob", "district", "list_position"), as.integer) %>% 
  left_join(pns, by = c("party" = "alt_names")) %>% 
  select(-party, -(address:residence))


# 3. Export ---------------------------------------------------------------
write_rds(candidates, "btw_candidates.rds", compress = "gz")
write_csv(candidates, "btw_candidates.csv")


