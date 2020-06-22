library(tidyverse)

# Download data -----------------------------------------------------------

read_table("
  file_name                    url
  data/btw13_wahlbewerber.pdf  https://web.archive.org/web/20130921060913/http://www.bundeswahlleiter.de/de/bundestagswahlen/BTW_BUND_13/veroeffentlichungen/BTW13_Sonderheft_Internet.pdf
  data/btw09_wahlbewerber.pdf  https://web.archive.org/web/20091007000338if_/http://www.bundeswahlleiter.de:80/de/bundestagswahlen/BTW_BUND_09/wahlbewerber/wahlbewerber/tab10.pdf
  data/btw05_wahlbewerber.csv  https://web.archive.org/web/20050905162714if_/http://www.bundeswahlleiter.de/bundestagswahl2005/downloads/alphabetcsv.csv
  data/btw_kerg.zip            https://www.bundeswahlleiter.de/dam/jcr/ce2d2b6a-f211-4355-8eea-355c98cd4e47/btw_kerg.zip") %>% 
  {map2(.$file_name, .$url, ~ download.file(.y, .x, mode = "wb"))}


unzip("data/btw_kerg.zip", exdir = "data/btw_kerg/")



