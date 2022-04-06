# Ladda bibliotek
library(readxl)
library(dplyr)
library(purrr)

LasInStatligaMyndigheter <- function() {
  # Listor på statliga myndigheter kan hämtas på: http://www.myndighetsregistret.scb.se/Myndighet
  # Det finns sex olika typer av myndigheter och de måste laddas ner alla var för
  # sig. Utlandsmyndigheter behöver inte laddas ned, och tas bort om den finns med
  # i mappen. Lägga alla fem (eller sex om utlandsmyndighetrer är med) i mappen
  # som listas nedan. Därifrån läses de in av skriptet.
  
  # Importera Excelfil
  url <- "G:/Samhällsanalys/Statistik/StatligaJobb/Import_myndigheter"
  filetype <- "*.xlsx"
  file.list <- list.files(path = url, pattern=filetype, full.names = TRUE)
  # Tar bort utlandsmyndigheter
  file.list <- purrr::discard(file.list,.p = ~stringr::str_detect(.x,"utland"))
  file.list <- setNames(file.list, file.list)  #Skapar en kolumn med filnamn
  myndigheter_df <- map_df(file.list, read_excel, .id= "id")
  # Byt ut hela sökvägen mot enbart filnamn, döp om kol från id till filnamn
  myndigheter_df$id <- basename(myndigheter_df$id) 
  myndigheter_df <- myndigheter_df %>% rename(filnamn = id) %>% 
    select(-Tfn, -Fax, -Epost, -Webbadress, -SFS, -Sort)
myndigheter_df
}