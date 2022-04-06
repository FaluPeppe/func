library(janitor)
library(tidyr)
library(stringi)
library(openxlsx)

# Funktion för att läsa in Excel-filer och flikar 
LasInFilerOchFlikar2 <- function(url_xlsx) {
  starttid <- Sys.time()           # för att mäta hur lång tid det tar
  print(paste0("Läser in ", length(url_xlsx), " filer till en lista med dataframes"))
  # dataset är listan som samlar alla flikar från alla filer
  dataset <- list()
  fliknamn <- NA
  # formulär är variabeln som vi räknar formulär med
  formular <- 0
  
  # skapa progress bar 
  pb <- txtProgressBar(min = 1, max = length(url_xlsx), style = 3)
  
  # Här bläddrar vi genom alla filer i fillistan och laddar in i dataframe
  # dataset
  
  for (i in 1:length(url_xlsx)){                                           
    tryCatch({
      # Skapa lista med med flikarna som finns i aktuell flik (1 flik eller fler)
      sheet <- getSheetNames(url_xlsx[i])
      
      
      # Bläddra igenom "sheet", listan som innehåller alla flikar (minst en)
      for (j in 1:length(sheet)){ 
        # Läs in aktuell flik (j) i aktuell fil (i)
        tmp <- read.xlsx(url_xlsx[i], sheet=j)
        # Kontrollera om fliken har fler än 100 rader

        # stega fram ett steg för varje formulär vi lägger till i dataset
        formular <- formular + 1
        # vi lägger in aktuell flik i den fil vi jobbar med i dataset
        dataset[[formular]] <- tmp
        names(dataset)[formular] <- sheet[j]
        #fliknamn[[formular]] <- sheet[j]
      
        setTxtProgressBar(pb, formular)     # uppdatera progress bar
        # Vi stänger for-loopen som stegar igenom alla flikar (j) i aktuell fil (i)
      }
      # slut på tryCatch-loopen
    },
    error = function(e) {print(paste0("Formulär ", formular, " som heter ", url_xlsx[i], " gick inte att läsa in."))}
    )
    
    # sen stänger vi loopen som stegar igenom alla filer (i)
  }
  sluttid <- Sys.time()
  kortid <- sluttid - starttid
  print(paste0("Det tog ", round(difftime(sluttid, starttid, units = "min"),2) , " minuter att läsa in ", formular, " formulär från ", length(url_xlsx), " filer. I genomsnitt tog det ", round(difftime(sluttid, starttid, units = "secs")/ formular,2), " sekunder per formulär."))
  close(pb)
  dataset
  #retur <- list("dataset" = dataset, "fliknamn" = fliknamn)
}

