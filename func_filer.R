# funktioner för att hantera filer och filnamn samt mappar

sparafil_unik <- function(full_path, fraga = FALSE, skrivover = FALSE){
  fil_namn <- basename(full_path)
  mapp_namn <- dirname(full_path)
  if (file.exists(full_path)){
    if (fraga == TRUE) {
      skrivover <- askYesNo("Filen finns redan, vill du skriva över den?", default = FALSE)
      if (is.na(skrivover)) skrivover <- FALSE 
    }
    if (skrivover == FALSE){
      finnsredan <- TRUE
      nummer <- 2
      while (finnsredan == TRUE){
        full_path <- paste0(mapp_namn, "/", unlist(strsplit(fil_namn, "\\."))[1], "_", nummer, ".", 
                            unlist(strsplit(fil_namn, "\\."))[2])
        if (!file.exists(full_path)) finnsredan <- FALSE
        nummer <- nummer + 1
      }
    }
    
  }
  return(full_path)
}

skapa_mapp_om_den_inte_finns <- function(sokvag){
  if (!dir.exists(sokvag)) dir.create(sokvag)
  return(sokvag)
}

sparafil_en_backup_nvdb <- function(full_path){
  fil_namn <- basename(full_path)
  mapp_namn <- dirname(full_path)
  if (file.exists(full_path)){
    backup_fil <-  paste0(mapp_namn, "/", unlist(strsplit(fil_namn, "\\."))[1], "_backup.", 
                          unlist(strsplit(fil_namn, "\\."))[2])
    if (file.exists(backup_fil)) file.remove(backup_fil)
    file.rename(full_path, backup_fil)
  }
  return(full_path)
}

# tar en backup på existerande fil om en sådan finns, så kan den nya filen heta medskickat namn
sparafil_backup_omfinns <- function(full_path, fraga = FALSE, skrivover = FALSE){
  fil_namn <- basename(full_path)
  mapp_namn <- dirname(full_path)
  if (file.exists(full_path)){
    if (fraga == TRUE) {
      skrivover <- askYesNo("Det finns redan en fil med samma namn, vill du göra en backup på den och sedan skriva över den?", default = FALSE)
      if (is.na(skrivover)) skrivover <- FALSE 
    }
    if (skrivover == FALSE){
      finnsredan <- TRUE
      nummer <- 2
      while (finnsredan == TRUE){
        ny_path <- paste0(mapp_namn, "/", unlist(strsplit(fil_namn, "\\."))[1], "_old_", nummer, ".", 
                            unlist(strsplit(fil_namn, "\\."))[2])
        if (!file.exists(ny_path)) {
          file.copy(full_path, ny_path)
          file.remove(full_path)
          finnsredan <- FALSE
        } 
        nummer <- nummer + 1
      }
    }
    
  }
  return(full_path)
}


ladda_ned_fil <- function(output_mapp, fil_url) {
  
  # ============================= laddar ner en GIS-fil och lägger i den mapp man skickar med sökväg till ==============================
  # ========================== fungerar med en länk som går direkt till en fil, inte till en mapp eller ftp ==============================
  # ================================ om det är en zipfil så packas den upp och zipfilen raderas ========================================
  
  # skapa mappen om den inte finns  
  skapa_mapp_om_den_inte_finns(output_mapp)
  
  # ta ut filnamn ur sökvägen
  fil_namn <- basename(fil_url)
  
  # lägg till en "/" på slutet om det inte finns i output_mapp
  output_mapp <- ifelse(substr(output_mapp, nchar(output_mapp), nchar(output_mapp)) != "/",
                        paste0(output_mapp, "/"), 
                        output_mapp)
  
  # ta bort sista positionen i strängen om det är "/" - för att sökvägen ska fungera med unzip
  zip_mapp <- ifelse(substr(output_mapp, nchar(output_mapp), nchar(output_mapp)) == "/",
                     substr(output_mapp, 1, nchar(output_mapp)-1), 
                     output_mapp)
  
  download.file(fil_url, destfile = paste0(output_mapp, fil_namn))         # ladda ner filen
  
  # packa upp om det är en zip-fil, och sedan ta bort zip-filen
  if (tolower(substr(fil_namn, nchar(fil_namn)-3, nchar(fil_namn))) == ".zip") {
    unzip(paste0(output_mapp, fil_namn), exdir = zip_mapp)                    # packa upp filen
    file.remove(paste0(output_mapp, fil_namn))
  }
}
