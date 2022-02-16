
# Denna funktion beräknar mittpunkter för två kolumner med x- och y-koordinater i 
# textform, om koordinaterna är i nedre vänstra hörnet (som SCB:s rutor).
#
# Funktionen  behöver: 
# - en dataframe som innehåller kolumner med x- och y-koordinater
# - namn på x- och y-kolumnerna som text
# - ett numeriskt värde för rutstorleken
# - namn för de nya x- och y-kolumnerna med mittpunkter, annars döps de till "mitt_x" och "mitt_y"
#
# Retur: en df som är likadan som den som skickades men med 2 nya kolumner som
#        innehåller mittpunkter för x- och y-koordinaten
#
berakna_mittpunkter <- function(df, xruta, yruta, rutstorlek, 
                                xkolnamn = "mitt_x", ykolnamn = "mitt_y"){
  # beräkna de nya kolumnerna
  df[xkolnamn] <- df[xruta]+(rutstorlek/2)
  df[ykolnamn] <- df[yruta]+(rutstorlek/2)
  # flytta de nya kolumnerna och lägg dem efter x- och y-kolumnerna
  df <- df %>% 
    relocate(all_of(xkolnamn), .after = yruta) %>% 
    relocate(all_of(ykolnamn), .after = all_of(xkolnamn))
  
  return(df)
}

postgis_skapa_schema_om_inte_finns <- function(schema_namn){
  # kör sql-kod för att skapa ett nytt schema med namn definierat ovan
  dbExecute(con, paste0("create schema if not exists ", schema_namn, ";"))
}