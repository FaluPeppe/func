# funktioner för att hantera api-anrop via px_web samt möjligen i framtiden även 
# andra paket

library(pxweb)
library(httr)             # för att komma förbi brandväggen
library(dplyr)
library(stringr)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

hamtaregtab <- function(){

# Hämta tabell med regioner
url_adress <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
url_adress <- paste0("http://api.scb.se", url_adress)


# Välj variabler
px_uttag_region <- pxweb_get(url = url_adress,
                             query = list(
                               Region = "*",
                               ContentsCode = "BE0101N1",
                               Tid = '2020'
                             )) 

# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
regdf <- as.data.frame(px_uttag_region) %>% 
  cbind(regionkod = as.data.frame(px_uttag_region, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
regdf <- regdf %>% select(regionkod, region)


regdf
}

hamtakommuner <- function(lan = "20", tamedlan = TRUE, tamedriket = TRUE, allakommuner = FALSE){
  regdf <- hamtaregtab()
  #lan <- as.character(lan)
  lan <- substr(lan,1,2)
  nydf <- regdf
  if (!allakommuner) nydf <- nydf %>%  filter(regionkod == "00" | substr(regionkod,1,2) == lan)
  if (!tamedlan) nydf <- nydf %>% filter(regionkod != lan) 
  if (!tamedriket) nydf <- nydf %>% filter(regionkod != "00")
  if (allakommuner) nydf <- nydf[nchar(nydf$regionkod) == 4,]
  vektor <- as.vector(nydf$regionkod)
  vektor
}

hamtaAllaLan <- function(tamedriket = TRUE){
  regdf <- hamtaregtab()
  
  # Hämta tabell med regioner
  url_adress <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  url_adress <- paste0("http://api.scb.se", url_adress)
  
  # gör litet uttag för att hitta senaste år
  px_uttag_region <- pxweb_get(url = url_adress,
                               query = list(
                                 Region = "20",
                                 ContentsCode = "BE0101N1",
                                 Tid = '*'
                               ))
  
  regdf <- as.data.frame(px_uttag_region) %>% 
    cbind(regionkod = as.data.frame(px_uttag_region, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  senaste_ar <- max(regdf$år)
  
  # nytt uttag med alla län för senaste år
  px_uttag_region <- pxweb_get(url = url_adress,
                               query = list(
                                 Region = "*",
                                 ContentsCode = "BE0101N1",
                                 Tid = senaste_ar
                               ))
  
  regdf <- as.data.frame(px_uttag_region) %>% 
    cbind(regionkod = as.data.frame(px_uttag_region, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  nydf <- regdf %>% select(regionkod, region)
  nydf <- nydf[nchar(nydf$regionkod) == 2,]
  
  if (!tamedriket) nydf <- nydf %>% filter(regionkod != "00")
  vektor <- as.vector(nydf$regionkod)
  vektor
}

hamtaregion_kod_namn <- function(regionkod){
  regdf <- hamtaregtab()
  #test_vec <- c("20", "22")
  retur_df <- regdf[regdf$regionkod %in% regionkod,]
  return(retur_df)
}

skapa_kortnamn_lan <- function(lansnamn){
  nyttnamn <- NA
  for (elem in 1:length(lansnamn)){
    if (substr(lansnamn[elem], nchar(lansnamn[elem])-3, nchar(lansnamn[elem]))==" län") nyttnamn[elem] <- substr(lansnamn[elem],1, nchar(lansnamn[elem])-4) else nyttnamn[elem] <- lansnamn[elem]
    if (substr(nyttnamn[elem], nchar(nyttnamn[elem]),nchar(nyttnamn[elem]))=="s") nyttnamn[elem] <- substr(nyttnamn[elem],1,nchar(nyttnamn[elem])-1)
  }
  return(nyttnamn)
}

hamta_senaste_tid_i_tabell <- function(skickad_url, tidkol, tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[!names(query_list) %in% tabort_var]
  
  px_small <- pxweb_get(url = skickad_url, query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  senaste_tid <- as.numeric(max(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  return(as.character(senaste_tid))
}

hamta_tidigaste_tid_i_tabell <- function(skickad_url, tidkol, tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[!names(query_list) %in% tabort_var]
  
  px_small <- pxweb_get(url = skickad_url,query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  tidigaste_tid <- as.numeric(min(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  return(as.character(tidigaste_tid))
}

hamta_giltig_tid_tabell <- function(skickad_url, tidkol, tabort_var = NA, region_varde = "20", query_list = NULL){
  # fyll query-lista till px_web-uttaget
  if (is.null(query_list)) query_list <- list(Region = region_varde, ContentsCode = "*", Tid = "*")
  # om tabellen inte innehåller en variabel, ta bort den variabeln ur query_list
  if (!is.na(tabort_var)) query_list <- query_list[names(query_list) %in% tabort_var]

  px_small <- pxweb_get(url = skickad_url, query = query_list) 
  px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
  senaste_tid <- as.numeric(max(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  tidigaste_tid <- as.numeric(min(px_df_small[[tidkol]]))     # startår är alltid ett år innan första året i prognosen
  giltig_vekt <- as.character(tidigaste_tid:senaste_tid)
  return(giltig_vekt)  
}

skapa_intervaller <- function(skickad_kolumn, antal_intervaller = 5){
  
  intervall <- NA              # skapa variabel
  
  max_kol <- max(skickad_kolumn) 
  min_kol <- min(skickad_kolumn)
  range_kol <- max_kol - min_kol
  
  intervall_range <- round(range_kol / (antal_intervaller-1))
  
  # ta fram en siffra som vi avrundar till, ska helst vara 5000 om siffran ligger nära där, 500 om siffran är nära där osv.
  avrundning_num <- round(intervall_range, nchar(intervall_range)*-1)/2
  # om avrundning blir 0 så måste vi öka på den något
  if (avrundning_num == 0) avrundning_num <- round(intervall_range, (nchar(intervall_range)-1)*-1)/2
  
  intervall[1] <- min_kol
  intervall[antal_intervaller] <- max_kol
  
  for (x in 2:(antal_intervaller-1)){
    intervall[x] <- min_kol + (intervall_range * (x-1))
  }
  
  for (x in 1:length(intervall)){
    tal <- round(intervall[x])
    
    intervall[x] <- plyr::round_any(tal, avrundning_num)
    if (intervall[x] == 0) intervall[x] <- avrundning_num/10    # speciallösning, kan kanske fungera för flera fall, om 0 använd avrundning_num / 10
    #intervall[x] <- round(tal, (nchar(tal)-ifelse(nchar(tal)<5,1,2))*-1)
    # lägg till om siffran blir samma som tidigare siffra i vektorn
    if (x > 1){
      if (intervall[x] == intervall[x-1]) intervall[x] <- intervall[x] + round(intervall_range, -2)  
    }
    
    # fixa till om talet är under 1000 och nästa tal är över 1000, sätt till 500 i så fall
    if (x < length(intervall)){
      if (intervall[x] < 1000 & intervall[x+1] > 999) intervall[x] <- 500
    }
  }
  return(intervall)
}
