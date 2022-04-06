library(janitor)

Skapa_grupperad_tabell <- function(skickad_DF, Installningar){
  # Skapa flikar =================================
  retur <- list()
  
  for (rapport in 1:length(Installningar)){
    # Här hämtar vi värden från Installningar, finns inte PivotVar2 eller TaBortTotal så blir de NA resp FALSE
    Summa_var <- as.name(Installningar[[rapport]][[which(grepl("Summa_var", names(Installningar[[rapport]])))]])
    PivotVar1 <- as.name(Installningar[[rapport]][[which(grepl("PivotVar1", names(Installningar[[rapport]])))]])
    variabel <- as.name(Installningar[[rapport]][[which(grepl("rad_var", names(Installningar[[rapport]])))]])
    filter_par <- Installningar[[rapport]][[which(grepl("filter", names(Installningar[[rapport]])))]]
    fliknamn <- Installningar[[rapport]][[which(grepl("namn", names(Installningar[[rapport]])))]]
    # Kolla om PivotVar2 finns i Inställningar, om inte ge den värdet NA
    if (any(grepl("PivotVar2", names(Installningar[[rapport]])))) PivotVar2 <- Installningar[[rapport]][[which(grepl("PivotVar2", names(Installningar[[rapport]])))]] else PivotVar2 <- NA
    if (!is.na(PivotVar2)) PivotVar2 <- as.name(PivotVar2)
    # Kolla om TaBortTotal finns i Inställningar, om inte ge den värdet FALSE
    if (any(grepl("TaBortTotal", names(Installningar[[rapport]])))) TaBortTotal <- Installningar[[rapport]][[which(grepl("TaBortTotal", names(Installningar[[rapport]])))]] else TaBortTotal <- FALSE
    
    # Kör filter om det inte är NA, i så fall kör vi skickad_DF som den är
    if (is.na(filter_par)) {
      temp_df <- skickad_DF 
    } else {
      temp_df <- skickad_DF %>% filter(!! rlang::parse_expr(filter_par))
    }
    
    # Skapa pivottabell med bara en variabel eller med två om det är valt att göra så
    if (is.na(PivotVar2)){
      temp_df <- temp_df %>%
      group_by(!!PivotVar1, !!variabel) %>% 
      summarize(antal = sum(!!Summa_var))  
    } else {
      temp_df <- temp_df %>%
        group_by(!!PivotVar1, !!variabel, !!PivotVar2) %>% 
        summarize(antal = sum(!!Summa_var))  
    }
    
    if (TaBortTotal) {
      # Här vill vi ta bort total-raden (gör vi i inkomst då vi redan har en totalrad där redan)
      if (is.na(PivotVar2)){
        # Här har vi bara År som vi pivoterar på
        temp_df <- temp_df %>% 
        pivot_wider(names_from = !!PivotVar1, values_from = antal)
      } else {
        # Här pivoterar vi även på en andra variabel (oftast kön, dvs det blir År och kön)
        temp_df <- temp_df %>% 
        pivot_wider(names_from = c(!!PivotVar1, !!PivotVar2), values_from = antal)
      }
    } else {
      # Här ska vi ha kvar total-raden vilket gäller alla rapporter utom inkomst
      if (is.na(PivotVar2)){
        # Här har vi bara År som vi pivoterar på 
        temp_df <- temp_df %>% 
          pivot_wider(names_from = !!PivotVar1, values_from = antal) %>%  
          adorn_totals("row")        # Lägg till totalrad om TaBortTotal = FALSE
      } else {
        # Här pivoterar vi även på en andra variabel (oftast kön, dvs det blir År och kön)
        temp_df <- temp_df %>% 
          pivot_wider(names_from = c(!!PivotVar1, !!PivotVar2), values_from = antal) %>%  
          adorn_totals("row")        # Lägg till totalrad om TaBortTotal = FALSE
      }
    }
    # Här tar vi bort kategorier som har blivit NA, specifikt utbildningsnivå och "ej i utbildningsregistret" som inte är med när vi gör factor av utbildningsnivå, vi vill inte ha med den så därför tar vi bort den här (den är bara med för att få med totalbefolkningen i uttagstabellen)
    temp_df <- temp_df[!is.na(temp_df[,1]),]
    temp_df[temp_df[,1] == "Total",1] <- "Totalt"       # döp om "Total" till "Totalt" OM "Total" finns i kolumn 1
    temp_temp <- temp_df[1:nrow(temp_df), 2:ncol(temp_df)]    # ta bort kolumn 1 och lägg i ny df
    temp_temp[temp_temp < 4 & temp_temp > 0] <- NA            # byt ut alla tal över 0 och under 4 till NA
    temp_df[1:nrow(temp_df), 2:ncol(temp_df)] <- temp_temp    # lägg tillbaka ny df där vi tog den ifrån men nu med NA för tal mellan 0 och 4
    
    retur[[rapport]] <- temp_df
    names(retur)[rapport] <- fliknamn
  }
  retur
}

BearbetaBIDData <- function(skickad_geografi, skickad_fliknamn, skickad_df){
  skickad_df_filtrerad <- skickad_df[grepl(skickad_geografi, skickad_df$Distrikt),]   # Välj ut skickad geografi ur alla möjliga geografier som finns i skickad df
  skickad_df_filtrerad$Distrikt <- trimws(gsub('[[:digit:]]+', '', skickad_df_filtrerad$Distrikt)) # ta bort eventuella siffror och mellanslag ur distriktsnamnet
  
  # ================================= ändra variabler om de finns i df ====================================
  # Ändra ålder om det finns i df
  if (any(grepl("alder", names(skickad_df_filtrerad)))){
    skickad_df_filtrerad$BID_alder[skickad_df_filtrerad$BID_alder == "80 år - 115 år"] <- "80+ år"
    skickad_df_filtrerad$BID_alder <- factor(skickad_df_filtrerad$BID_alder, levels = c(
      "0 år - 6 år", "7 år - 15 år", "16 år - 19 år", "20 år - 24 år", "25 år - 44 år", 
      "45 år - 64 år", "65 år - 79 år", "80+ år", "Totalt"))
    skickad_df_filtrerad <- skickad_df_filtrerad %>% rename(Ålder = BID_alder)
  }
  
  # Ändra utbildningsnivå om det finns i df
  if(any(grepl("Utbildningsnivå_4", names(skickad_df_filtrerad)))){
    skickad_df_filtrerad <- skickad_df_filtrerad %>% rename(Utbildningsnivå = recUtbildningsnivå_4kat)
    skickad_df_filtrerad$Utbildningsnivå <- factor(skickad_df_filtrerad$Utbildningsnivå, levels = c(
      "Förgymnasial", "Gymnasial", "Eftergymnasial kort", "Eftergymnasial lång", "Uppgift saknas"))
  }
  
  # Ändra flyttgränser-variabeln om den finns i df
  if (any(grepl("Flyttgräns", names(skickad_df_filtrerad)))) skickad_df_filtrerad <- skickad_df_filtrerad %>% rename(Befolkningsförändringar = Flyttgräns.administrativa.indelningar)
  
  # Ändra inkomst-variabeln om den finns i df
  if (any(grepl("Förvärvsinkomst", names(skickad_df_filtrerad)))) skickad_df_filtrerad <- skickad_df_filtrerad %>% rename(Medianinkomst = Median.of.Förvärvsinkomst.individ)
  
  # Ändra SNI-variabeln om den finns i df
  if (any(grepl("SNI", names(skickad_df_filtrerad)))) skickad_df_filtrerad <- skickad_df_filtrerad %>% rename(Bransch = SNI_Rapsgrupperingar_.Gxx.)
  
  
  
  # styr skickad_df_filtrerad till funktion utifrån vilken typ av data den innehåller ============================
  if (skickad_fliknamn == "BID_bef") {
    Installningar <- list(c(PivotVar1 = "Ar", rad_var = "Kön", filter = NA, namn = "Befolkning", Summa_var = "fBBefolkning"),
                          c(PivotVar1 = "Ar", rad_var = "Ålder", filter = NA, namn = "Bef Ålder", Summa_var = "fBBefolkning"),
                          c(PivotVar1 = "Ar", rad_var = "Ålder", filter = NA, namn = "Bef Kön och ålder", PivotVar2 = "Kön", Summa_var = "fBBefolkning"),
                          c(PivotVar1 = "Ar", rad_var = "Utbildningsnivå", filter = 'Ålder == "25 år - 44 år" | Ålder == "45 år - 64 år"', 
                            namn = "Bef Utbnivå 25-64 år", Summa_var = "fBBefolkning"),
                          c(PivotVar1 = "Ar", rad_var = "Civilstånd", filter = NA, namn = "Civilstånd", Summa_var = "fBBefolkning"))
  }
  if (skickad_fliknamn == "BID_flytt") {
    Installningar <- list(c(PivotVar1 = "Ar", rad_var = "Befolkningsförändringar", filter = NA, namn = "Befolkningsförändringar", Summa_var = "fFlyttning"))
  }
  if (skickad_fliknamn == "BID_hushall") {
    Installningar <- list(c(PivotVar1 = "Ar", rad_var = "Hustyp", filter = NA, namn = "Hushåll", Summa_var = "fBBefolkning"))
  }
  if (skickad_fliknamn == "BID_inkomst") {
    Installningar <- list(c(PivotVar1 = "Ar", rad_var = "Kön", filter = NA, namn = "Medianinkomst", TaBortTotal = TRUE, Summa_var = "Medianinkomst"))
  }
  if (skickad_fliknamn == "BID_syss") {
    Installningar <- list(c(PivotVar1 = "Ar", rad_var = "Bransch", filter = 'Sysselsättningsstatus == "Förvärvsarbetande"', namn = "Branscher", Summa_var = "Personer"),
                          c(PivotVar1 = "Ar", rad_var = "Kön", filter = 'Sysselsättningsstatus == "Förvärvsarbetande"', namn = "Förvärvsarbetande kön", Summa_var = "Personer"),
                          c(PivotVar1 = "Ar", rad_var = "Ålder", filter = 'Sysselsättningsstatus == "Förvärvsarbetande"', namn = "Förvärvsarbetande ålder", Summa_var = "Personer"))
  }
  retur_df <- Skapa_grupperad_tabell(skickad_df_filtrerad, Installningar)
  
}