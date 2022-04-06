
Berakna_varden_stodlinjer <- function(min_varde, max_varde, y_borjar_pa_noll = TRUE, procent_0_100_10intervaller = FALSE) {
  if (procent_0_100_10intervaller) {
    stodlinjer <- list("min_yvar" = 0, "max_yvar" = 100, "min_by_yvar" = 2, "maj_by_yvar" = 10)
  } else {
    modifierat_varde <- FALSE
    if (max_varde < 1) {
      max_varde <- max_varde * 100
      min_varde <- min_varde * 100
      modifierat_varde <- TRUE
    }
    
    # tilldela variabler för att beräkna max och min i diagrammet
    min_yvar <- ifelse(min_varde > 0 & y_borjar_pa_noll, 0, round(min_varde,(nchar(trunc(min_varde))-2)*-1))
    max_yvar <- round(max_varde,(nchar(trunc(max_varde))-2)*-1)
    # om max_yvar är mindre än maxvärdet eller ligger mindre än 5 % över maxvärdet
    # så lägger vi till ett värde så att vi istället avrundar uppåt
    antal_siff_avrundn <- ifelse(nchar(trunc(max_yvar)) < 2, -1,-2)
    max_yvar <- ifelse(max_yvar < max_varde,
                       plyr::round_any(max_yvar, (10 ^ (nchar(trunc(max_yvar))+antal_siff_avrundn)), f = ceiling),
                       max_yvar)
    maj_by_yvar <- round((max_yvar-min_yvar) / 6, (nchar(trunc((max_yvar-min_yvar)/6))-1)*-1)
    maj_by_yvar <- 2 * ifelse(floor(maj_by_yvar/2)==0, 1, ceiling(maj_by_yvar/2))
    test_div <- c(5, 6, 4, 7)
    max_yvar <- (round(max_yvar / maj_by_yvar)) * maj_by_yvar     # För att maxvärdet alltid ska vara en major break
    if (max_yvar < max_varde) max_yvar <- (round(max_yvar / maj_by_yvar)+1) * maj_by_yvar
    min_by_yvar <- NA
    for (i in 1:length(test_div)){
      if (maj_by_yvar %% test_div[i] == 0) {
        min_by_yvar <- maj_by_yvar / test_div[i]
        break
      }
    }
    if (is.na(min_by_yvar)) min_by_yvar <- maj_by_yvar / 5
    
    # anpassa maxvärdet för stödlinjen om den inte är jämn med noll och
    # värdena sträcker sig över 0-linjen (både pos och neg tal)
    if (min_varde < 0 & max_varde > 0){
  
      # vi börjar med positiva maxvärdet
      test_varde <- 0
      while(test_varde < max_varde) test_varde <- test_varde + maj_by_yvar
      max_yvar <- test_varde
      
      # och så kör vi negativa minvärdet också
      test_varde <- 0
      while(test_varde > min_varde) test_varde <- test_varde - maj_by_yvar
      min_yvar <- test_varde
    }
    
    stodlinjer <- list("min_yvar" = min_yvar, "max_yvar" = max_yvar, "min_by_yvar" = min_by_yvar, "maj_by_yvar" = maj_by_yvar)
    # kontrollera om vi har modifierat max och min-värdena, i så fall, modifiera tillbaka till ursprungsvärdena
    if (modifierat_varde) {
      stodlinjer <- list("min_yvar" = min_yvar / 100, "max_yvar" = max_yvar / 100, 
                         "min_by_yvar" = min_by_yvar / 100, "maj_by_yvar" = maj_by_yvar / 100)
    }
    return(stodlinjer)
  } # slut if-sats procent_0_100_10intervaller
}



SkapaProcForandrTvaAr <- function(df, ar_kol, gruppering_vect, summ_var, startar = NA, slutar = NA){
  
  if (is.na(startar)) startar <- min(df[,ar_kol])            # om inte startår skickas med, använd första året i årskolumnen
  if (is.na(slutar)) slutar <- max(df[,ar_kol])            # om inte slutår skickas med, använd sista året i årskolumnen
  gruppering_strang <- c(ar_kol , gruppering_vect)
  
  retur_df <- df %>%
    filter(!!as.name(ar_kol) == startar | !!as.name(ar_kol) == slutar) %>%
    group_by_at(vars(one_of(gruppering_strang))) %>% 
    summarize(syss = sum(!!as.name(summ_var))) %>% 
    pivot_wider(names_from = !!as.name(ar_kol), names_prefix = "ar_", values_from = syss)
  
  retur_df <- retur_df %>% mutate(proc = ((!!as.name(names(retur_df)[ncol(retur_df)])-
                                           !!as.name(names(retur_df)[ncol(retur_df)-1]))/
                                           !!as.name(names(retur_df)[ncol(retur_df)-1]))*100)
  retur_df$proc[retur_df$proc == "Inf"] <- NA
  names(retur_df)[ncol(retur_df)] <- paste0("Förändring ", tolower(summ_var), " ", startar, "-", slutar, " (procent)")
  return(retur_df)
}

diagramfarger <- function(farg = "gron_sex"){

  # ===================================== orange färgskalor ===========================================
  orange_en <- rgb(237,125,49, maxColorValue = 255)
  
  orange_tva <- c(rgb(131,60,12, maxColorValue = 255),
                      rgb(237,125,49, maxColorValue = 255))
  
  orange_tre_fokus <- c(rgb(237,125,49, maxColorValue = 255),
                            rgb(131,60,12, maxColorValue = 255),
                            rgb(0,0,0, maxColorValue = 255))
  
  orange_fyra <- c(rgb(245,194,177, maxColorValue = 255),
                   rgb(222,117,45, maxColorValue = 255),
                   rgb(186,97,36, maxColorValue = 255),
                   rgb(131,60,12, maxColorValue = 255))
  
  orange_sex <- c(rgb(245,190,175, maxColorValue = 255),
                  rgb(235,154,147, maxColorValue = 255),
                  rgb(222,117,45, maxColorValue = 255),
                  rgb(186,97,36, maxColorValue = 255),
                  rgb(151,80,32, maxColorValue = 255), 
                  rgb(103,53,19, maxColorValue = 255))
  
  # ==================================== grön färgskalor ================================================
  
  gron_tva_fokus <- c(rgb(112,173,71, maxColorValue = 255),
                      rgb(55,86,35, maxColorValue = 255))
                      #rgb(0,0,0, maxColorValue = 255))    # svart
  
  gron_tva_fokus <- c(rgb(112,173,71, maxColorValue = 255),
                      #rgb(55,86,35, maxColorValue = 255),
                      rgb(0,0,0, maxColorValue = 255))
  
  gron_tre_fokus <- c(rgb(112,173,71, maxColorValue = 255),
                                 rgb(55,86,35, maxColorValue = 255),
                                 rgb(0,0,0, maxColorValue = 255))
  
  gron_fyra <- c(rgb(169,208,142, maxColorValue = 255),
                            rgb(112,173,71, maxColorValue = 255),
                            rgb(84,130,53, maxColorValue = 255),
                            rgb(55,86,35, maxColorValue = 255))

  gron_sex <- c(rgb(226,239,218, maxColorValue = 255),
                           rgb(198,224,180, maxColorValue = 255),
                           rgb(169,208,142, maxColorValue = 255),
                           rgb(112,173,71, maxColorValue = 255),
                           rgb(84,130,53, maxColorValue = 255),
                           rgb(55,86,35, maxColorValue = 255))
  
  # ======================================== kön =========================================================
  
  kon <- c("#e2a855", "#459079")
  kon_fokus <-c("#e2a855", "#459079",rgb(237,125,49, maxColorValue = 255),"#0e5a4c")
  
  
  # ======================================== Lupp ========================================================
  
  # färgskala för att sätta ihop undersökning och kön
  grp_und_kon <- c(rgb(255,236,159, maxColorValue = 255),
                   rgb(255,211,120, maxColorValue = 255),
                   rgb(226,168,85, maxColorValue = 255),
                   rgb(147,206,193, maxColorValue = 255),
                   rgb(84,183,152, maxColorValue = 255),
                   rgb(69,144,121, maxColorValue = 255))
  
  Kön <- c("#e2a855", "#459079")
  
  Fodelseland_kat <- gron_sex
  
  Vistelsetid_kat <- gron_sex
  
  socek_kat <- orange_sex
  
  # grupperna men färger per år - använd samma färgskala som ovan
  
  Kön_år <-  brewer.pal(9, "Blues")[c(4, 6, 8)]                                        #grp_und_kon
  
  Fodelseland_kat_år <- gron_sex
  
  Vistelsetid_kat_år <- gron_sex                                           #c(brewer.pal(9, "Greens"), brewer.pal(9, "Purples")[c(4, 6, 8)])
  
  socek_kat_år <- orange_sex
  
  # grupperna men färger för alla svarsalternativ
  
  Kön_alla <- c(rgb(255,236,159, maxColorValue = 255),
                rgb(147,206,193, maxColorValue = 255),
                rgb(255,211,120, maxColorValue = 255),
                rgb(84,183,152, maxColorValue = 255),
                rgb(226,168,85, maxColorValue = 255),
                rgb(69,144,121, maxColorValue = 255))
  
  Fodelseland_kat_alla <- gron_sex[c(4,1,5,2,6,3)]
  
  Vistelsetid_kat_alla <- rep(gron_sex[c(3,4,5,6)],3)                                           #c(brewer.pal(9, "Greens"), brewer.pal(9, "Purples")[c(4, 6, 8)])
  
  socek_kat_alla <- orange_sex[c(4,1,5,2,6,3)]
  
  # kommunjämförelser
  kommun_jmfr <- brewer.pal(9, "BuPu")[c(6,7,8)]
  
  
  # ========================================= övriga =====================================================
  

  
  bla_gra_tre <- c(rgb(91,155,213, maxColorValue = 255),
                   rgb(191,191,191, maxColorValue = 255),
                   rgb(31,78,120, maxColorValue = 255))
  
  gron_gul_tvagrp_fyra <- rev(c(rgb(55,86,35, maxColorValue = 255),
                                 rgb(84,130,53, maxColorValue = 255),
                                 rgb(112,173,71, maxColorValue = 255),
                                 rgb(198,224,180, maxColorValue = 255),
                                 rgb(128,96,0, maxColorValue = 255),
                                 rgb(191,143,0, maxColorValue = 255),
                                 rgb(255,192,0, maxColorValue = 255),
                                 rgb(255,230,153, maxColorValue = 255)))
  
  gron_yrke_4_fokus <- c(rgb(0,0,0, maxColorValue = 255),
                         rgb(84,130,53, maxColorValue = 255),
                         rgb(112,173,71, maxColorValue = 255),
                        rgb(55,86,35, maxColorValue = 255))
  
  
  return(get(farg))
}
