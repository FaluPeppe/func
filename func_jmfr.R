jmfr_lagre_hogre_an <- function(valt_obj, jmfr_obj){
  retur_strang <- case_when(
    valt_obj > jmfr_obj ~ "högre än",
    valt_obj < jmfr_obj ~ "lägre än",
    valt_obj == jmfr_obj ~ "i nivå med"
  )
  return(retur_strang)
}

jmfr_okning_minskning_proc <- function(skickat_varde){
  retur_strang <- case_when(
    skickat_varde > 0 ~ paste0("ökning på ", round(abs(skickat_varde),1), " procent"),
    skickat_varde < 0 ~ paste0("minskning med ", round(abs(skickat_varde),1), " procent"),
    skickat_varde == 0 ~ "i nivå med"
  )
  retur_strang <- gsub("\\.", ",", retur_strang)
  return(retur_strang)
}

jmfr_sticker_ut <- function(skickat_namn, skickat_varde, topp = TRUE, jmfr_mot_noll = TRUE, max_antal = 5, grans_skillnad = 0.2){
  klar <- FALSE
  jmfr_noll <- FALSE
  # skickad_namn ska innehålla namn på ex. kommuner eller län som ska jämföras
  # skickat_varde ska innehålla värden för de geografier (eller annat) som skickas med som namn
  df <- as.data.frame(cbind(namn = skickat_namn, varde = skickat_varde)) # sätt ihop till en df
  df$varde <- as.numeric(df$varde)
  
  # säkerställ att max_antal är minst 1
  if (is.na(max_antal) | max_antal <= 0) max_antal <- 1 
  # sortera df utifrån om vi vill ha topp-värden eller botten-värden
  if(topp) df <- df %>% arrange(desc(varde)) else df <- df %>% arrange(varde)
  # beräkna skillnad mellan raderna för att hitta de som sticker ut
  df$diff <- (df$varde - lag(df$varde,1))/df$varde
  
  # om vi vill ha jämförelse mot noll och det är max 5 värden som är över eller under noll 
  # (beroende på om vi vill ha högsta eller lägsta värden) så kör vi med dem
  # annars tar vi ut max fem värden med gränsvärdet som angetts
  if (jmfr_mot_noll){             # om vi vill ta ut de som skiljer sig mot noll OM de inte är fler än max_antal, då kör vi med grans_skillnad
    if(topp) antal_diff_noll <- sum(df$varde > 0) else antal_diff_noll <- sum(df$varde < 0) 
      # här skapar vi retur om alla är diffar mot noll (och inte fler än max_antal)
      if (antal_diff_noll <= max_antal) {
        retur_namn <- list_komma_och(df$namn[1:antal_diff_noll])
        jmfr_noll <- TRUE
        klar <- TRUE
      }
  }
  if (klar == FALSE) {
  # om vi inte jämför mot noll, eller om diff mot noll är fler än max_antal så behöver vi använda grans_skillnad
  # om vi inte har någon grans_skillnad skickar vi bara maxantalet
    if (is.na(grans_skillnad)) {
      retur_namn <- list_komma_och(df$namn[1:max_antal])
    } else {
    # annars lägger vi till alla från toppen tills vi stöter på en diff > grans_skillnad 
      for (elem in 2:nrow(df)){    # loopa igenom df tills vi har en diff > grans_skillnad 
        if (abs(df$diff[elem]) > grans_skillnad) break   
      }
    # när vi brutit for-loopen skickar vi de element fram till man bröt
    retur_namn <- list_komma_och(df$namn[1:(elem-1)])
    }
  }
  
  retur_list <- list("namn" = retur_namn, "jmfr_noll" = jmfr_noll)
  return(retur_list)
  
}

jmfr_forandr_sen_x_tid <- function(df, tid_kol, varde_kol, antal_tidenheter, filter_kol = NA, filter_varde = NA){

  # Här skapas ett filter av 
  if (!is.na(filter_kol)){
    filter_hela <- paste0("'", filter_varde, "'") #%>%             # Lägg till ' runt värdena som ska filtreras ut
    filter_hela <- paste0(filter_kol, " == ", filter_hela)     # Sätt ihop variabeln som de tillhör, likamedtecken samt variabelvärdena (från raden ovan) 
    filter_hela <- paste(filter_hela, collapse = " | ")           # Sätt ihop alla värden till en OR-sats
  
    df_ny <- df %>% filter(!! rlang::parse_expr(filter_hela)) 
  } else {
    df_ny <- df
  }
  
  filter_tid <- paste0(names(df_ny[tid_kol]), " >= ", max(df_ny[tid_kol])-antal_tidenheter-1)
  
  df_ny <- df_ny %>% filter(!! rlang::parse_expr(filter_tid))
  # beräkna förändring per vecka och om den varit "märkbar"
  df_ny$forandrsenv <- df_ny[[varde_kol]] - lag(df_ny[[varde_kol]])
  #names(df_ny)[4] <- "for_sen_vddddd"
  df_ny$markbar_forandr <- NA
  df_ny$markbar_forandr <- ifelse(df_ny$forandrsenv/lag(df_ny$`antal personer`) > 0.01,1,0)
  df_ny$markbar_forandr <- ifelse(df_ny$forandrsenv/lag(df_ny$`antal personer`) < -0.01,-1,df_ny$markbar_forandr)
  # vi tar bort så vi bara har antal_tidenheter med i analysen, vi tog med en till för att få förändring på alla tidsenheter
  df_ny <- df_ny[2:nrow(df_ny),] 
  
  riktning <- case_when(
    sum(df_ny$forandrsenv)>0 ~ "ökat", 
    sum(df_ny$forandrsenv)<0 ~ "minskat",
    sum(df_ny$forandrsenv)==0 ~ "varit oförändrad"
    )
  
  riktning_2 <- case_when(
    riktning == "ökat" ~ "ökning", 
    riktning == "minskat" ~ "minskning"
  )
  
  huvudtext <- case_when(
    abs(sum(df_ny$forandrsenv) / df_ny[1,varde_kol]) < 0.01 ~ "relativt oförändrad",
    abs(sum(df_ny$forandrsenv) / df_ny[1,varde_kol]) < 0.02 ~ paste0(riktning, " något"),
    abs(sum(df_ny$forandrsenv) / df_ny[1,varde_kol]) < 0.03 ~ paste0(riktning),
    abs(sum(df_ny$forandrsenv) / df_ny[1,varde_kol]) < 0.02 ~ paste0(riktning, " markant"),
  )
  
  antal <- case_when(
    riktning == "ökat" ~ length(df_ny$forandrsenv[df_ny$forandrsenv > 0]),
    riktning == "minskat" ~ length(df_ny$forandrsenv[df_ny$forandrsenv < 0]),
    riktning == "oförändrad" ~ length(df_ny$forandrsenv[df_ny$forandrsenv == 0])
  )
  
  markbar <- case_when(
    sum(df_ny$markbar_forandr) == 0 ~ paste0("förändrats relativt lite ", names(df_ny[tid_kol]), " till ", names(df_ny[tid_kol])),
    sum(df_ny$markbar_forandr) == antal_tidenheter ~ paste0("ökat varje ", names(df_ny[tid_kol])),
    sum(df_ny$markbar_forandr) == - antal_tidenheter ~ paste0("minskat varje ", names(df_ny[tid_kol])),
    TRUE ~ "både ökat och minskat"
  )
  
  riktning_senasteveckan <- case_when(
    df_ny$forandrsenv[nrow(df_ny)]>0 ~ "ökat", 
    df_ny$forandrsenv[nrow(df_ny)]<0 ~ "minskat",
    df_ny$forandrsenv[nrow(df_ny)]==0 ~ "varit oförändrad"
  )
  
  senasteveckan <- case_when(
    df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] < 0.01 |
      df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] > -0.01 ~ "varit tämligen oförändrad",
    df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] < 0.02 |
      df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] > -0.02 ~ paste0(riktning_senasteveckan, " något"),
    df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] < 0.03 |
      df_ny$forandrsenv[nrow(df_ny)] / df_ny[nrow(df_ny)-1, varde_kol] > -0.03 ~ riktning_senasteveckan,
    TRUE ~ paste0(riktning_senasteveckan, " markant"))

   
  retur_list <- list("rikting" = riktning,
                     "riktning2" = riktning_2,
                     "huvudtext" = huvudtext,
                     "antal" = antal,
                     "markbar" = markbar, 
                     "senaste" = senasteveckan)
  return(retur_list)
}
