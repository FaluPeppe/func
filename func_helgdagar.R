ArHelgDag <- function(datum, ReturneraVilkenHelgDag = FALSE) {
  retur <- FALSE
  
  helgdagar <- Helgdagslista(format(datum, "%Y"))
  if (datum %in% helgdagar[,1]) {
      if (ReturneraVilkenHelgDag) { 
        retur <- helgdagar[helgdagar$datum == datum,2]  
      } else {
        retur <- TRUE
      }
  }  
  return(retur)
}

Helgdagslista <- function(ar = format(Sys.time(), "%Y")) {
  ar_int <- as.integer(ar)
  
  df_dag <- NULL
  df_dag$datum <- as.Date(NA)
  df_dag$helgdag <- as.character(NA)
  df_dag <- as.data.frame(df_dag)
  
  df_dag[1,1] <- as.Date(paste(ar_int, 1, 1, sep = "-"), format("%Y-%m-%d"))  
  df_dag[1,2] <- "Nyårsdagen"
  
  df_dag[2,1] <- as.Date(paste(ar_int, 1, 6, sep = "-"), format("%Y-%m-%d"))  
  df_dag[2,2] <- "Trettondedag jul"
  
  Paskdag <- PaskdagDatum(ar_int)
  df_dag[3,1] <- Paskdag - 2
  df_dag[3,2] <- "Långfredag"
  
  df_dag[4,1]<- Paskdag
  df_dag[4,2] <- "Påskdagen"
  
  df_dag[5,1] <- Paskdag + 1
  df_dag[5,2] <- "Annandag påsk"
  
  df_dag[6,1] <- as.Date(paste(ar_int, 5, 1, sep = "-"), format("%Y-%m-%d"))
  df_dag[6,2] <- "Första maj"
  
  df_dag[7,1] <- Paskdag + 39
  df_dag[7,2] <- "Kristi himmelsfärdsdag"
  
  df_dag[8,1] <- Paskdag + 49
  df_dag[8,2] <- "Pingstdagen"
  
  df_dag[9,1] <- as.Date(paste(ar_int, 6, 6, sep = "-"), format("%Y-%m-%d"))
  df_dag[9,2] <- "Sveriges nationaldag"
  
  df_dag[10,1] <- MidsommardagenDatum(ar_int)
  df_dag[10,2] <- "Midsommardagen"
  
  df_dag[11,1] <- AllaHelgonsdagDatum(ar_int)
  df_dag[11,2] <- "Alla helgons dag"
  
  df_dag[12,1] <- as.Date(paste(ar_int, 12, 24, sep = "-"), format("%Y-%m-%d"))
  df_dag[12,2] <- "Julafton"
  
  df_dag[13,1] <- as.Date(paste(ar_int, 12, 25, sep = "-"), format("%Y-%m-%d"))
  df_dag[13,2] <- "Juldagen"
  
  df_dag[14,1] <- as.Date(paste(ar_int, 12, 26, sep = "-"), format("%Y-%m-%d"))
  df_dag[14,2] <- "Annandag jul"
  
  df_dag[15,1] <- as.Date(paste(ar_int, 12, 31, sep = "-"), format("%Y-%m-%d"))
  df_dag[15,2] <- "Nyårsafton"
  retur <- as.data.frame(df_dag)
}

  
PaskdagDatum <- function(ar = format(Sys.time(), "%Y")) {
  # funkar bara för åren 1900-2099
  ar_int <- as.integer(ar)
  M <- 24
  N <- 5
  
  a <- ar_int %% 19
  b <- ar_int %% 4
  c <- ar_int %% 7
  d <- (((19 * a) + M) %% 30)
  e <- ((2 * b) + (4 * c) + (6 * d) + N) %% 7
  
  if ((22 + d + e) > 31) {
    manad <- 04
    dag <- (22 + d + e) - 31 
  } else {
    manad <- 03
    dag <- (22 + d + e)
  }
retur <- as.Date(paste(ar_int, manad, dag, sep = "-"), format("%Y-%m-%d"))
}

MidsommardagenDatum <- function(ar = format(Sys.time(), "%Y")) {
  ar_int <- as.integer(ar)
  datum <- as.Date(paste(ar, 6, 20, sep = "-"), format("%Y-%m-%d"))
  while (format(datum, "%u") != 6) datum <- datum +1 
  retur <- datum
  
}

AllaHelgonsdagDatum <- function(ar = format(Sys.time(), "%Y")) {
  ar_int <- as.integer(ar)
  datum <- as.Date(paste(ar, 10, 31, sep = "-"), format("%Y-%m-%d"))
  while (format(datum, "%u") != 6) datum <- datum +1 
  retur <- datum
  
}
