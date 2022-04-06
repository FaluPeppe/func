library(ggplot2)
library(RColorBrewer)
library(magick)
library(scales)       # för att använda format_format-funktionen och fixa till format på etiketter
library(httr)
#library(png)

source("G:/skript/func/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_diagramfunktioner.R", encoding = "utf-8", echo = FALSE)


# För att komma förbi brandvägg - om man har en sådan, annars kan man ta bort dessa två rader
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# ================================== funktion för att hämta sökväg till logga som läggs in i diagram =============================
hamta_logga_path <- function(){return("G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png")}           # sökväg till logga för att kunna lägga in den i diagrammen

# ================================== Inställningar för alla diagram som skapas ===========================================================

SkapaStapelDiagram <- function(skickad_df, 
                               skickad_x_var, 
                               skickad_y_var, 
                               skickad_x_grupp = NA, 
                               skickad_filter_OR_vect = NA, 
                               skickad_filter_OR_var = NA,
                               diagram_titel = NULL,          # textsträng, blir diagramtitel, om NULL så skrivs diagrammet utan titel
                               output_mapp,                   # textsträng, en giltig sökväg till den mapp som diagrammet ska sparas i 
                               diagram_capt = NULL,           # skicka med en textsträng som hamnar i nedre vänstra hörnet på diagram, som kan beskriva källa, vem som gjort diagrammet etc.
                               #diagram_capt_size, 
                               berakna_index = FALSE,
                               diagram_facet = FALSE,
                               facet_grp = NA,
                               facet_scale = "free",
                               facet_legend_bottom = FALSE,
                               manual_color = NA,
                               brew_palett = "Greens",
                               utan_diagramtitel = FALSE,
                               manual_y_axis_title = NA,
                               manual_x_axis_title = NA,
                               manual_x_axis_text_vjust = 0,
                               manual_x_axis_text_hjust = 0.5, 
                               x_axis_lutning = 45,
                               x_axis_storlek = 10.5,
                               x_axis_sort_value = FALSE,     # för att sortera x-axelns etiketter efter värdet i y-variablen
                               x_var_fokus = NA,              # om färgerna på grupperna ska styras av en särskild variabel anges den här, ex. "fokus" om det finns en kolumn som heter fokus och som innehåller ett antal unika värden (OBS! Då måste färgskalan innehålla minst lika många olika färger)
                               y_axis_100proc = FALSE,        # sätt till TRUE om y-axeln ska vara mellan 0 och 100
                               y_axis_borjar_pa_noll = TRUE,  # sätt till FALSE om y-axeln ska börja på annat värde än 0
                               procent_0_100_10intervaller = FALSE,  # om TRUE, så går y-axeln mellan 0 och 100, med tjocka stödlinjer med 10 enheters mellanrum, passar bra med procent 
                               y_axis_storlek = 12,
                               dataetikett_noll_visa_ej = FALSE,     # om TRUE, skrivs inga dataetiketter för nollvärden
                               legend_titel = NA,             # textsträng. Sätt en titel på teckenförklaringen. Om NA, så skriver den ut teckenförklaring utan titel
                               legend_tabort = FALSE,         # TRUE om man vill ta bort legenden oavsett andra inställningar
                               diagram_liggande = FALSE,      # TRUE/FALSE. TRUE för att skriva ut liggande diagram, annars blir det stående
                               geom_position_stack = FALSE,   # om man vill ha ett stacked bar chart och inte dodge (=grupper i samma stapel och inte bredvid varandra)
                               AF_special = FALSE,            # en speciallösning för arbetslöshetsdiagram från AF. 
                               lagg_pa_logga = TRUE,          # TRUE/FALSE. Sätt till FALSE om man inte vill ha med logga
                               logga_path = NA,               # textsträng. En giltig sökväg med filnamn till den bildfil som innehåller den logga man vill lägga med i diagrammet
                               logo_scale = 15,               # för att styra storlek på logga om man har med en sån, mindre tal = större logga
                               dataetiketter = FALSE,
                               skriv_till_diagramfil = TRUE,
                               diagramfil_hojd = 7,
                               diagramfil_bredd = 12,
                               filnamn_diagram){
  
  # Här skapas variabler som används av de som skickats till funktionen
  x_var <- as.name(skickad_x_var)
  if (!is.na(skickad_x_grupp)) x_grupp <- as.name(skickad_x_grupp) else x_grupp <- NA
  if (!is.na(x_var_fokus)) x_var_fokus_asname <- as.name(x_var_fokus)
  if (is.na(skickad_x_grupp)) grupp_var <- x_var else grupp_var <- as.character(c(x_var, x_grupp))       # Skapa group_by-sträng utifrån om det finns grupperingsvariabel
  if (!is.na(x_var_fokus)) grupp_var <- c(grupp_var, x_var_fokus)
  if (is.na(facet_grp)) {
    facet_grp <- x_grupp 
  } else {
    facet_grp <- as.name(facet_grp)
    grupp_var <- c(grupp_var, facet_grp)
  }
  # eventuellt är det så att `-symbolen ställer till det i variabelnamn med mellanslag i just grupp_var, 
  # jag testar därför att ta bort ` ur grupp_var
  grupp_var <- gsub("`", "", grupp_var)
  
  y_var <- as.name(skickad_y_var)
  filter_or <- skickad_filter_OR_vect
  filter_or_var <- skickad_filter_OR_var
  
  # om vi vill ha en titel på teckenförklaringen
  if (is.na(legend_titel)) legend_titel <- NULL
  
  # Här skapas ett filter av 
  if (!is.na(filter_or)){
    filter_or <- paste0("'", filter_or, "'") #%>%             # Lägg till ' runt värdena som ska filtreras ut
    filter_or <- paste0(filter_or_var, " == ", filter_or)     # Sätt ihop variabeln som de tillhör, likamedtecken samt variabelvärdena (från raden ovan) 
    filter_or <- paste(filter_or, collapse = " | ")           # Sätt ihop alla värden till en OR-sats
  }
  # gruppera variabler inför diagramskapande ===========================================================
  # testa om filter-variabeln är tom
  if (!is.na(filter_or)){
    # om filter_or har ett värde
    plot_df <- skickad_df %>% 
      filter(!! rlang::parse_expr(filter_or)) %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE))
  } else {
    # om filter_or är NA
    plot_df <- skickad_df %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE))
  }
  
  # beräkna index om det är valt =======================================================
  if (berakna_index){
    plot_index <- plot_df %>% 
      group_by(region) %>% 
      mutate(!!paste0("index_", min(plot_df$år)) := round((total/total[år == min(år)] * 100),0))
  }
  
  # ======================================== specialanpassningar av titlar och kategorinamn ===========================
  
  if (!is.na(manual_y_axis_title)) y_titel <- manual_y_axis_title else y_titel <- skickad_y_var
  
  # =========================================== skapa stödlinje-variabler =============================================
  
  if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här - slice(1) utfall att det finns flera grupper som uppnår maxvärde (då tar vi bara en av dem)
  if (geom_position_stack) max_varde_plot_df <- plot_df %>% group_by(across(all_of(skickad_x_var))) %>% summarise(summ = sum(total)) %>% filter(summ == max(summ)) %>% slice(1) %>% pull()
  stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min(plot_df["total"]), max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller)
  
  min_yvar <- stodlinjer_list$min_yvar
  max_yvar <- stodlinjer_list$max_yvar
  min_by_yvar <- stodlinjer_list$min_by_yvar
  maj_by_yvar <- stodlinjer_list$maj_by_yvar
  
  # =================================================================================================
  
  antal_grupper <- ifelse(is.na(skickad_x_grupp), 0, nrow(unique(plot_df[x_grupp])))
  
  #etikettformat <- format_format(big.mark = " ", decimal.mark = ".",
  #                               scientific = FALSE)
  
  # används för att skapa etikettformat
  etikett_format <- function(x){
    x <- format(x, big.mark = " ", scientific = FALSE)
    if (!is.na(manual_y_axis_title)){
      if (manual_y_axis_title == "procent") x <- paste0(x, " %")
    }
    return(x)
  }
  
  
  # diagramfärger
  if (!is.na(manual_color)[1]) {
    # om man skickat med manual_color så används den, ananrs kollar man övrigt
    chart_col <- manual_color
  } else {
    if (is.na(brew_palett)) brew_palett <- "Greens"
    if (!is.na(skickad_x_grupp)) {
      chart_col <- case_when(
        nrow(unique(plot_df[x_grupp])) == 1 ~ "#4f6228",
        nrow(unique(plot_df[x_grupp])) == 2 ~ c("#9bbb59", "#4f6228")
      )
      if (nrow(unique(plot_df[x_grupp])) > 2) chart_col <- brewer.pal(nrow(unique(plot_df[x_grupp])), brew_palett)
    } else {
      chart_col <- "#4f6228"
    }
  }
  # ge legend_pos ett standardvärde
  legend_pos <- "none"
  
  # ge position ett standardvärde
  if (geom_position_stack) geom_bar_position <- "stack" else geom_bar_position <- "dodge"
  
  # här styr vi huruvida vi vill ha titlar på x- och y-axlarna. 
  if (is.na(y_titel)) y_titel <- NULL
  if (manual_y_axis_title == "procent" & !is.na(manual_y_axis_title)) y_titel <- NULL            # om vi skickat med "procent" manuellt för y-axeln så läggs % på enheten och vi kan stänga av "procent" som y-axeletikett
  if (is.na(manual_x_axis_title)) manual_x_axis_title <- NULL
  
  # vill vi sortera x-axeln utifrån värdet på y
  if (x_axis_sort_value == TRUE) {
    plot_df[x_var] <- factor(plot_df[[x_var]])
      if (diagram_liggande){
        plot_df[x_var] <- reorder(plot_df[[x_var]], plot_df$total) # om vi kör liggande diagram sorterar vi inte med desc() så blir största värde högst upp istället, blir bättre vid liggande diagram
      } else {
        plot_df[x_var] <- reorder(plot_df[[x_var]], desc(plot_df$total))
      }
  }
  
  # Här börjar vi rita ut diagrammet
  if (!is.na(x_var_fokus)) {
    plot_df[x_var_fokus] <- factor(plot_df[[x_var_fokus]])
    #plot_df[x_var_fokus] <- reorder(plot_df[[x_var_fokus]], desc(plot_df$total))
    # Detta om vi bara ska ha en färg i diagrammet men fokusera på ett par olika staplar
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total)) + 
      geom_bar(position = geom_bar_position, stat="identity", aes(fill = !!x_var_fokus_asname))
  } else if (is.na(skickad_x_grupp)) {
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total, fill = chart_col))
  } else {
    p <- plot_df %>% 
      ggplot(aes(x=!!x_var, y=total, fill = as.factor(!!x_grupp)))
    if (!diagram_facet | facet_legend_bottom) legend_pos <- "bottom" else legend_pos <- "none"
  }
  # en möjlighet att ta bort legenden oavsett andra val
  if (legend_tabort) legend_pos <- "none"
  
  p <- p +
    {if (antal_grupper > 3) {
      geom_bar(stat="identity",width = 0.6, position = geom_bar_position)
      #scale_x_discrete()
    } else if (!is.na(x_var_fokus)){
      #geom_bar(position = "dodge", stat="identity", fill = plot_df$fokus)
    } else {
      geom_bar(position = geom_bar_position, stat="identity")
    }} +
    #geom_bar(stat="identity", position = "dodge") +      #position = "dodge", width = 0.25,
    
      {if(dataetiketter){
        
        if (dataetikett_noll_visa_ej){
          # om vi valt parametern dataetikett_noll_visa_ej == TRUE, dvs. att ta bort dataetiketter som visar noll
          geom_text(data = subset(plot_df, total != 0),  aes(y=total+sign(total), x=!!x_var, label=total,
                        vjust = ifelse(total >= 0, -0.5, 1)),
                    color = "#464d48",
                    size=2.3,
                    position = position_dodge(width = 0.9))
          
        } else {
          # om vi kör på som vanligt, dvs. visar nollvärden som dataetiketter  
          geom_text(aes(y=total+sign(total), x=!!x_var, label=total,
                      vjust = ifelse(total >= 0, -0.5, 1)),
                  color = "#464d48",
                  size=2.3,
                  position = position_dodge(width = 0.9))
        }
      }} +
    
    {if (diagram_liggande) coord_flip()} +
    theme(axis.text.x = element_text(size = x_axis_storlek, angle = x_axis_lutning, 
                                     hjust = manual_x_axis_text_hjust, vjust = manual_x_axis_text_vjust),
          axis.text.y = element_text(size = y_axis_storlek),
          axis.ticks = element_blank(),
          legend.position = legend_pos,
          legend.margin = margin(0,0,0,0),
          legend.title = element_text(),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.title.position = "plot",
          plot.caption = element_text(face = "italic",
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size=0.8, colour = "lightgrey"),
          panel.grid.minor.y = element_line(size=0.4, colour = "lightgrey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    {if (diagram_liggande) { 
      theme(panel.grid.major.x = element_line(size=0.8, colour = "lightgrey"),
            panel.grid.minor.x = element_line(size=0.4, colour = "lightgrey"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
      }
    } +
    {if (utan_diagramtitel) theme(plot.title = element_blank())} +
    #{if (diagram_liggande) scale_x_discrete(limits = rev(levels(plot_df[x_var])))} + 
    labs(title = diagram_titel,
         caption = diagram_capt,
         x = manual_x_axis_title,
         y = y_titel,
         fill = legend_titel) +
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5)) +
    scale_fill_manual(values = chart_col) +
    #scale_fill_manual(values = c("red", "blue")) +  
    { if (AF_special) {  
      scale_x_continuous(expand = c(0,.3), breaks = seq(1,max(plot_df[x_var]), by = 1))
    }} +
    # en lösning för att ta bort limits om man kör facets
    scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
                                      by = maj_by_yvar),
                         minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
                         labels = etikett_format,
                       limits = c(min_yvar, max_yvar)) +
    {if (diagram_facet) facet_wrap(as.formula(paste("~",facet_grp)), scales = facet_scale) } +
    {if(diagram_facet){
      theme(strip.text = element_text(color = "black", size = 12),
            strip.background = element_blank(),
            axis.text.x = element_text(size = 10))  
    } else {  
      theme(strip.text = element_blank())
    }}
  #p
  
  if (skriv_till_diagramfil){
    # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
    bredd <- diagramfil_bredd
    hojd <- diagramfil_hojd
    
    fullpath <- paste0(output_mapp, filnamn_diagram)
    ggsave(fullpath, width = bredd, height = hojd)
    
    # Lägg till logga till diagrammet =======================================
    
    if (lagg_pa_logga){  
      if (is.na(logga_path)) logga_path <- hamta_logga_path()   # hämta sökväg till diagram
      add_logo(
        plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
        logo_path = logga_path, # url or local file for the logo
        logo_position = "bottom right", # choose a corner
        # 'top left', 'top right', 'bottom left' or 'bottom right'
        logo_scale = logo_scale,
        #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
        replace = TRUE
      )
    }
  }
  return(p)
}

SkapaLinjeDiagram <- function(skickad_df, 
                              skickad_x_var, 
                              skickad_y_var, 
                              skickad_x_grupp = NA, 
                              skickad_filter_OR_vect = NA, 
                              skickad_filter_OR_var = NA,
                              diagram_titel = NULL, 
                              output_mapp, 
                              diagram_capt = NULL,             # skicka med en textsträng som hamnar i nedre vänstra hörnet på diagram, som kan beskriva källa, vem som gjort diagrammet etc.
                              berakna_index = FALSE,
                              diagram_facet = FALSE,
                              facet_grp = NA,
                              facet_scale = "free",
                              manual_color = NA,
                              brew_palett = "Greens",
                              manual_y_axis_title = NA,
                              manual_x_axis_title = NA,
                              x_axis_lutning = 45,
                              x_axis_storlek = 10.5,
                              facet_legend_bottom = FALSE,
                              visa_var_x_xlabel = 0,
                              AF_special = FALSE,
                              lagg_pa_logga = TRUE,
                              procent_0_100_10intervaller = FALSE,
                              logga_path = NA,
                              skriv_till_diagramfil = TRUE,
                              filnamn_diagram,
                              utan_diagramtitel = FALSE,
                              y_axis_borjar_pa_noll = TRUE,              # sätt till FALSE om y-axeln ska börja på annat värde än 0
                              y_axis_100proc = FALSE){                   # sätt till TRUE om y-axeln ska vara mellan 0 och 100
  
  # Här skapas variabler som används av de som skickats till funktionen
  x_var <- as.name(skickad_x_var)
  if (!is.na(skickad_x_grupp)) x_grupp <- as.name(skickad_x_grupp) else x_grupp <- NA
  if (is.na(skickad_x_grupp)) grupp_var <- x_var else grupp_var <- as.character(c(x_var, x_grupp))       # Skapa group_by-sträng utifrån om det finns grupperingsvariabel
  if (is.na(facet_grp)) {
    facet_grp <- x_grupp 
  } else {
    facet_grp <- as.name(facet_grp)
    grupp_var <- as.character(c(grupp_var, facet_grp))
  }
  y_var <- as.name(skickad_y_var)
  filter_or <- skickad_filter_OR_vect
  filter_or_var <- skickad_filter_OR_var
  
  # Här skapas ett filter av 
  filter_or <- paste0("'", filter_or, "'") #%>%             # Lägg till ' runt värdena som ska filtreras ut
  filter_or <- paste0(filter_or_var, " == ", filter_or)     # Sätt ihop variabeln som de tillhör, likamedtecken samt variabelvärdena (från raden ovan) 
  filter_or <- paste(filter_or, collapse = " | ")           # Sätt ihop alla värden till en OR-sats
  
  # gruppera variabler inför diagramskapande ===========================================================
  # testa om filter-variabeln är tom
  if (!is.na(skickad_filter_OR_vect)){
    # om filter_or har ett värde
    plot_df <- skickad_df %>% 
      filter(!! rlang::parse_expr(filter_or)) %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE))
  } else {
    # om filter_or är NA
    plot_df <- skickad_df %>% 
      group_by(across(all_of(grupp_var))) %>% 
      summarize(total = sum(!!y_var, na.rm = TRUE))
  }
  
  # beräkna index om det är valt =======================================================
  if (berakna_index){
    plot_df <- plot_df %>% 
      group_by(!!x_grupp) %>% 
      mutate(index := round((total/total[år == min(år)] * 100),0))
    if (is.na(manual_y_axis_title)) y_titel <- paste0(strsplit(skickad_y_var, ",")[[1]][1], ", index 100 = ", min(plot_df$år)) else y_titel <- manual_y_axis_title
    plot_df <- plot_df %>% select(-total) %>% rename(total = index)
  } else {
    
    # ======================================== specialanpassningar av titlar och kategorinamn ===========================
    
    if (!is.na(manual_y_axis_title)) y_titel <- manual_y_axis_title else y_titel <- skickad_y_var
  }
  # =========================================== skapa stödlinje-variabler =============================================
  
  if (y_axis_100proc) max_varde_plot_df <- 100 else max_varde_plot_df <- max(plot_df["total"])    # om vi skickat med att vi vill ha låsa y-axelns maxvärde till 100 så fixar vi det här
  stodlinjer_list <- Berakna_varden_stodlinjer(min_varde =  min(plot_df["total"]), max_varde = max_varde_plot_df, y_borjar_pa_noll = y_axis_borjar_pa_noll, procent_0_100_10intervaller = procent_0_100_10intervaller)  
  min_yvar <- stodlinjer_list$min_yvar
  max_yvar <- stodlinjer_list$max_yvar
  min_by_yvar <- stodlinjer_list$min_by_yvar
  maj_by_yvar <- stodlinjer_list$maj_by_yvar
  
  # om vi vill visa var x:e x-axeletikett
  if (visa_var_x_xlabel > 0) rep_vec <- c(T, rep(F, visa_var_x_xlabel-1))
  
  
  etikett_format <- function(x){
    x <- format(x, big.mark = " ", scientific = FALSE)
    if (!is.na(manual_y_axis_title)){
      if (manual_y_axis_title == "procent") x <- paste0(x, " %")
    }
    return(x)
  }
  
  # diagramfärger
  if (!is.na(skickad_x_grupp)) {
    chart_col <- case_when(
      nrow(unique(plot_df[x_grupp])) == 1 ~ "#4f6228",
      nrow(unique(plot_df[x_grupp])) == 2 ~ c("#9bbb59", "#4f6228")
    )
    if (nrow(unique(plot_df[x_grupp])) > 2) chart_col <- brewer.pal(nrow(unique(plot_df[x_grupp])), brew_palett)
  } else {
    chart_col <- "#4f6228"
  }
  # styr om vi ska köra manuell färgskala som är medskickad samt samma med x- och y-titlar
  if (!is.na(manual_color[1])) chart_col <- manual_color
  if (manual_y_axis_title == "procent" & !is.na(manual_y_axis_title)) y_titel <- NULL
  if (is.na(manual_x_axis_title)) manual_x_axis_title <- NULL
  
  # för att sätta limits i ggplot korrekt
  expand_vekt <- c(0,0)
  limit_min <- ifelse(min_yvar < 0, min_yvar, 0)
  if (!y_axis_borjar_pa_noll) {
    limit_min <- min_yvar
    expand_vekt <- NULL
  }
  
  # ge legend_pos ett standardvärde
  legend_pos <- "none"
  
  # Här börjar vi göra diagrammet ======================================================
  # om x_grupp är tom ta bort den raden, annars kör med den  
  if (is.na(skickad_x_grupp)) {
    p <-plot_df %>% ggplot(aes(x=!!x_var, y=total)) +
      {if (berakna_index) geom_hline(yintercept = 100, color = "grey32", size = 1.2)} +
      geom_line(aes(color = chart_col), size = 1.5)
  } else {
    p<-plot_df %>% ggplot(aes(x=!!x_var, y=total, group = !!x_grupp)) +
      {if (berakna_index) geom_hline(yintercept = 100, color = "grey32", size = 1.2)} +
      geom_line(aes(color = !!x_grupp), size = 1.5)
    if (!diagram_facet | facet_legend_bottom) legend_pos <- "bottom"
  }
  # fortsätt att fylla på objektet p som är diagrammet
  p <- p +
    theme(axis.text.x = element_text(size = x_axis_storlek, angle = x_axis_lutning, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.ticks = element_blank(),
          legend.position = legend_pos,
          legend.key = element_rect(fill = "white"),
          legend.margin = margin(0,0,0,0),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.caption = element_text(face = "italic",
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size=0.8, colour = "lightgrey"),
          panel.grid.minor.y = element_line(size=0.4, colour = "lightgrey") ,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    {if (utan_diagramtitel) theme(plot.title = element_blank())} +
    labs(title = diagram_titel, 
         x = manual_x_axis_title,
         caption = diagram_capt,
         y = y_titel) +
    #scale_color_brewer(palette = "Paired", direction = -1) +
    scale_color_manual(values = chart_col) +  
    { if (AF_special) {  
      scale_x_continuous(expand = c(0,.3), breaks = seq(1,53, by = 1))
    }} +
    {if (visa_var_x_xlabel > 0){
      scale_x_discrete(breaks = plot_df[[x_var]][rep_vec])
    }} +
    # scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
    #                                 by = round(max_yvar / 6, (nchar(trunc(max_yvar/6))-1)*-1)),
    #                    minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
    scale_y_continuous(breaks = seq(min_yvar, max_yvar, 
                                    by = maj_by_yvar),
                       minor_breaks = seq(min_yvar, max_yvar, by = min_by_yvar),
                       labels = etikett_format,
                       #expand = expand_vekt, 
                       limits = c(limit_min,max_yvar)) +
    #labels = function(x) format(x, big.mark = " ")) +
    {if (diagram_facet & x_grupp != "NA") facet_wrap(as.formula(paste("~",facet_grp)), scales = facet_scale) } +
    {if(diagram_facet & x_grupp != "NA"){
      theme(strip.text = element_text(color = "black", size = 12),
            strip.background = element_blank(),
            axis.text.x = element_text(size = x_axis_storlek))  
    } else {  
      theme(strip.text = element_blank())
    }}
  
  # skriv till diagramfil om sådan är vald
  if (skriv_till_diagramfil){
    # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
    bredd <- 12
    hojd <- 7
    
    fullpath <- paste0(output_mapp, filnamn_diagram)
    ggsave(fullpath, width = bredd, height = hojd)
    
    # Lägg till logga till diagrammet =======================================
    if (lagg_pa_logga) {
      if (is.na(logga_path)) logga_path <- hamta_logga_path()       # hämta logga_path i funktion först i denna fil
      if (!is.null(logga_path)){  
        add_logo(
          plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
          logo_path = logga_path, # url or local file for the logo
          logo_position = "bottom right", # choose a corner
          # 'top left', 'top right', 'bottom left' or 'bottom right'
          logo_scale = 15,
          #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
          replace = TRUE
        )
      } # if !is.null(logga_path)
    } # if lagg_pa_logga
  } # if skriv_till_diagramfil
  return(p)
}
