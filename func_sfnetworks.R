  
skapa_sf_nat <- function(sf_obj, 
                         skapa_en_component = FALSE, 
                         stada_natverk = TRUE, 
                         crs = 3006,
                         directed_net = FALSE){
  if (directed_net){
  skapat_sfnatverk <- as_sfnetwork(sf_obj) %>% 
    st_transform(crs)
  } else {
  skapat_sfnatverk <- as_sfnetwork(sf_obj, directed = FALSE) %>% 
    st_transform(crs)  
  }
  
  # skapa variabel för från-till för varje länk (båge) ett sorts edges-ID
  skapat_sfnatverk <- skapat_sfnatverk %>% 
    activate(edges) %>% 
    mutate(edge_lbl = paste0(from, "-", to))
  
  # skapa variabel för node-ID
  skapat_sfnatverk <- skapat_sfnatverk %>% 
    activate(nodes) %>% 
    mutate(nodeID = row_number())
  
  # ================== städa nätverket litegrann ===============================
  
  if (stada_natverk) {
    skapat_sfnatverk = skapat_sfnatverk %>%
      activate("edges") %>%
      arrange(edge_length()) %>% 
      filter(!edge_is_multiple()) %>%
      filter(!edge_is_loop())
  }
  
  # Our network consists of several unconnected components.
  antal_component <- with_graph(skapat_sfnatverk, graph_component_count())

  if (skapa_en_component & antal_component > 1) {
    skapat_sfnatverk <- skapat_sfnatverk %>%
      activate("nodes") %>%
      filter(group_components() == 1)
  }
  return(skapat_sfnatverk)
}
