fitness_hcluster <- function(x){
  
  id_hcluster <- fastcluster::hclust(dist_matrix, method = "complete") |> 
    cutree(h = x)
  
  out <- feat |> 
    st_drop_geometry() |> 
    as_tibble() |> 
    dplyr::mutate(id_hcluster = id_hcluster) |> 
    group_by(id_hcluster) |> 
    summarise(area = sum(area, na.rm = TRUE),
              list_of_commodities = fun_collapse_groups(list_of_commodities),
              group_size = str_split(list_of_commodities, pattern = ",") |> unlist() |> length()) |> 
    dplyr::mutate(group_type = ifelse(is.na(list_of_commodities), "Unknown", ifelse(group_size == 1, "Single host", "Companion"))) |> 
    dplyr::group_by(group_type) %>% 
    dplyr::summarise(n = dplyr::n(), area = sum(area)) %>% 
    dplyr::mutate(n.perc = n/sum(n)*100, area.perc = area/sum(area)*100)
  
  y <- numeric(2)
  y[1] <- out$area[out$group_type=="Companion"]
  y[2] <- out$area[out$group_type=="Unknown"]
  
  return(y)
}
