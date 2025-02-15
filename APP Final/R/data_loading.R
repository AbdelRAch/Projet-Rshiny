library(dplyr)
library(lubridate)
library(hms)
library(data.table)
library(janitor)
library(sf)

load_data <- function() {
  
  # chargement des données:
  conso_sup36<- fread("data/conso-sup36-region.csv")%>% clean_names()
  conso<- fread("data/conso-inf36-region.csv")%>% clean_names()
  df<- fread("data/prod-region.csv")%>% clean_names()
 
  df <- df[complete.cases(df),] %>%
    mutate(
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = as_hms(format(horodate_clean, format = "%H:%M:%S")),
      total_energie_injectee_wh = as.numeric(total_energie_injectee_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(16:18, 2:15))
  
  
  conso <- conso[complete.cases(conso),] %>%
    mutate(
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = as_hms(format(horodate_clean, format = "%H:%M:%S")),
      total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(16:18, 2:15))
  
  conso_sup36 <- conso_sup36[complete.cases(conso_sup36),] %>%
    mutate(
      horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
      date = as.Date(horodate_clean),
      heure = format(horodate_clean , format = "%H:%M:%S"),
      total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
      courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
      courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
    ) %>%
    select(c(17:19, 2:16))
  
  df <- df %>% arrange(horodate_clean)
  conso <- conso %>% arrange(horodate_clean)
  conso_sup36 <- conso_sup36 %>% arrange(horodate_clean)
  
  regions_geo <- st_read("data/regions.geojson")
  regions_geo$code <- as.numeric(regions_geo$code)
  
  return(list(conso_sup36 = conso_sup36, conso = conso, df = df, regions_geo = regions_geo))
}
