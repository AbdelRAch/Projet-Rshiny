library(ggplot2)
library(dplyr)
# Fonction pour générer les courbes de consommation et production:
courbe_moyenne_plots <- function(data,data1,data2,regions,filiere,puissance,puissance1,puissance2) {
  
  # Filtrer et transformer les données pour 'data'(Production)
  dd<-data %>%
    filter(region %in% regions,
           filiere_de_production == filiere,
           plage_de_puissance_injection == puissance)%>%
    mutate(total_energie=total_energie_injectee_wh/nb_points_injection)%>%
    group_by(date,region,plage_de_puissance_injection) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Filtrer et transformer les données pour 'data1'(Consommation supérieur à 36K)
  dd1<-data1 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance1)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Filtrer et transformer les données pour 'data2'(Consommation inférieur à 36K)
  dd2<-data2 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance2)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Création des graphiques pour la consommation inférieure à 36k:
  conso_inf <- ggplot(dd1, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée <=36k",
         x = "date",
         y = "Consommation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  # Création des graphiques pour la consommation supérieure à 36k:
  conso_sup <- ggplot(dd2, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée >36k",
         x = "date",
         y = "Consommation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  
  # Création des graphiques pour la production:
  production <- ggplot(dd, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne injectées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne injectées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale d'énergie injectée",
         x = "date",
         y = "Production",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  # Retourner la liste de tous les graphiques:
  list(conso_inf, conso_sup, production)
}