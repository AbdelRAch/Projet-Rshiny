Objectif principal : Créer une application Shiny permettant de visualiser la consommation et la production d’énergie à l’échelle demi-horaire ou quotidienne, pour une région ou un ensemble de régions.

Une interface de trois onglets :

Conso < 36kVA : Consommation pour les utilisateurs avec des puissances inférieures à 36 kVA.
Conso >= 36kVA : Consommation pour les utilisateurs avec des puissances égales ou supérieures à 36 kVA.
Production : Production d’énergie avec différentes plages de puissance et filières.

Dans le cadre de ce projet, nous avons commencé par diviser les tâches en trois applications distinctes, chacune étant gérée par un membre de l’équipe :
une application pour les consommations inférieures à 36kVA, une autre pour les consommations supérieures ou égales à 36kVA, et enfin une pour la production.
Ces applications étaient fonctionnelles indépendamment les unes des autres, ce qui nous a permis de tester et de valider chacune dans son propre contexte.

L’étape actuelle consiste à fusionner ces trois applications en une seule application Shiny. 
Nous avons déjà établi les bases de cette fusion et sommes en train de structurer l’interface utilisateur et de relier les différentes fonctionnalités à partir des applications existantes.

Ce qui a été fait :

Création des trois applications indépendantes : 
Chaque membre a été responsable de la mise en place d’une application pour une des catégories de données (consommation <36kVA, consommation >=36kVA, et production).
Interface utilisateur de base : chaque membre de groupe a développé une interface simple permettant de sélectionner les différentes options de filtre (période, régions, profils, plages de puissance, etc.), 
selon les critères spécifiés pour chaque catégorie de données.
Affichage des graphiques interactifs : Des graphiques ont été générés pour visualiser les courbes de consommation ou de production, avec des couleurs différenciées selon les régions,
profils ou autres critères de filtrage.

Ce qui reste à faire :

Fusion les applications : L’essentiel du travail restant consiste à intégrer les trois applications dans une seule interface Shiny. 
Cela inclut la gestion de la navigation entre les différents onglets (conso <36kVA, conso >=36kVA, production) et la cohérence dans les filtres
de données qui devront être appliqués simultanément dans l’ensemble de l’application.

Amélioration de l’interface utilisateur : Nous devons perfectionner les éléments interactifs comme les checkboxes, radio buttons, et les daterangeinput, 
pour que l’utilisateur puisse facilement naviguer entre les différentes options et voir les résultats correspondant à ses critères.

