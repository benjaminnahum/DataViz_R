# Titre du projet
Les jeux vidéo dans le monde de 1980 à 2016

## Description du contexte

Étant de grands adeptes de jeux vidéo, nous avons fait le choix de réaliser une étude sur ce sujet. 
Cette base de donnée contient un grand nombre d'informations au sujet des ventes, des plateformes de jeux ou encore des éditeurs.


## Description de la base de donnée

Cette base de donnée contient:

- Rank : Les jeux vidéo sont classés en fonction des ventes globales dans un ordre décroissant
- Name : Nom des jeux vidéo
- Platform : Nom des consoles de jeux
- Year : Années de commercialisation des jeux
- Genre : Catégories des jeux vidéo
- Publisher : Éditeurs des jeux vidéo
- NA_Sales : Ventes globales des jeux vidéo aux Etats-Unis
- EU_Sales : Ventes globales des jeux vidéo en Europe
- JP_Sales : Ventes globales des jeux vidéo aux Japon
- Other_Sales : Ventes globales des jeux vidéo hors Europe, Etats-Unis et Japon
- Global_Sales : Ventes globales des jeux vidéo dans le monde

Lien vers le dataset : https://www.kaggle.com/gregorut/videogamesales

## Installation des packages R

- install.packages('shiny')
- install.packages("shinydashboard")
- install.packages("plotly")
- install.packages("ggplot2")
- install.packages("dplyr")


## Le script

Fichiers concernés : 
- vgsales.csv
- jeux_video.Rproj
- jeux_video.R

Dans RStudio ouvrir le projet jeux_video.Rproj 
puis ouvrir le fichier jeux_video.R dans la case en bas à droite

Pour executer le fichier: 

- Dans la fenêtre du fichier jeux_video.R appuyez sur la commande "Run App"
- La commande va exécuter l'ui, le server et le shinyApp(ui = ui, server = server).
- Le dashboard va s'ouvrir automatiquement et vous pourrez alors le parcourir. 


## Conclusion de l'étude

Nous pouvons tirer les conclusions suivantes de cette étude :
        
- Depuis 1980 jusqu'a 2016, les catégories qui ont vendu le plus sont les jeux d'action et de sport
- Nintendo, Electronic Arts et Activison ont vendu à eux 3 environ 50% des jeux depuis 1980
- La PS2 et la XBOX sont les deux consoles qui ont vendus le plus de jeux vidéo mais il faut noter que la WII a connu un succès fulgurant lors de sa sortie avec 137 millions de ventes la première année
- Les jeux vidéos étaient à leur apogée dans les années 2006 à 2009 puis sont moins important aujourd'hui surement à cause de la hausse des prix des consoles et à la democratisation des jeux sur smartphone.
