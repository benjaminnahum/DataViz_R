#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)
library(datasets)
library(ggthemes) 

#On charge le fichier csv dans une dataframe nommée data
data <- read_csv("vgsales.csv")

data2 = select(data,Name,Platform,Global_Sales) #On selectionne trois colonnes sur la Dataframe de départ.
a <-c(filter(data2, Global_Sales >= 20.5)) #On filtre sur ces colonnes les ventes globales supérieur ou égal à 20.5 millions.
df1 <- as_tibble(a) #Méthode de tydiverse permettant de créer une Dataframe.

#Histogramme des jeux les plus vendus
jeuleplusvendu <- ggplot(df1, aes(x=reorder(Name, Global_Sales), y =Global_Sales))
jeuleplusvendu <- jeuleplusvendu + geom_bar(stat='identity')
jeuleplusvendu <-  jeuleplusvendu + xlab("Nom des jeux") + ylab("Ventes globales") + theme(axis.text.x = element_text(angle = 45))
jeule <- ggplotly(jeuleplusvendu)
jeule <- style(jeuleplusvendu,hoverinfo = 'y')#hoverinfo permet de rendre intercatif le graphique en affichant une information sur chaque valeur.


 
b <- group_by(data, Platform) %>% summarise(Global_Sales = sum(Global_Sales)) #On associe à la nouvelle variable b la somme des ventes globales en fonction de la plateforme
df2 <- as_tibble(b) #Méthode de tydiverse permettant de créer une Dataframe.

#Histogramme des plateformes avec le plus grand nombres de ventes
p2 <- ggplot(df2,aes(x= reorder(Platform, Global_Sales), y =Global_Sales))
p2 <- p2 + geom_bar(stat='identity') 
p2 <- p2 + xlab("Console de jeu") + ylab("Ventes globales")+ theme(axis.text.x = element_text(angle = 45))
p <- ggplotly(p2)
p <- style(p,hoverinfo = 'y') #hoverinfo permet de rendre intercatif le graphique en affichant une information sur chaque valeur.



#Camembert des consoles de jeu avec le plus grand nombre de ventes
dfconsole = c(filter(df2, Global_Sales >= 150)) #On associe à la nouvelle variable dfconsole un filtre permettant de récuperer seulement les valeurs de Global_Sales supérieur a 150.
dfconsole2 <- as_tibble(dfconsole)
dfconsole2
camconsole<- plot_ly(dfconsole2, labels = ~Platform, values = ~Global_Sales, type = 'pie') #On crée le camembert grace a plotly et on crée la légende ainsi que les valeurs que l'on souhaite associé a ce camembert.
camconsole <- camconsole %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), #Légende du camembert
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))








data3 = select(data,Name,Platform,NA_Sales) #On selectionne les colonnes PLatform et NA_Sales sur la DataFrame de départ
c <-c(filter(data3, NA_Sales >= 10)) #On filtre les valeurs de NA_Sales supérieur ou égal a 10 millions.
df3 <- as_tibble(c)

#Histogramme des jeux les plus vendus aux Etats-Unis
jeuUS <- ggplot(df3, aes(x = reorder(Name, NA_Sales), y = NA_Sales ))
jeuUS <- jeuUS + geom_bar(stat='identity')
jeuUS <- jeuUS + ggtitle('Histogramme des jeux les plus vendus aux Etats-Unis') + xlab("Nom des jeux") + ylab("Ventes globales") + theme(axis.text.x = element_text(angle = 45))
u <- ggplotly(jeuUS)
u <-style(jeuUS, hoverinfo = 'y')





data4 = select(data,Name,Platform,EU_Sales) #On selectionne les colonnes PLatform et EU_Sales sur la DataFrame de départ.
d <-c(filter(data4, EU_Sales >= 8)) #On filtre les valeurs de EU_Sales superieur ou égal a 8.
df4 <- as_tibble(d)

#Histogramme des jeux les plus vendus en Europe
jeuEU <- ggplot(df4, aes(x = reorder(Name, EU_Sales), y = EU_Sales))
jeuEU <- jeuEU + geom_bar(stat='identity')
jeuEU <- jeuEU + ggtitle('Histogramme des jeux les plus vendus en Europe') + xlab("Nom des jeux") + ylab("Ventes globales")  + theme(axis.text.x = element_text(angle = 45))
e <- ggplotly(jeuEU)
e <-style(jeuEU, hoverinfo = "y")



data5 = select(data,Name,Platform,JP_Sales)
jj <-c(filter(data5, JP_Sales >= 4))
df5 <- as_tibble(jj)

#Histogramme des jeux les plus vendus au Japon
jeuJP <- ggplot(df5, aes(x = reorder(Name,JP_Sales), y = JP_Sales ))
jeuJP <- jeuJP + geom_bar(stat='identity') + ggtitle('Histogramme des jeux les plus vendus au Japon')
jeuJP <- jeuJP + xlab("Nom des jeux") + ylab("Ventes globales")  + theme(axis.text.x = element_text(angle = 35))
j <- ggplotly(jeuJP)
j <-style(jeuJP,hoverinfo = "y")



#Evolution du nombre de ventes au fil des années
data6 = select(data,Year,Global_Sales)
f <- group_by(data, Year) %>% summarise(Global_Sales = sum(Global_Sales)) #On associe a la variable f la somme des ventes globales en fonction de l'année.
sup <- c(f %>% filter(Year != "N/A")) #On filtre les valeurs des années pour ne récuperer que les valeurs qui ne sont pas égal à "N/A".
sup2 <- as_tibble(sup)
evolutionglobale <- ggplot(sup2,aes(x = Year,y = Global_Sales, size= Global_Sales))
evolutionglobale <- evolutionglobale + geom_bar(stat='identity')
evolutionglobale <- evolutionglobale + xlab("Année") + ylab("Ventes globales")+ theme(axis.text.x = element_text(angle = 45))
evolution <- ggplotly(evolutionglobale)
evolution <-style(evolutionglobale,hoverinfo = "y")






#Evolution du nombre de ventes au fil des années en Europe
data7 = select(data,Year,EU_Sales)
f1 <- group_by(data7, Year) %>% summarise(EU_Sales = sum(EU_Sales))
sup3 <- c(f1 %>% filter(Year != "N/A"))#On filtre les valeurs des années pour ne récuperer que les valeurs qui ne sont pas égal à "N/A".
sup4 <- as_tibble(sup3)
evolutioneurope <- ggplot(sup4,aes(x = Year,y = EU_Sales))
evolutioneurope <- evolutioneurope + geom_bar(stat='identity')
evolutioneurope <- evolutioneurope + xlab("Année") + ylab("Ventes globales") + theme(axis.text.x = element_text(angle = 45))
evolutioneu <- ggplotly(evolutioneurope)
evolutioneu <-style(evolutioneu,hoverinfo = "y")



#Evolution du nombre de ventes au fil des années au Japon
data8 = select(data,Year,JP_Sales)
f2 <- group_by(data8, Year) %>% summarise(JP_Sales = sum(JP_Sales))
sup5 <- c(f2 %>% filter(Year != "N/A"))
sup6 <- as_tibble(sup5)
evolutionjapon <- ggplot(sup6,aes(x = Year,y = JP_Sales))
evolutionjapon <- evolutionjapon + geom_bar(stat='identity')
evolutionjapon <- evolutionjapon + xlab("Année") + ylab("Ventes globales") + theme(axis.text.x = element_text(angle = 45)) #Le nom des jeux vidéo étant trop grand, nous avons utilisé un theme permettant de mettre a 45 degrés la légende.
evolutionjp <- ggplotly(evolutionjapon)
evolutionjp <-style(evolutionjp,hoverinfo = "y")



#Evolution du nombre de ventes au fil des années au Etats-Unis
data9 = select(data,Year,NA_Sales)
f3 <- group_by(data9, Year) %>% summarise(NA_Sales = sum(NA_Sales))
sup7 <- c(f3 %>% filter(Year != "N/A"))
sup8 <- as_tibble(sup7)
evolutionusa <- ggplot(sup8,aes(x = Year,y = NA_Sales))
evolutionusa <- evolutionusa + geom_bar(stat='identity')
evolutionusa <- evolutionusa + xlab("Année") + ylab("Ventes globales") + theme(axis.text.x = element_text(angle = 45))
evolutionus <- ggplotly(evolutionusa)
evolutionus <-style(evolutionus,hoverinfo = "y")


#Graphique du nombres de ventes des jeux selon l'éditeur
f4 <- group_by(data, Publisher) %>% summarise(Global_Sales = sum(Global_Sales))
f5 <-c(filter(f4, Global_Sales >= 100))
df10 <- as_tibble(f5)
editeur <- ggplot(df10,aes(x = Publisher,y = Global_Sales))
editeur <- editeur + geom_bar(stat='identity')
editeur <- editeur + xlab("Éditeur") + ylab("Ventes globales")  + theme(axis.text.x = element_text(angle = 45))
edi <- ggplotly(editeur)
edi <-style(edi,hoverinfo = "y")


#Camembert des éditeurs avec le plus grand nombre de ventes
edi2 <- plot_ly(df10, labels = ~Publisher, values = ~Global_Sales, type = 'pie')
edi2 <- edi2 %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




#Evolution du nombres de ventes des jeux au fil des années en fonction des differentes plateformes
f6 <- group_by(data, Platform, Year) %>% summarise(Global_Sales = sum(Global_Sales))
sup11 <- c(f6 %>% filter(Year != "N/A"))
sup12 <- as_tibble(sup11)
mx <- ggplot(sup12,aes(x = Year,y = Global_Sales, group = Platform, color = Platform))
mx <- mx + geom_line()  + geom_point()
mx <- mx + xlab("Années") + ylab("Ventes globales")+ theme(axis.text.x = element_text(angle = 45))
mx <- ggplotly(mx)




#Histogramme du nombre de ventes en fonction des genres de jeux vidéos
genre = count(data, Genre)
mx2 = ggplot(genre, aes(x = Genre,y=n))
mx2 = mx2 + geom_histogram(stat = 'identity')
mx2 <- mx2 + xlab("Genre") + ylab("Nombre de jeux") + theme(axis.text.x = element_text(angle = 45))
mx3 <- ggplotly(mx2)
mx3 <-style(mx3,hoverinfo = "y")

#graphique du nombre de ventes global par années en fonction des platformes
#data <- data.frame(data)
dfplat <- group_by(data,Platform,Year) %>% summarise(g_sales = sum(Global_Sales))
dfplat


#Time de vente d chaque platformes
dfplat2 <- (data %>% filter(Year != "N/A"))
dfplat2 <- group_by(dfplat2,Platform) %>% summarize(year_min = min(Year),year_max = max(Year))

time_plat <- ggplot(dfplat2) + geom_segment(aes(x=year_min,xend= year_max,y=Platform,yend= Platform))          
time_plat <- time_plat + xlab("Années") + ylab("Plateformes")
time_plat <- ggplotly(time_plat)
time_plat <- plotly_build(time_plat)



l =list(Monde = list(df1,jeule),USA = list(df3,u), #On crée la liste des éléments que l'on va placé dans notre dropdown, on associe les graphiques correpondant 
        Europe = list(df4,e),Japon = list(df5,j)) #aux jeux les plus vendus par continent.

choice_data <- names(l) 

l2 =list(Monde = list(sup2,evolution),USA = list(sup8,evolutionus), #On crée la liste des éléments que l'on va placé dans notre dropdown, on associe les graphiques correpondant 
         Europe = list(sup4,evolutioneu),Japon = list(sup6,evolutionjp)) #à lévolution du nombre de ventes au fils des années selon le continent.

choice_data <- names(l2)




  ui <- dashboardPage( #Création de la dashboard.
    dashboardHeader( #Création de l'en tête.
      title = "Étude des jeux vidéo dans le monde de 1980 à 2016",
      titleWidth = 500
    ),
    dashboardSidebar( #Création d'une Sidebar dans laquelle on rajoute 5 rubriques.
      sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("play")),
        menuItem("Ventes globales", tabName = "vente", icon = icon("money-bill-wave")),
        menuItem("Plateformes", tabName = "platforme", icon = icon("gamepad")),
        menuItem("Vision globale", tabName = "annee", icon = icon("globe")),
        menuItem("Conclusion", tabName = "conclusion", icon = icon("fast-forward"))
       
      )
    ),
    dashboardBody( 
      tabItems( #Fonction qui regroupe tout les tabItem
        tabItem( #Fonction permettant de lié la rubrique a la page et contient toutes les fonctionnalités de la page
          "intro",
          tabBox( #Box contenant une introduction.
            width = 15,
            tabPanel(title = "Introduction", "Ce Dashboard constitue une étude des jeux vidéo dans le monde de 1980 à 2016. 
                     Nous avons fait le choix de ce sujet car nous sommes tous deux de grands adeptes de jeux vidéo. 
                     À travers les différents graphiques et interprétations, nous allons mettre en évidence les genres de jeux vidéo les plus prisés, les jeux les plus vendus selon la zone géographique ou encore les plateformes les plus rentables de ces dernières années. 
                     Nous avons fait le choix de créer 3 onglets dans lesquels se mêlent différents types de données sous différentes formes.")
            
          ),),
        tabItem(
          "vente",
          box( #Box contenant le graphique
            title = "Histogramme du nombre de jeux développés en fonction de leurs genre",
            status = "info",
            solidHeader = TRUE,
            width = 8, #Paramètre permettant de choisir la taille de la Box.
            plotlyOutput("hist1")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Cette histogramme nous montre les catégories de jeux qui ont été le plus developpé par les éditeurs depuis 1980.
                                On remarque que la plupart des jeux developpés sont des jeux d'actions et de sports. Il est intérressant de voir que ces deux catégories ressortent le plus et cette étude à sûrement dû 
                                aider les éditeurs de jeux pour cibler quels genres plaisaient le plus aux gamers et pour developper des jeux en fonction. Personnellement nous pensions que le genre Shooter pesait beaucoup plus
                                sachant qu'aujourd'hui la plupart des jeunes jouent à des jeux de guerre comme Call of duty, Fortnite etc...
                                mais il ne faut pas oublier que notre dataset remonte les jeux depuis 1980 et qu'en 1980 on ne jouait pas aux même jeux qu'aujourd'hui.
                                Vous pouvez survolez le graphique pour voir en detail le nombre de jeux créés dans chaque catégorie.")
            
          ),
          box(
            title = "Histogramme des jeux les plus vendus selon le continent",
            footer = "en millions",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            selectInput("Continent","Continent",choices = choice_data),textOutput("selected_var"), uiOutput("test") #Création du dropdown
          ),
          tabBox( #Box contenenat une interprétation du graphique
            width = 4,
            tabPanel(title = "Interprétation", "Ces 4 histogrammes permettent de montrer les jeux vidéo ayant le plus grand nombre de ventes en fonction de 4 zones géographiques différentes, les États-Unis, le Japon, l'Europe ainsi que le monde entier. 
                     Nous pouvons observer que les jeux sont assez différents en fonction du continent. Le jeu « Wii Sports » est par exemple le jeu le plus vendus aux États-Unis et en Europe mais ne fait pas partie du top 10 des jeux les plus vendus au Japon. 
                     Ce graphique constitué de 4 histogrammes permet de mettre en évidence le choix assez différents des jeux choisis par la population en fonction des continents.")
            
          ),
          box(
            title = "Graphique du nombre de ventes des jeux selon l'éditeur",
            footer = "en million",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("hist5")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Cet histogramme permet de mettre en évidence les éditeurs avec le plus grand nombre de ventes de jeux vidéo. 
                     Nous pouvons observer que Nintendo est très largement en tête avec plus de 1780 millions de ventes de jeux depuis sa création. 
                     Nintendo est l’éditeur à l’origine de jeux comme « Mario » qui connaissent un succès assez important dans les principaux pays du monde. 
                     C’est une entreprise très ancienne datant de 1889 et à l’origine de la création de plus de milliers de jeux ainsi que la création de nombreuses consoles de jeux. 
                     Les éditeurs Electronics Arts et Activision occupent quant à eux la deuxième et troisième place avec respectivement 1130 et 727 millions de jeux vendus.")
            
          ),
          box(
            title = "Évolution du nombre de ventes des jeux au fil des années selon l'éditeur",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("cam1")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Ce camembert nous offre une vision différente de l'histogramme précédent. 
                     Nous pouvons observer que Nintendo, Electronic Arts, Activison et Sony Computer Entertainment ont vendus à eux 4 environ 60% des jeux depuis 1980. 
                     À eux seuls les 4 éditeurs réalisent un plus grand nombre de ventes que plus de 15 éditeurs réunis. 
                     Nous pouvons aussi noter que Nintendo réalise environ un quart de toutes les ventes depuis 1980. 
                     Cela peut s’expliquer car les budgets ne sont pas les mêmes pour tout le monde, certaines firmes ne sortent ou ont sorti que très peu de jeux depuis leurs créations. ")
            
          )
        ),
        tabItem(
          "platforme",
          box(
            title = 'Histogramme des plateformes avec le plus grand nombre de ventes selon le continent',
            footer = "en million",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("hist3")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Cet histogramme permet de montrer les plateformes de jeux avec le plus grand nombre de ventes. 
            Ici, nous pouvons observer que la PS2 est la console qui a vendu le plus de jeux avec plus de 1255 millions de ventes. 
            Nous pouvons observer que les consoles de salon Playstation de la marque Sony sont toutes présentes dans le top 6. 
            La somme de la vente des 3 consoles est égal a  plus de 2900 millions. 
            Ce qui fait de Sony l’éditeur ayant vendu le plus de consoles de jeux.")
          ),
          box(
            title = "Camembert des consoles de jeu avec le plus grand nombre de ventes",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("hist4")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Ce camembert est une vision différente de la figure précédente, il permet d'afficher sous la forme d'un camembert les plateformes ayant vendu le plus de jeux. 
            La PS2, la XBOX 360, La PS3 et la WII sont les plateformes constituant plus de 50% de la vente des jeux vidéo. 
                     Nous pouvons observer que 6 plateformes se détachent en nombre de ventes des autres. 
                     À elles 6, elles constituent environ 70% de la ventes de consoles de jeux. 
                     La Wii et la DS sont deux consoles créées par Nintendo et elles représentent 20% des ventes. 
                     Sony Computer Entertainment réalise 34% des ventes.")
          ),
          box(
            title = "Évolution du nombre de ventes des jeux au fil des années pour chaque plateforme",
            footer = "en million",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hist14")
          ),
          box(
            title = "Timeline de ventes de jeux sur chaque platforme",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("hist16")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Les deux graphiques ci-dessus ont un lien et c'est pour cela que l'on va les étudier ensemble. 
                     Le deuxième graphique reprensente en réalité la durée de commercialisation des jeux vidéo pour chaque plateforme et donc logiquement la durée de vie de toutes les plateformes.
                     Si les éditeurs ont décidé de ne plus sortir de jeux vidéo pour telle plateforme c'est que cette plateforme devenait obsolete et qu'elle n'interressait plus personne.
                     On peut donc se rendre facilement compte grace au graphique les plateformes qui ont réussi à s'imposer dans la durée. Nous pouvons que constater l'impressionnante domination de la DS
                     et du PC dans le temps. Il est vrai que la DS et le PC ont connu des évolutions logiciel mais pas au niveau du nom contrairement à la PS2 qui est devenu la PS3 par exemple.
                     Le premier graphique represente les ventes globales des jeux vidéo en fonction des années pour chaque plateforme. Si on regarde le second graphique on a accès aux dates de sorties de chaque plateforme
                     car logiquement l'année où le premier jeu video est sorti sur une plateforme correspond à la date de sortie de la pateforme elle même. Donc si on regarde la date de sortie de la Wii par exemple (2006)
                     et qu'on la reporte sur le premier graphique, nous obsevons que 137 millions de jeux vidéo se sont vendus pour la wii la premiere année. La PS2 arrive en deuxième position avec 39 millions de jeux 
                     vidéo vendus la premiere année. L'ecart est très conséquent et on se rend compte de l'impact de la wii l'année de son lancement. Il est vrai que de nos jours la wii est une console qui a touché toutes les générations.")
          )
          
        ),
        tabItem(
          "annee",
          box(
            title = 'Évolution du nombre de ventes des jeux au fil des années selon la zone géographique',
            footer = "en million",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            selectInput("Continent2","Continent",choices = choice_data),textOutput("selected_var2"), uiOutput("test2")
          ),
          tabBox(
            width = 4,
            tabPanel(title = "Interprétation", "Cette histogramme nous offre une vision globale de l'importance des jeux vidéo au niveau mondial et pour differents continent dans le monde.
                     Dans le monde on peut voir que les jeux vidéo n'ont cessé de se democratimer jusqu'en 2008 où l'on a atteint le pique. Depuis 2008 il y a une baisse considerable d'années en années.
                     Au États-Unis, on retrouve cette même tendence avec une democratisation jusqu'en 2008 puis une baisse à partir de cette date. On remarque qu'en Europe les jeux ont eu beaucoup de mal à se democratiser
                     au debut entre 1980 à 1995. Après cette date cela s'est vite democratisé jusqu'en 2009 et il y a eu ensuite une baise importante jusqu'en 2016. On constate que l'Europe était donc legèrement en retard
                     par rapport aux États-Unis à propos des jeux vidéo. Enfin pour le Japon on constate que les jeux vidéos se sont democratisés beaucoup plus vite qu'au USA et qu'en Europe. Le pique a eu lieu en 2006 ce qui 
                     témoigne de leur avance.
                     Pour tous les continents on peut peut etre expliquer une baisse de vente des jeux vidéos à partir des années 2006-2009 par la hausse des prix des consoles et des jeux vidéos. Il est vrai qu'aujourd'hui jouer au jeux vidéos est devenue un privilège
                     et qu'il faut debourser beaucoup d'argents dans du matériels pour pouvoir jouer.
                     ")
          )
          
          
        ),
          tabItem(
            "conclusion",
            tabBox(
              width = 15,
              tabPanel(title = "Conclusion", "Nous pouvons tirer les conclusions suivantes de cette étude :
                       - Depuis 1980 jusqu'a 2016, les catégories qui ont vendu le plus sont les jeux d'action et de sport
                       - Nintendo, Electronic Arts et Activison ont vendus à eux 3 environ 50% des jeux depuis 1980
                       - La PS2 et la XBOX sont les deux consoles qui ont vendus le plus de jeux vidéo mais il faut noter que la WII a connu un succès fulgurant lors de sa sortie avec 137 millions de vente la première année
                       - Les jeux vidéos étaient à leur apogée dans les années 2006 à 2009 puis sont moins important aujourd'hui surement grâce à la hausse des prix des consoles et à la democratisation des jeux sur smartphone.")
              
            ),
            )
      )
    ),
    title = "jeux vidéo",
    skin = "blue" #Couleur de l'en tête.
  )
  
  
  server <- function(input, output) {
    
    output$hist1 <- renderPlotly({ #On associe a hist1 le graphique que l'on souhaite afficher.
      
      mx3 
      
    })
    
    output$test <- renderUI({ #On associe a test la fonction qui va permettre d'afficher plusieurs graphiques en meme temps avec le dropdown.
      
      l[[input$Continent]][2]
      
    })
    
    output$hist5 <- renderPlotly({
      
      edi
      
    })
    
    output$cam1 <- renderPlotly({
      
      edi2
      
    })
    
    output$hist3 <- renderPlotly({
      
      p
      
    })
    
    output$hist4 <- renderPlotly({
      
      camconsole
      
    })
    
    output$hist14 <- renderPlotly({
      
      mx
      
    })
    
    output$hist16 <- renderPlotly({
      
      time_plat
      
    })
    
    output$test2 <- renderUI({
      l2[[input$Continent2]][2]
      
    })
    
  }

shinyApp(ui = ui, server = server) #Création de la page internet.
