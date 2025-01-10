---
title: "movies recommendation"
author: "Carpentier Lola, De Oliveira Corentin, Reynaud Valentin"
date: "2025-01-10"
output:
    pdf_document:
        toc: true 
    html_document:
        toc: true 
___

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, error = FALSE, collaps= TRUE)
```


# Movie-recommendation
Hands-on for APE masters students 

# Introduction

Nous vous présentons notre projet de développement d’un système interactif de recommandation de films basé sur les préférences de l’utilisateur. Ce projet utilise le langage **R** et le package **Shiny**, une bibliothèque qui permet de créer des applications web interactives. L’objectif principal était de concevoir une interface conviviale où l’utilisateur peut :

-   Sélectionner une émotion qu’il souhaite ressentir.
-   Indiquer de combien de temps il dispose pour regarder un film.
-   Recevoir des recommandations personnalisées.
-   Choisir son film préféré et visionner sa bande-annonce directement dans le navigateur.

Passons maintenant au détail de la construction du programme, que nous allons expliquer étape par étape.

# Installer les packages nécessaires

Dans la première partie du code, nous installons et chargeons les packages nécessaires: **Shiny** : pour créer l'interface utilisateur et le serveur interactif. **Readr** : pour lire les données contenues dans le fichier pelletier_emotions.csv.

```{r packages, message=FALSE, warning=FALSE, include=FALSE, results='hide'}
install.packages("shiny")
install.packages("readr")
library(shiny)
library(readr)

```

# Charger les données

Voici la ligne de code importante:

```{r donnees, message=FALSE, warning=FALSE, include=FALSE }
movies <- read_csv("pelletier_emotions.csv")

```

Ce fichier contient les informations sur les films, comme le titre, le genre, la durée, l’émotion associée, et le lien vers la bande-annonce. Les informations sur les films ont étaient constitués en partie avec le site suivant : [Kaggle](https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows) puis également grace à des recherches afin de compléter certaines informations.

# Normalisation des données

Ensuite, nous normalisons les données pour nous assurer que les tags d'émotions sont tous en minuscules grâce à la ligne :

```{r, echo=FALSE, warning=FALSE, message=FALSE}
movies$emotion_tags <- trimws(tolower(movies$emotion_tags))

```

# Interface utilisateur

L’interface utilisateur, ou **UI**, est construite avec **fluidPage**. Elle est divisée en deux sections :

1.  **Le panneau latéral (sidebarPanel)** :

    -   L’utilisateur peut sélectionner une émotion via une liste déroulante.

    -   Il peut indiquer le temps dont il dispose : "Moins de 2h" ou "Plus de 2h".

    -   Un champ permet de spécifier combien de recommandations il souhaite obtenir.

    -   Enfin, un bouton déclenche l’affichage des recommandations.

        **Exemple de composant de l’interface :**

```{r}
selectInput("emotion", "🎭 Choisissez une émotion :", choices = unique(movies$emotion_tags))
```

2.  **Le panneau principal (mainPanel)** :

    -   Il affiche les recommandations dans une table.

    -   Une zone pour valider ou rejeter les recommandations apparaît dynamiquement.

    -   Après validation, des cases à cocher permettent de choisir un ou plusieurs films.

    -   Enfin, un bouton ouvre les bande-annonces des films sélectionnés.

```{r interface, message=FALSE, warning=FALSE, include=FALSE}
ui <- fluidPage(
  titlePanel("🎬 Système de Recommandation de Films"),
  sidebarLayout(
    sidebarPanel(
      h4("🌈 Sélectionnez vos préférences :"),
      selectInput("emotion", "🎭 Choisissez une émotion :", 
                  choices = unique(movies$emotion_tags)),
      radioButtons("duration", "⏳ Combien de temps avez-vous ?", 
                   choices = c("Moins de 2h" = "short", "Plus de 2h" = "long")),
      numericInput("num_recommendations", "🔢 Nombre de recommandations :", value = 3, min = 1),
      actionButton("recommend", "Obtenir des recommandations 🎬"),
      uiOutput("feedback_ui")  # Section pour valider ou rejeter les recommandations
    ),
    mainPanel(
      h3("📋 Recommandations :"),
      tableOutput("recommendations"),  # Affiche les recommandations
      uiOutput("choose_movie_ui"),  # Cases à cocher pour choisir un film
      actionButton("open_trailer", "Ouvrir la bande-annonce 🎥"),  # Bouton pour ouvrir la bande-annonce
      textOutput("final_choice")  # Confirmation du choix
    )
  )
)
```

# Serveur

```{r serveur, message=FALSE, warning=FALSE, include=FALSE}
server <- function(input, output, session) {
  # Stocker les recommandations
  recommendations <- reactiveVal(data.frame())  # Pour stocker les recommandations actuelles
  
  # Générer les recommandations quand le bouton est cliqué
  observeEvent(input$recommend, {
    filtered <- movies[movies$emotion_tags == input$emotion, ]
    
    if (input$duration == "short") {
      filtered <- filtered[filtered$duration <= 120, ]
    } else if (input$duration == "long") {
      filtered <- filtered[filtered$duration > 120, ]
    }
    
    # Sélectionner un nombre donné de films
    if (nrow(filtered) > 0) {
      filtered <- filtered[sample(nrow(filtered), min(input$num_recommendations, nrow(filtered))), ]
    }
    
    # Mettre à jour les recommandations
    recommendations(filtered)
  })
  
  # Afficher les recommandations
  output$recommendations <- renderTable({
    req(recommendations())
    recs <- recommendations()
    if (nrow(recs) > 0) {
      recs[, c("title", "genre", "duration", "BA")]  # Colonnes affichées
    } else {
      NULL
    }
  }, rownames = TRUE)
  
  # Demander si les recommandations conviennent
  output$feedback_ui <- renderUI({
    if (nrow(recommendations()) > 0) {
      tagList(
        h4("🔁 Ces recommandations vous conviennent-elles ?"),
        actionButton("yes", "Oui, elles sont parfaites"),
        actionButton("no", "Non, je veux en voir d'autres")
      )
    }
  })
  
  # Réinitialiser les recommandations si "Non" est sélectionné
  observeEvent(input$no, {
    showNotification("🔄 Nouvelles recommandations en cours...", type = "message")
    recommendations(data.frame())  # Réinitialiser les recommandations
  })
  
  # Afficher les cases à cocher pour choisir un film uniquement après validation
  output$choose_movie_ui <- renderUI({
    req(input$yes)  # Attendre la validation des recommandations
    recs <- recommendations()
    if (nrow(recs) > 0) {
      checkboxGroupInput("chosen_movies", "🎥 Cochez le ou les films qui vous intéressent :", 
                         choices = recs$title)
    }
  })
  
  # Ouvrir les bande-annonces des films cochés
  observeEvent(input$open_trailer, {
    req(input$chosen_movies)  # S'assurer qu'au moins un film a été coché
    recs <- recommendations()
    chosen <- recs[recs$title %in% input$chosen_movies, ]  # Trouver les films cochés
    if (nrow(chosen) > 0) {
      lapply(chosen$BA, browseURL)  # Ouvrir les bande-annonces dans le navigateur
      output$final_choice <- renderText({
        paste("Vous avez choisi de regarder :", paste(chosen$title, collapse = ", "), "- Bon visionnage ! 🎬")
      })
    }
  })
}

```

La partie serveur est le cœur du programme. Elle gère toutes les interactions de l'utilisateur. Nous allons vous expliquer les fonctionnalités principales. 
1. **Génération des recommandations** Lorsque l’utilisateur clique sur le bouton "Obtenir des recommandations", le programme applique des filtres: 
- *Filtrage par émotion* : Il sélectionne les films qui correspondent à l’émotion choisie

- *Filtrage par durée* : Ensuite, il restreint les résultats selon la durée sélectionnée

- *Nombre de recommandations* : Enfin, il limite le nombre de films affichés en sélectionnant un échantillon aléatoire
Les recommandations sont ensuite stockées dans une variable réactive. 

2.  **Affichage des recommandations** Les recommandations sont affichées sous forme de table dynamique dans l'interface principale.
Cette table montre les titres des films, leur genre, leur durée, et un lien vers la bande-annonce. 

3. **Validation ou rejet des recommandations** Une fois que les recommandations sont affichées, l’utilisateur doit indiquer s’il est satisfait ou non grâce à deux boutons ("Oui" ou "Non"). Si l’utilisateur clique sur "Non", les recommandations sont réinitialisées, et le processus recommence.

4.  **Sélection des films préférés** Si les recommandations conviennent, une liste de cases à cocher apparaît pour permettre à l’utilisateur de sélectionner un ou plusieurs films parmi les options proposées:


5.  **Ouverture des bande-annonces** Lorsque l’utilisateur clique sur "Ouvrir la bande-annonce 🎥", les liens vers les bande-annonces des films sélectionnés sont ouverts dans le navigateur:


# Démonstration de l'application

Nous allons désormais vous montrer notre projet de développement d’un système interactif de recommandation de films. 
```{r application}
shinyApp(ui, server)

```
