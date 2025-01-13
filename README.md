---
title: "Système interactif de recommandation de films"
author: "Carpentier Lola, De Oliveira Corentin, Reynaud Valentin"
date: "2025-01-10"
output:
  html_document: default
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, error = FALSE, collapse = TRUE)
```


# Introduction

Nous vous présentons notre projet de développement d’un système interactif de recommandation de films basé sur les préférences de l’utilisateur. Ce projet utilise le langage **R** et le package **Shiny**, une bibliothèque qui permet de créer des applications web interactives. L’objectif principal était de concevoir une interface conviviale où l’utilisateur peut :

-   Sélectionner une émotion qu’il souhaite ressentir.
-   Indiquer de combien de temps il dispose pour regarder un film.
-   Recevoir des recommandations personnalisées.
-   Choisir son film préféré et visionner sa bande-annonce directement dans le navigateur.

Passons maintenant au détail de la construction du programme, que nous allons expliquer étape par étape.

# Installer les packages nécessaires

Dans la première partie du code, nous installons et chargeons les packages nécessaires: **Shiny** : pour créer l'interface utilisateur et le serveur interactif. **Readr** : pour lire les données contenues dans le fichier pelletier_emotions.csv.

```{r packages, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='hide'}

install.packages("shiny")
install.packages("readr")
library(shiny)
library(readr)

```

# Charger les données

Voici la ligne de code importante:

```{r donnees, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
movies <- read_csv("pelletier_emotions.csv")

```

Ce fichier contient les informations sur les films, comme le titre, le genre, la durée, l’émotion associée, et le lien vers la bande-annonce. Les informations sur les films ont étaient constitués en partie avec le site suivant : [Kaggle](https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows) puis également grace à des recherches afin de compléter certaines informations.

# Normalisation des données

Ensuite, nous normalisons les données pour nous assurer que les tags d'émotions sont tous en minuscules grâce à la ligne :

```{r normalisation,eval=FALSE, message=FALSE, warning=FALSE}
movies$emotion_tags <- trimws(tolower(movies$emotion_tags))

```

 # Ajout du script pour les confettis
 ```{r panneau latéral, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js")
  ),
  ```
  # Script pour lancer les confettis à l'ouverture
```{r panneau latéral, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
  tags$script(HTML("
    window.onload = function() {
      confetti({
        particleCount: 100,
        spread: 70,
        origin: { y: 0.6 }
      });
    };
  ")),
```

# Interface utilisateur
L’interface utilisateur, ou **UI**, est construite avec **fluidPage**. Elle est divisée en deux sections:

1.  **Le panneau latéral (sidebarPanel)**:

    -   L’utilisateur peut sélectionner une émotion via une liste déroulante.

    -   Il peut indiquer le temps dont il dispose : "Moins de 2h" ou "Plus de 2h".

    -   Un champ permet de spécifier combien de recommandations il souhaite obtenir.

    -   Enfin, un bouton déclenche l’affichage des recommandations.

      
```{r panneau latéral, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
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
```

2.  **Le panneau principal (mainPanel)**:

    -   Il affiche les recommandations dans une table.

    -   Une zone pour valider ou rejeter les recommandations apparaît dynamiquement.

    -   Après validation, des cases à cocher permettent de choisir un ou plusieurs films.

    -   Enfin, un bouton ouvre les bande-annonces des films sélectionnés.
```{r panneau principal,eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
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

La partie serveur est le cœur du programme.
```{r serveur, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
server <- function(input, output, session) {
```

Elle gère toutes les interactions de l'utilisateur. Nous allons vous expliquer les fonctionnalités principales. 
1. **Génération des recommandations** Lorsque l’utilisateur clique sur le bouton "Obtenir des recommandations", le programme applique des filtres: 

- *Filtrage par émotion* : Il sélectionne les films qui correspondent à l’émotion choisie: 
```{r filtrage émotions, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
   observeEvent(input$recommend, {
    filtered <- movies[movies$emotion_tags == input$emotion, ]
```


-   *Filtrage par durée* : Ensuite, il restreint les résultats selon la durée sélectionnée :
```{r filtrage durée, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
 if (input$duration == "short") {
      filtered <- filtered[filtered$duration <= 120, ]
    } else if (input$duration == "long") {
      filtered <- filtered[filtered$duration > 120, ]
    }

```


-   *Nombre de recommandations* : Enfin, il limite le nombre de films affichés en sélectionnant un échantillon aléatoire Les recommandations sont ensuite stockées dans une variable réactive. 

```{r filtrage nombre, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
 if (nrow(filtered) > 0) {
      filtered <- filtered[sample(nrow(filtered), min(input$num_recommendations, nrow(filtered))), ]
    }
    
```

Les recommandations sont ensuite stockées dans une variable réactive: 
```{r stockage, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
recommendations <- reactiveVal(filtered)

```


2.  **Affichage des recommandations** Les recommandations sont affichées sous forme de table dynamique dans l'interface principale. Cette table montre les titres des films, leur genre, leur durée, et un lien vers la bande-annonce.
```{r affichage, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}

 output$recommendations <- renderTable({
    req(recommendations())
    recs <- recommendations()
    if (nrow(recs) > 0) {
      recs[, c("title", "genre", "duration", "BA")]  # Colonnes affichées
    } else {
      NULL
    }
  }, rownames = TRUE)

```

3.  **Validation ou rejet des recommandations** Une fois que les recommandations sont affichées, l’utilisateur doit indiquer s’il est satisfait ou non grâce à deux boutons ("Oui" ou "Non").
```{r validation, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE }
 output$feedback_ui <- renderUI({
    if (nrow(recommendations()) > 0) {
      tagList(
        h4("🔁 Ces recommandations vous conviennent-elles ?"),
        actionButton("yes", "Oui, elles sont parfaites"),
        actionButton("no", "Non, je veux en voir d'autres")
      )
    }
  })
```

Si l’utilisateur clique sur "Non", les recommandations sont réinitialisées, et le processus recommence.
```{r réinitialisation,  eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
observeEvent(input$no, {
    showNotification("🔄 Nouvelles recommandations en cours...", type = "message")
    recommendations(data.frame())  # Réinitialiser les recommandations
  })
```

4.  **Sélection des films préférés** Si les recommandations conviennent, une liste de cases à cocher apparaît pour permettre à l’utilisateur de sélectionner un ou plusieurs films parmi les options proposées:
```{r préférences, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
  output$choose_movie_ui <- renderUI({
    req(input$yes)  # Attendre la validation des recommandations
    recs <- recommendations()
    if (nrow(recs) > 0) {
      checkboxGroupInput("chosen_movies", "🎥 Cochez le ou les films qui vous intéressent :", 
                         choices = recs$title)
    }
  })
  

```
5.  **Ouverture des bande-annonces** Lorsque l’utilisateur clique sur "Ouvrir la bande-annonce 🎥", les liens vers les bande-annonces des films sélectionnés sont ouverts dans le navigateur:
```{r bande annonces, eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE}
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

# Démonstration de l'application

Nous allons désormais vous montrer notre projet de développement d’un système interactif de recommandation de films. 
```{r application}
shinyApp(ui, server)

```
