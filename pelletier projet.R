# Installer les packages nécessaires
install.packages("shiny")
install.packages("readr")
library(shiny)
library(readr)

# Charger les données
movies <- read_csv("~/Documents/pelletier_emotions.csv")

# Normalisation des données
movies$emotion_tags <- trimws(tolower(movies$emotion_tags))

# Interface utilisateur
ui <- fluidPage(
  # Ajout de CSS personnalisé pour l'arrière-plan
  tags$head(
    tags$style(HTML("
      body {
        background-color: #fdf6e3; /* Couleur de fond */
        font-family: 'Arial', sans-serif;
      }
    "))
  ),
  
  # Ajout du script pour les confettis
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js")
  ),
  
  # Script pour lancer les confettis à l'ouverture
  tags$script(HTML("
    window.onload = function() {
      confetti({
        particleCount: 100,
        spread: 70,
        origin: { y: 0.6 }
      });
    };
  ")),
  
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
      uiOutput("feedback_ui") 
    ),
    mainPanel(
      h3("📋 Recommandations :"),
      tableOutput("recommendations"),  
      uiOutput("choose_movie_ui"),  
      actionButton("open_trailer", "Ouvrir la bande-annonce 🎥"),  
      textOutput("final_choice")  
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Stocker les recommandations
  recommendations <- reactiveVal(data.frame()) 
  
  # Générer les recommandations quand le bouton est cliqué
  observeEvent(input$recommend, {
    filtered <- movies[movies$emotion_tags == input$emotion, ]
    
    if (input$duration == "short") {
      filtered <- filtered[filtered$duration <= 120, ]
    } else if (input$duration == "long") {
      filtered <- filtered[filtered$duration > 120, ]
    }
    
    # Sélectionner un nombre de films
    if (nrow(filtered) > 0) {
      filtered <- filtered[sample(nrow(filtered), min(input$num_recommendations, nrow(filtered))), ]
    }
    
    # Mettre à jour 
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
    recommendations(data.frame()) 
  })
  
  # Afficher les cases à cocher pour choisir un film après validation
  output$choose_movie_ui <- renderUI({
    req(input$yes) 
    recs <- recommendations()
    if (nrow(recs) > 0) {
      checkboxGroupInput("chosen_movies", "🎥 Cochez le ou les films qui vous intéressent :", 
                         choices = recs$title)
    }
  })
  
  # Ouvrir les bande-annonces des films cochés
  observeEvent(input$open_trailer, {
    req(input$chosen_movies)  
    recs <- recommendations()
    chosen <- recs[recs$title %in% input$chosen_movies, ]  
    if (nrow(chosen) > 0) {
      lapply(chosen$BA, browseURL)  
      output$final_choice <- renderText({
        paste("Vous avez choisi de regarder :", paste(chosen$title, collapse = ", "), "- Bon visionnage ! 🎬")
      })
    }
  })
}

# Lancer l'application
shinyApp(ui, server)



