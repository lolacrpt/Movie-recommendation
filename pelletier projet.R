# Installer les packages nécessaires
install.packages("readxl")
install.packages("readr")
install.packages("crayon")
library(readxl)
library(readr)
library(crayon)

# Charger les données
movies <- read_csv("Documents/pelletier_emotions.csv")

# On normalise les données 
movies$emotion_tags <- trimws(tolower(movies$emotion_tags))

# Fonction
recommender_system <- function() {
  # Liste des émotions disponibles pour l'utilisateur 
  emotions <- unique(movies$emotion_tags)
  
  # Bienvenue dans le système de recommandation 
  cat("\n========================================\n")
  cat(bgBlue$white$bold(" 🎬 Bienvenue dans le système de recommandation de films ! "), "\n")
  cat("========================================\n")
  cat("🌈 Les émotions disponibles sont :", green(paste(emotions, collapse = ", ")), "\n")
  
  repeat {
    # Afficher un menu pour sélectionner l'émotion
    cat("\n🎭 Sélectionnez une émotion parmi les options suivantes :\n")
    choice <- menu(emotions, title = yellow("👉 Entrez votre choix"))
    
    # Si aucun choix n'est fait
    if (choice == 0) {
      cat(red("\n❌ Aucun choix sélectionné. Merci de relancer le programme.\n"))
      break
    }
    
    # Récupérer l'émotion choisie
    user_emotion <- emotions[choice]
    cat(green(paste("\n✨ Vous avez choisi l'émotion :", user_emotion)), "\n")
    
    # Filtrer les films en fonction de l'émotion
    filtered_movies <- movies[movies$emotion_tags == user_emotion, ]
    
    # Vérifier si des films sont disponibles
    if (nrow(filtered_movies) == 0) {
      cat(red("\nDésolé, aucun film ne correspond à votre recherche. Essayez une autre émotion.\n"))
      next
    }
    
    # Demander le temps disponible
    cat("\n⏳ Combien de temps avez-vous pour regarder un film ?\n")
    time_choice <- menu(c("Moins de 2h", "Plus de 2h"), title = "👉 Sélectionnez votre choix")
    
    # Filtrer selon la durée choisie par l'utilisateur 
    if (time_choice == 1) {
      filtered_movies <- filtered_movies[filtered_movies$duration <= 120, ]
      cat("\n🎥 Vous avez choisi des films de moins de 2h.\n")
    } else if (time_choice == 2) {
      filtered_movies <- filtered_movies[filtered_movies$duration > 120, ]
      cat("\n🎥 Vous avez choisi des films de plus de 2h.\n")
    } else {
      cat(red("\n❌ Aucun choix sélectionné. Merci de relancer le programme.\n"))
      break
    }
    
    # Vérifier si des films correspondent à la durée souhaitée
    if (nrow(filtered_movies) == 0) {
      cat(red("\nDésolé, aucun film ne correspond à votre durée souhaitée. Essayez un autre choix.\n"))
      next
    }
    
    # Demander combien de recommandations l'utilisateur souhaite obtenir 
    num_recommendations <- as.integer(readline(prompt = blue("🔢 Combien de recommandations souhaitez-vous ? (ex: 1, 3, 5) : ")))
    if (is.na(num_recommendations) || num_recommendations <= 0) {
      cat(red("⚠️ Entrée invalide. Une recommandation par défaut sera donnée.\n"))
      num_recommendations <- 1
    }
    
    # Sélectionner les films recommandés
    recommended_movies <- filtered_movies[sample(nrow(filtered_movies), min(num_recommendations, nrow(filtered_movies))), ]
    
    # Afficher les recommandations
    cat("\n🎬 Voici vos recommandations de films :\n")
    for (i in 1:nrow(recommended_movies)) {
      cat(yellow(paste("\n--- Film", i, "---\n")))
      cat(blue(paste("Titre :", recommended_movies$title[i], "\n")))
      cat(green(paste("Genre :", recommended_movies$genre[i], "\n")))
      cat(cyan(paste("Émotion :", recommended_movies$emotion_tags[i], "\n")))
      cat(paste("Durée :", recommended_movies$duration[i], "minutes\n"))
      cat(magenta(paste("Bande-annonce :", recommended_movies$BA[i], "\n")))  # Ajout de la bande-annonce
    }
    
    # Demander si l'utilisateur souhaite une autre recommandation
    replay <- tolower(trimws(readline(prompt = yellow("\n🔁 Souhaitez-vous une autre recommandation ? (oui/non) : "))))
    if (replay != "oui") {
      cat(bgGreen$white("\nMerci d'avoir utilisé le système de recommandation. Bon visionnage ! 🎥\n"))
      break
    }
  }
}

# Exécuter la fonction de recommandation
recommender_system()


