################     EXERCICE 1 ###########################

#creation des variables
prix <- 100
quantite <- 5
#calcul du total
total <- prix * quantite
total
#application du remise de 10%
remise <- total * 0.1
remise

#calcul du taxe (15% du total apres remise)
total_apres_remise <- total - remise
taxe <- total_apres_remise * 0.15
taxe

#calcul du prix final
prix_final <- total_apres_remise + taxe
print(prix_final)
# verifier le type de chaque variable
class(prix)
class(quantite)
class(prix_final)
# en divisant par zero
prix/0
#resultat infini.


################     EXERCICE 2 ###########################

#creation d'un vecteur temperature en celsius avec ces valeurs: 22, 24, 19, 25, 23,26, 21
temperature <- c(22, 24, 19, 25, 23, 26, 21)
# calcul de la temperature moyenne
temperature_moyenne <- mean(temperature)
temperature_moyenne
#retrouver la temperature maximale et minimale
temperature_max <- max(temperature)
temperature_min <- min(temperature)
temperature_max
temperature_min
#creation d'un nouveau vecteur avec les temperatures supérieures à 23 degres
temperature_23_sup <- temperature[temperature > 23]
temperature_23_sup
#conversion des temperatures en fahrenheit
temperature_fahrenheit <- (temperature * 9/5) + 32
temperature_fahrenheit
#nommer les elements du vecteur(Lundi, Mardi etc.)
names(temperature_fahrenheit) <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
temperature_fahrenheit

#################     EXERCICE 3 ###########################

#Creation dataframe
etudiants <- data.frame(
    Nom = c("Alice", "Bob", "Charlie", "Diana"),
    note_Maths = c(85, 90, 78, 95),
    note_Info = c(88, 85, 92, 90),
    present=c(TRUE, TRUE, FALSE, TRUE)
)
etudiants
#AJout d'une colonne moyenne
etudiants$moyenne <- rowMeans(etudiants[, c("note_Maths", "note_Info")])
etudiants

#on trouve l'etudiant avec la meilleure moyenne
meilleur_Etudiant <- etudiants[which.max(etudiants$moyenne),]
meilleur_Etudiant
#filtrage des etudiants presents
etudiants_presents<- etudiants[etudiants$present == TRUE, ]
etudiants_presents

#calcul de la moyenne de la classe en maths
moyenne_classe_maths <- mean(etudiants$note_Maths)
moyenne_classe_maths

#ajout de la colonne mention(>= 90: "Excellent", >= 80: "Bien", sinon:"Passable")
etudiants$mention <- ifelse(
    etudiants$moyenne >= 90, "Excellent",
    ifelse(
        etudiants$moyenne > 85, "Bien", "Passable"
        )
        )
        
etudiants
