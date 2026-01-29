

library(tidyverse)
############### Exercice 1  ####################

# Convertir du code R base en code avec pipe
ventes <- data.frame(
  produit = c("A", "B", "C", "A", "B", "C", "A", "B"),
  prix = c(10, 15, 20, 10, 15, 20, 10, 15),
  quantite = c(5, 3, 2, 8, 4, 1, 6, 7)
)
#Réécrire avec le pipe %>% 
#1 head(ventes, 3)
ventes %>%
  head(3)
#2 nrow(ventes)
ventes %>%
  nrow()
#3 summary(ventes$prix)
ventes %>%
  pull(prix) %>%
  summary()
#4 unique(ventes$produit)
ventes %>%
  pull(produit) %>%
  unique()

  ##################### Exercice 2 : Sélection et Filtrage ####################

#Données : dataset iris (préchargé dans R)

#1 Sélectionnez uniquement les colonnes Species et Petal.Length
iris %>%
  select(Species, Petal.Length)

#2 Sélectionnez toutes les colonnes commençant par "Sepal"
iris %>%
  select(starts_with("Sepal"))
#3 Filtrez les fleurs de l’espèce "setosa"
iris %>%
  filter(Species == "setosa")

#4 Filtrez les fleurs avec Sepal.Length > 5 ET Sepal.Width > 3
iris %>%
  filter(Sepal.Length > 5 & Sepal.Width > 3)

#5 Filtrez les espèces "setosa" OU "virginica"
iris %>%
  filter(Species == "setosa" | Species == "virginica")

#   iris %>%
#   filter(Species %in% c("setosa", "virginica"))


#6 Créez un pipeline combinant sélection et filtrage :
        #Colonnes : Species et celles contenant "Petal"
        #Lignes : Petal.Length > 4
iris %>%
  select(Species, contains("Petal")) %>%
  filter(Petal.Length > 4)

###################### Exercice 3 : Mutate ####################

employes <- data.frame (
    nom = c(" Alice ", " Bob", " Charlie ", " Diana ", " Eve"),
    salaire_mensuel = c(3000, 3500, 2800, 4200, 3100),
    anciennete = c(2, 5, 1, 8, 3),
    departement = c("IT", "RH", "IT", " Finance ", "RH")
)
employes

#1 Créez salaire_annuel (× 12)
employes_complet <- employes %>%
  mutate(salaire_annuel = salaire_mensuel * 12)

#2 Créez bonus (5% du salaire annuel si ancienneté > 3, sinon 0)
employes_complet <- employes_complet %>%
  mutate(
    salaire_annuel = salaire_mensuel * 12,
    bonus = ifelse(anciennete > 3, salaire_annuel * 0.05, 0)
  )
#3 Créez salaire_total (salaire_annuel + bonus)
employes_complet <- employes_complet %>%
  mutate(
    salaire_annuel = salaire_mensuel * 12,
    bonus = ifelse(anciennete > 3, salaire_annuel * 0.05, 0),
    salaire_total = salaire_annuel + bonus
  )
#4 Créez niveau : "Junior" (< 3 ans), "Senior" (3-6 ans), "Expert" (> 6 ans)
employes_complet <- employes_complet %>%
  mutate(
    niveau = case_when(
      anciennete < 3 ~ "Junior",
      anciennete >= 3 & anciennete <= 6 ~ "Senior",
      anciennete > 6 ~ "Expert"
    )
)

#5 Créez nom_complet : nom en majuscules
employes_complet <- employes_complet %>%
  mutate(
    nom_complet = toupper(trimws(nom))
)

employes_complet
# tout en UN#
employes_complet <- employes %>%
  mutate(
    salaire_annuel = salaire_mensuel * 12,
    bonus = ifelse(anciennete > 3, salaire_annuel * 0.05, 0),
    salaire_total = salaire_annuel + bonus,
    niveau = case_when(
      anciennete < 3 ~ "Junior",
      anciennete >= 3 & anciennete <= 6 ~ "Senior",
      anciennete > 6 ~ "Expert"
    ),
    nom_complet = toupper(trimws(nom))
  )
employes_complet

##################### Exercice 4 : Trier et resumer des donnees ####################


#Données : Utilisez employes de l’exercice précédent
#Tâches avec arrange() :
    #1 Triez par salaire croissant
    employes %>%
        arrange(salaire_mensuel)
    #2 Triez par salaire décroissant
    employes %>%
        arrange(desc(salaire_mensuel))
    #3 Triez par département puis salaire décroissant
    employes %>%
        arrange(departement, desc(salaire_mensuel))
    #4 Affichez les 3 salaires les plus élevés
    employes %>%
        arrange(desc(salaire_mensuel)) %>%
        head(3)
#Tâches avec summarise() :
    #1 Calculez : salaire moyen, médian, min, max
    employes %>%
    summarise(
        salaire_moyen = mean(salaire_mensuel),
        salaire_median = median(salaire_mensuel),
        salaire_min = min(salaire_mensuel),
        salaire_max = max(salaire_mensuel)
  )
    #2 Comptez le nombre d’employés
    employes %>%
        summarise(nombre_employes = n())
    #3 Calculez l’ancienneté moyenne
    employes %>%
        summarise(anciennete_moyenne = mean(anciennete))

################# EXERCICE 5  Analyse Groupée ###########################


    ventes <- data.frame (
    date = as.Date(c("2024-01-15", "2024-01-15", "2024-01-16",
                     "2024-01-16", "2024-01-17", "2024-01-17")),
    produit = c("A","B","A","C","B","A"),
    region = c("Nord", "Sud", "Nord","Est","Sud","Nord"),
    quantite = c(10, 5, 15, 8, 12, 20) ,
    prix_unitaire = c(100 , 150, 100, 200, 150, 100)
    )

#1 Calculez le chiffre d’affaires par produit
ventes %>%
  mutate(ca = quantite * prix_unitaire) %>%
  group_by(produit) %>%
  summarise(chiffre_affaires = sum(ca))
  
  ventes

#2 Trouvez les ventes moyennes par région
ventes %>%
  group_by(region) %>%
  summarise(quantite_moyenne = mean(quantite))
#3 Comptez le nombre de transactions par date
ventes %>%
  group_by(date) %>%
  summarise(nb_transactions = n())
#4 Pour chaque région : CA total, quantité totale, nb transactions
ventes %>%
  mutate(ca = quantite * prix_unitaire) %>%
  group_by(region) %>%
  summarise(
    ca_total = sum(ca),
    quantite_totale = sum(quantite),
    nb_transactions = n()
  )
#5 Trouvez le produit le plus vendu (en quantité) par région
ventes %>%
  group_by(region, produit) %>%
  summarise(quantite_totale = sum(quantite), .groups = "drop") %>%
  group_by(region) %>%
  slice_max(quantite_totale, n = 1)


  ################# EXERCICE 6 : JListe des fichiers ###########################


 # Creer un fichier CSV pour l’ exercice
    write_csv(employes, "employes.csv")

#1 Lisez le fichier employes.csv
employes_import <- read_csv("employes.csv")

#2 Affichez les 5 premières lignes
employes_import %>%
  head(5)
#3 Vérifiez les types de colonnes avec spec()
spec(employes_import)
#4 Relisez en spécifiant que departement est un facteur
employes_import <- read_csv(
  "employes.csv",
  col_types = cols(
    departement = col_factor()
  )
)
spec(employes_import)
#5 Comptez les employés par département
employes_import %>%
  count(departement,sort = TRUE)


################    EXERCICE 7 : Joindre des Tables ###########################
#Maîtriser les différents types de jointures
# Vous avez 3 tables :
# produits : id, nom, categorie, prix
# clients : id, nom, region, statut
# ventes : id, date, produit_id, client_id, quantite

produits <- data.frame(
  produit_id = 1:10,
  nom_produit = paste("Produit", LETTERS[1:10]),
  categorie = rep(c("Electronique", "Vetements"), each = 5),
  prix = c(500, 750, 300, 1000, 450, 50, 80, 120, 60, 90)
)

# Clients
clients <- data.frame(
  client_id = 1:8,
  nom_client = paste("Client", 1:8),
  region = rep(c("Nord", "Sud", "Est", "Ouest"), each = 2),
  statut = rep(c("Premium", "Standard"), 4)
)

# Ventes
set.seed(42)
ventes <- data.frame(
  vente_id = 1:20,
  date = sample(
    seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
    20
  ),
  produit_id = sample(1:10, 20, replace = TRUE),
  client_id = sample(1:8, 20, replace = TRUE),
  quantite = sample(1:5, 20, replace = TRUE)
)

#1 Joindre les 3 tables
data_complete <- ventes %>%
  left_join(produits, by = "produit_id") %>%
  left_join(clients, by = "client_id") %>%
  mutate(ca = quantite * prix)

data_complete

#2 Calculer le CA par produit et par région
ca_produit_region <- data_complete %>%
  group_by(nom_produit, region) %>%
  summarise(
    ca_total = sum(ca),
    .groups = "drop"
  )
ca_produit_region

# 3 Identifier les 5 meilleurs clients (CA total)
top_clients <- data_complete %>%
  group_by(nom_client) %>%
  summarise(
    ca_total = sum(ca),
    .groups = "drop"
  ) %>%
  arrange(desc(ca_total)) %>%
  head(5)
top_clients

# 4 Analyser les ventes par catégorie et statut client
analyse_ventes <- data_complete %>%
  group_by(categorie, statut) %>%
  summarise(
    ca_total = sum(ca),
    quantite_totale = sum(quantite),
    nb_transactions = n(),
    .groups = "drop"
  )
  analyse_ventes
# 5 Créer un tableau résumé avec : CA total, nb transactions, panier
tableau_resume <- data_complete %>%
  group_by(nom_produit, categorie) %>%
  summarise(
    ca_total = sum(ca),
    quantite_totale = sum(quantite),   
    nb_clients = n_distinct(client_id),
    nb_produits_uniques = n_distinct(produit_id),
    .groups = "drop"
  ) %>%
  arrange(mois, desc(ca_total))

  tableau_resume
