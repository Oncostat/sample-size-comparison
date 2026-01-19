library(targets)
library(tarchetypes)

# Chargement des fonctions
source("R/rpact_wrapper.R")

# Options pour les différentes étapes
options(tidyverse.quiet = TRUE)
tar_option_set(packages = "tidyverse")

tar_plan(
    # Chargement du fichier CSV
    tar_file(csv_file, "data/nat2020.csv"),
    donnees_brutes = read_csv2(csv_file),

    # Mise en forme des données
    donnees = donnees_brutes %>%
        filter_data() %>%
        pivot_2019_2020()

    # Calcul indicateurs
    donnees_evo = donnees %>%
        calcule_evo(min_n = 1000)
)