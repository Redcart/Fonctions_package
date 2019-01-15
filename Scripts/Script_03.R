###################################################################################################################################################
#######                                                                                                                                      ######
#######                                             Fonctions utiles (création de package ?)                                                 ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Idées de fonctions

# - Procédure pour combler les gaps des séries temporelles au sein d'un panel (avec répétition pour les débuts et fins)

rm(list = ls())

### Chargement de packages

library(ggplot2)
rm(list = ls())

#install.packages("ineq")
#install.packages("sqldf")

### Chargement des librairies
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(sqldf)


##############################
### Chargement des données ###
##############################

donnees_ca <- fread("Output/donnees_ca_corrigees.csv")
# ==> Données hebdomadaires par gamme 

budget <- read.csv2("Data/budget_fdj_ALL_YEARS.csv")
budget$SommeBudget <- as.numeric(as.character(budget$SommeBudget))


perimetre_1 <- fread("Output/liste_pdv_gamme_constante.csv")
str(perimetre_1)

perimetre_2 <- fread("Output/liste_pdv_gamme_significative.csv")
str(perimetre_2)

# on exclut les PDV de P2 qui sont déja présents dans P1
perimetre_2 <- perimetre_2 %>% 
  anti_join(perimetre_1, by = c("Code PDV" = "Code PDV"))

perimetre_2 %>% select(`Code PDV`) %>% distinct() %>% count


##########################################################
### Corrections des gap au sein des séries temporelles ###
##########################################################

### Méthode interpolation linéaire


###################
### Perimètre 1 ###
###################


# Créer un calendrier complet

create_calendar(data ){
  
  
}
annees <- 2014:2017
semaines <- str_pad(1:52, 2, pad = "0")

calendrier <- annees %>% CJ(semaines) 
colnames(calendrier) = c("annee", "semaine")
calendrier$id_semaine <- paste0(calendrier$annee, "W", calendrier$semaine, sep = "")
calendrier <- data.frame(calendrier)
str(calendrier)


calendrier %>% select(annee) %>% distinct() %>% count
calendrier %>% select(id_semaine) %>% distinct() %>% count

# 2ème étape: faire un cross join entre les pdv et le calendrier 2014-2017

calendrier_pdv_gamme_cst <- sqldf("SELECT * 
                                  FROM perimetre_1
                                  CROSS JOIN calendrier")


calendrier_pdv_gamme_cst %>% select(`Code PDV`) %>% distinct() %>% count
calendrier_pdv_gamme_cst %>% select(annee) %>% distinct() %>% count
calendrier_pdv_gamme_cst %>% select(id_semaine) %>% distinct() %>% count


# On remplace les débuts et fin de série manquantes par répétition des premières et dernières valeurs renseignées

end_start_to_fill <- function(data, calendar, gap_variable, key_variable, time_variable, digits = 2){
  
  start_end <- data %>% 
    arrange(get(key_variable), get(time_variable)) %>% 
    select(key_variable, gap_variable) %>% 
    group_by(get(key_variable)) %>% 
    mutate(first_gap_variable = first(get(gap_variable))) %>% 
    mutate(last_gap_variable = last(get(gap_variable))) %>% 
    ungroup() %>% 
    select(-gap_variable) %>% 
    distinct()
  
  
  data_1 <- calendar %>% 
    left_join(data, by = c(key_variable, time_variable)) %>% 
    left_join(start_end, by = key_variable) %>% 
    arrange(get(key_variable), get(time_variable)) %>% 
    ungroup() %>% 
    mutate(boo_gap = ifelse(is.na(get(gap_variable)), 1, 0)) %>% # début de période
    group_by(get(key_variable)) %>% 
    mutate(lag_boo_gap = lag(boo_gap)) %>% 
    mutate(first_gap = ifelse(boo_gap == 1 & (lag_boo_gap == 0 | is.na(lag_boo_gap)), 1, 0)) %>% 
    mutate(n_gap = cumsum(first_gap)) %>% 
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>% 
    mutate(boo_start_toreplace = ifelse(is.na(first(get(gap_variable))), 1, 0)) %>% 
    ungroup() %>% 
    mutate(values_to_replace = ifelse(boo_start_toreplace == 1 & n_gap == 1, 1, 0)) %>% 
    mutate(gap_variable_corrected = ifelse(values_to_replace == 1, first_gap_variable, get(gap_variable))) %>% 
    arrange(get(key_variable), desc(get(time_variable))) %>% # fin de période
    mutate(boo_gap = ifelse(is.na(gap_variable_corrected), 1, 0)) %>% 
    group_by(get(key_variable)) %>% 
    mutate(lag_boo_gap = lag(boo_gap)) %>% 
    mutate(first_gap = ifelse(boo_gap == 1 & (lag_boo_gap == 0 | is.na(lag_boo_gap)), 1, 0)) %>% 
    mutate(n_gap = cumsum(first_gap)) %>% 
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>% 
    mutate(boo_end_toreplace = ifelse(is.na(first(gap_variable_corrected)), 1, 0 )) %>% 
    ungroup() %>% 
    mutate(values_to_replace = ifelse(boo_end_toreplace == 1 & n_gap == 1, 1, 0)) %>% 
    mutate(gap_variable_corrected = ifelse(values_to_replace == 1, last_gap_variable, gap_variable_corrected)) %>% 
    arrange(get(key_variable), get(time_variable)) %>% 
    select(key_variable, time_variable, gap_variable, gap_variable_corrected)
  
  
  
  return(data_1)
  
}

# On identifie les gaps intermédiaires par interpolation linéaire

gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits = 2){
  
  
  data <- data %>% 
    arrange(get(key_variable), get(time_variable)) %>% 
    mutate(boo_gap = ifelse(is.na(get(gap_variable)), 1, 0)) %>% 
    group_by(get(key_variable)) %>% 
    mutate(lag_boo_gap = lag(boo_gap)) %>% 
    mutate(first_gap = ifelse(boo_gap == 1 & lag_boo_gap == 0, 1, 0)) %>% 
    mutate(n_gap = cumsum(first_gap)) %>% 
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>% 
    ungroup() %>% 
    group_by(get(key_variable), n_gap) %>% 
    mutate(n_gap_step = cumsum(boo_gap)) %>% 
    mutate(number_gap_step = max(n_gap_step)) %>%
    ungroup() %>% 
    group_by(get(key_variable)) %>% 
    mutate(gap_variable_before = lag(get(gap_variable))) %>% 
    mutate(gap_variable_after = lead(get(gap_variable))) %>% 
    ungroup() %>% 
    group_by(get(key_variable), n_gap) %>% 
    mutate(gap_variable_before = ifelse(is.na(gap_variable_before), 0, gap_variable_before)) %>% 
    mutate(gap_variable_after = ifelse(is.na(gap_variable_after), 0, gap_variable_after)) %>% 
    mutate(gap_variable_before = max(gap_variable_before)) %>% 
    mutate(gap_variable_after = max(gap_variable_after)) %>% 
    ungroup() %>% 
    mutate(gap_variable_corrected = ifelse(is.na(get(gap_variable)), gap_variable_before + ((gap_variable_after - gap_variable_before)*n_gap_step/(number_gap_step+1)), 
                                           get(gap_variable))) %>%
    mutate(gap_variable_corrected = round(gap_variable_corrected, digits)) %>% 
    ungroup() 
  
  return(data)
  
  
}

rep(c("Paris", "Madrid", "Berlin"), each = 10)

jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
                          "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
                          "year" = 2009:2018,
                          "value" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500, 
                                      0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
                                      100, 200, 400, 700, 700, 800, 600, 500, NA, NA))


jeu_donnees <- na.omit(jeu_donnees)

annees <- data.frame("year" = 2009:2018)
ids <- jeu_donnees %>%  select(country) %>% distinct()

calendrier <-  sqldf("SELECT * 
                     FROM ids
                     CROSS JOIN annees")

data_to_check_1 <- end_start_to_fill(data = jeu_donnees, calendar = calendrier, gap_variable = "value", key_variable = "country", time_variable = "year", digits = 2)


data_to_check_2 <- gap_to_fill(data = data_to_check_1, gap_variable = "gap_variable_corrected", key_variable = "country", time_variable = "year", digits = 1)
