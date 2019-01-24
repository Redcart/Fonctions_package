###################################################################################################################################################
#######                                                                                                                                      ######
#######                                                        Fonctions utiles                                                              ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Fonctions codées dans ce script

# - Procédure pour combler les gaps des séries temporelles au sein d'un panel

  # - Fonction permettant de créer un calendrier vide
  # - Fonction pour réaliser la répétition en début et fin de série temporelle
  # - Fonction pour combler les gaps intermédiaires d'une série temporelle

rm(list = ls())

### Chargement de packages

library(ggplot2)
library(dplyr)
library(sqldf)
library(stringr)

###################################################
###  Fonction pour créer un calendrier complet  ###
###################################################

# arguments:
  # - data: data frame
  # - key_variable: variable faisant référence à la clé de l'observation (ID, ...)
  # - time_variable: variable temporelle permettant d'ordonner les observations
  # - start_year: année de début de la série temporelle
  # - end_year: année de fin de la série temporelle

create_calendar_day <- function(data, key_variable, time_variable, start_year, end_year){
  
   #, scale_time
  
  ids <- data %>% 
    select(key_variable) %>% 
    distinct()
  
  years <- data.frame(start_year:end_year)

  calendar <- sqldf("SELECT * 
                     FROM ids
                     CROSS JOIN years")
  
  colnames(calendar) <- c(key_variable, time_variable)
  
  return(calendar)
  
}


#####################################################################
###  Fonction pour combler les débuts et fin de série manquantes  ###
#####################################################################

# arguments:
  # - data: data frame
  # - calendar: calendrier vide par ID
  # - gap_variable: variable dont il faut combler les gap
  # - key_variable: variable faisant référence à la clé de l'observation (ID, ...)
  # - time_variable: variable temporelle permettant d'ordonner les observations
  # - digits: nombre de chiffres après la virgule à garder lors de l'arrondi de la valeur interpolée

end_start_to_fill <- function(data, calendar, gap_variable, key_variable, time_variable, digits = 2){
  
  new_var <- paste(gap_variable, "_corrected_1", sep = "")
  
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
    select(key_variable, time_variable, gap_variable, gap_variable_corrected) %>% 
    rename(!!new_var:=gap_variable_corrected) 

  return(data_1)
  
}

####################################################################################
####    Fonction pour combler les gaps intermdédiaires d'une série temporelle    ###
####################################################################################

# arguments:
  # - data: data frame
  # - gap_variable: variable dont il faut combler les gap
  # - key_variable: variable faisant référence à la clé de l'observation (ID, ...)
  # - time_variable: variable temporelle permettant d'ordonner les observations
  # - digits: nombre de chiffres après la virgule à garderlors de l'arrondi de la valeur interpolée

gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits = 2){

  new_var <- paste(str_sub(gap_variable, 1, str_length(gap_variable)-1), "2", sep = "")
  
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
    mutate(gap_variable_corrected = ifelse(is.na(get(gap_variable)), 
                                           gap_variable_before + ((gap_variable_after - gap_variable_before)*n_gap_step/(number_gap_step+1)), 
                                           get(gap_variable))) %>%
    mutate(gap_variable_corrected = round(gap_variable_corrected, digits)) %>% 
    ungroup() %>% 
    select(-boo_gap, -lag_boo_gap, -first_gap, -n_gap, -n_gap_step, -number_gap_step, -gap_variable_before, -gap_variable_after, -`get(key_variable)`) %>% 
    rename(!!new_var:=gap_variable_corrected)
  
  return(data)
  
}

#################################
####   Tests des fonctions   ####
#################################

### création d'un jeu de données test

rep(c("Paris", "Madrid", "Berlin"), each = 10)

jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
                          "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
                          "year" = 2009:2018,
                          "gdp" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500, 
                                      0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
                                      100, 200, 400, 700, 700, 800, 600, 500, NA, NA))


jeu_donnees <- na.omit(jeu_donnees)

### vérification

data_to_check_1 <- create_calendar_day(data = jeu_donnees, key_variable = "country", time_variable = "year", start_year = 2009, end_year = 2018)
  
data_to_check_2 <- end_start_to_fill(data = jeu_donnees, calendar = data_to_check_1, gap_variable = "gdp", key_variable = "country", time_variable = "year", digits = 2)

data_to_check_3 <- gap_to_fill(data = data_to_check_2, gap_variable = "gdp_corrected_1", key_variable = "country", time_variable = "year", digits = 1)

### OK !!!


### RESTE A FAIRE
# la création d'un calendrier plus complexe
# gérer le problème de get(key_variable)

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