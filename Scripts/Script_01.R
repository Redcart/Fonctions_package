###################################################################################################################################################
#######                                                                                                                                      ######
#######                                             Fonctions utiles (création de package ?)                                                 ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Idées de fonctions

# -	Fonction qui change automatiquement les \ en /
# -	Fonction qui crée la formule automatiquement sur les colonnes d’un dataframe
# -	Fonction qui recrée la proc freq de SAS
# -	Fonction pour la courbe de lift (arguments en input : probabilités prédites et true labels)

rm(list = ls())

### Chargement de packages

library(ggplot2)

###########################################
###  Changer de path windows ==> linux  ###
###########################################

windows_to_linux_path <- function()
{
  
  path <- readline("Input File: ")
  new_path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  return(new_path)
  
}

"C:\Users\scorde\Desktop\Data_Science"

windows_to_linux_path()


################################################
###  Créer une formule pour un modèle de ML  ###
################################################

create_formula <- function(data, position = 1)
{
  
  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)
  
}

create_formula(mtcars)


##########################
###  Proc Freq de SAS  ###
##########################

proc_freq <- function(variable, digits = 4)
{
  
  categories <- names(sort(table(variable)))
  frequencies <- as.vector(sort(table(variable)))
  
  n <- sum(frequencies)
  percentages <- round(frequencies / n, digits)*100
  cum_frequencies <- cumsum(frequencies)
  cum_percentages <- cumsum(percentages)
  
  result <- data.frame("Category" = categories, 
                       "Frequency" = frequencies, 
                       "Percentage" = percentages, 
                       "Cumulative.Frequency" = cum_frequencies,
                       "Cumulative.Percentage" = cum_percentages)
  
  return(result)
  
}


data <- iris

str(iris)

dat <- proc_freq(data$Species)


#####################################
###  Effet Lift / Courbe de lift  ###
#####################################


lift_effect <- function(predictions, true_labels, positive_label){
  
  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)
  
  data_1 <- data %>% 
    arrange(desc(predictions))
  
  data_2 <- data %>% 
    arrange(desc(true_labels))
  
  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(0, n, step+1)
  
    
    lift_1 <- c()
    
    for (i in 1:100){
      
      lift_1 <- c(lift_1, mean(data_1$true_labels[1:points[i]] == positive_label))
      
    }
    
    
    plot_1 <- ggplot() +
      geom_line(aes(x = points[-1], y = lift_1[-1]), color = "#56B4E9") +
      geom_hline(yintercept = mean(predictions), lty = "dashed", color = "grey") +
      coord_cartesian(ylim = c(0.05, 0.8)) +
      ggtitle("Courbe de l'effet Lift") +
      xlab("Effectif Cumulé") +
      ylab("% True Positive Label") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    plot_1
    
}


lift_curve <- function(predictions, true_labels, positive_label)
{
  
  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)
  
  data_1 <- data %>% 
    arrange(desc(predictions))
  
  data_2 <- data %>% 
    arrange(desc(true_labels))
  
  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(0, n, step+1)
    
  lift_2 <- c()
  truth_lift <- c()
  nb_positifs <- sum(data_1$true_labels == positive_label)
    
    
    for (i in 1:100){
      
      lift_2 <- c(lift_2, sum(data_1$true_labels[1:points[i]] == positive_label)/nb_positifs)
      truth_lift <- c(truth_lift, sum(data_2$true_labels[1:points[i]] == positive_label)/nb_positifs)
      
    }

    
    plot_2 <- ggplot() +
      geom_line(aes(x = points, y = lift_2)) +
      geom_line(aes(x = points, y = truth_lift)) +
      geom_segment(aes(x = 0, y = 0, xend = n, yend = 1), lty = "dashed", color = "grey") +
      coord_cartesian(ylim = c(0.05, 1)) +
      ggtitle("Courbe de Lift") +
      xlab("Effectif cumulé") +
      ylab("Lift") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    plot_2
  
  
}


