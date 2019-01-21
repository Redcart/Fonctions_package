###################################################################################################################################################
#######                                                                                                                                      ######
#######                                             Fonctions utiles (création de package ?)                                                 ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Idées de fonctions

# -	Fonction qui change automatiquement les \ en /
# -	Fonction qui crée la formule automatiquement sur les colonnes d’un dataframe
# -	Fonction qui recréé la proc freq de SAS
# -	Fonction pour la courbe de lift (arguments en input : probabilités prédites et données étiquettées)
# - Fonction pour calculer la variance intraclasse pour une CAH (à la manière d'un kmeans)

rm(list = ls())

### Chargement de packages

library(ggplot2)
library(dplyr)

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


###############################################
###  Calcul de la variance intraclasse CAH  ###
###############################################

# Fonction qui permet de calculer le centroïde des clusters sur un data frame 

clust.centroid <- function(i, data, cluster)
{
  
  colMeans(data[cluster == i,])# on obtient les coordonnées pour chaque variable des centroïdes de chaque cluster
  
}


compute_variance_ahc <- function(data, max_clusters = 10)
{
  
  n <- dim(data)[1]
  
  variance_interclasse_ahc <- c()
  
  model_ahc <- hclust(d = dist(data), method = "ward.D")

  # Etape 1: Trouver les centroïdes des différents clusters et le centroïde global
  # Etape 2: Calculer les écarts quadratiques des centroïdes au centroïde global
  # Etape 3: Faire la moyenne des écarts quadratiques en prenant en compte le nombre total de clusters

  for (i in 2: max_clusters){
  
    ahc_clusters <- cutree(tree = model_ahc, k = i)
  
    centroids <- sapply(unique(ahc_clusters), clust.centroid, data, ahc_clusters)
  
    centroids <- centroids %>% cbind(rowMeans(centroids))
  
    squares <- (centroids[, -(i+1)] - centroids[, (i+1)])^2
  
    frequencies <- as.vector(table(ahc_clusters)) / n
    
    variance_interclasse_ahc <- c(variance_interclasse_ahc, sum(apply(squares, 2, sum)*frequencies))
  
  }

  return(variance_interclasse_ahc)

}

data <- mtcars
str(data)

compute_variance_ahc(data = data)

# Question de l'évolution de l'inertie qui n'est pas stricte lorsque le nombre de cluster augmente 