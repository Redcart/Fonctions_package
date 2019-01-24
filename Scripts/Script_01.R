###################################################################################################################################################
#######                                                                                                                                      ######
#######                                                       Fonctions utiles                                                               ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Fonctions codées dans ce script

# -	Fonction qui change automatiquement les \ en /
# -	Fonction qui créé la formule automatiquement sur les colonnes d’un dataframe
# -	Fonction qui recréé la proc freq de SAS
# -	Fonction pour la courbe de lift (arguments en input : probabilités prédites et données étiquettées)
# - Fonction pour calculer la variance intraclasse pour une CAH (à la manière d'un kmeans)

rm(list = ls())

### Chargement des packages

library(ggplot2)
library(dplyr)

###########################################
###  Changer de path windows ==> linux  ###
###########################################

# arguments:
  # input requis dans la console

windows_to_linux_path <- function()
{
  
  path <- readline("Input File: ")
  new_path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  return(new_path)
  
}

#windows_to_linux_path()

#C:\Users\scorde\Desktop\Data_Science

################################################
###  Créer une formule pour un modèle de ML  ###
################################################

# arguments:
  # data: data frame
  # position: numéro de colonne de la variable d'intérêt (que l'on souhaite prédire)

create_formula <- function(data, position = 1)
{
  
  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)
  
}

#create_formula(mtcars)


##########################
###  Proc Freq de SAS  ###
##########################

# arguments:
  # variable: variable catégorielle sur laquelle on veut faire la proc freq
  # digits: nombre de chiffres après la virgule pour l'arrondi

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


# data <- iris
# 
# str(data)
# 
# dat <- proc_freq(data$Species)


#####################################
###  Effet Lift / Courbe de lift  ###
#####################################

# arguments:
  # predictions: probabilités prédites par un modèle statistique
  # true_labels: étiquettes rélles des individus statistiques
  # positive_label: nom de la modalité correspondant au label positif (Y = 1)

lift_effect <- function(predictions, true_labels, positive_label)
{
  
  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)
  
  data_1 <- data %>% 
    arrange(desc(predictions))
  
  data_2 <- data %>% 
    arrange(desc(true_labels))
  
  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(1, n, step)
  quantiles <- quantile(0:n)
                        
  lift_1 <- c()
  
  for (i in 1:100)
  {
    
    lift_1 <- c(lift_1, mean(data_1$true_labels[1:points[i]] == positive_label))
    
  }
  
  plot_1 <- ggplot() +
    geom_line(aes(x = points, y = lift_1), color = "#56B4E9") +
    geom_hline(yintercept = mean(true_labels == positive_label), lty = "dashed", color = "grey") +
    coord_cartesian(ylim = c(0.05, 0.8)) +
    scale_x_continuous(breaks = as.vector(quantiles), labels = names(quantiles)) +
    ggtitle("Effect Lift Curve") +
    xlab("Cumulative Population") +
    ylab("% True Positive Label") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  return(plot_1)
    
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
  points <- seq(1, n, step)
    
  lift_2 <- c()
  truth_lift <- c()
  nb_positifs <- sum(data_1$true_labels == positive_label)
  quantiles <- quantile(0:n)
  
  for (i in 1:100)
   {
      
      lift_2 <- c(lift_2, sum(data_1$true_labels[1:points[i]] == positive_label)/nb_positifs)
      truth_lift <- c(truth_lift, sum(data_2$true_labels[1:points[i]] == positive_label)/nb_positifs)
      
   }

  plot_2 <- ggplot() +
     geom_line(aes(x = points, y = lift_2), color = "#56B4E9") +
     geom_line(aes(x = points, y = truth_lift)) +
     geom_segment(aes(x = 0, y = 0, xend = n, yend = 1), lty = "dashed", color = "grey") +
     coord_cartesian(ylim = c(0.05, 1)) +
     scale_x_continuous(breaks = as.vector(quantiles), labels = names(quantiles)) +
     ggtitle("Lift Curve") +
     xlab("Cumulative Population") +
     ylab("Lift") + 
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5)) 
    
  return(plot_2)
  
}


###############################################
###  Calcul de la variance intraclasse CAH  ###
###############################################

# arguments:
  # i: numéro du cluster
  # data: data frame
  # cluster: nom de la variable indiquant le cluster

clust.centroid <- function(i, data, cluster)
{
  
  return(colMeans(data[cluster == i,]))# on obtient les coordonnées pour chaque variable des centroïdes de chaque cluster
  
}

# arguments:
  # data: data frame (uniquement variables numériques)

compute_inertia <- function(data)
{
  
  n <- dim(data)[1]
  
  data_with_center <- t(data %>% rbind(colMeans(data)))
  
  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2
  
  inertia <- sum(apply(squares, 2, sum)) / n
  
  return(inertia) 
  
}

#compute_inertia(mtcars)


# arguments:
  # data: data frame (uniquement variables numériques)
  # max_clusters: nombre maximal de clusters pour le quel on fait calcule la variance interclasse

# Etape 1: Trouver les centroïdes des différents clusters et le centroïde global
# Etape 2: Calculer les écarts quadratiques des centroïdes au centroïde global
# Etape 3: Faire la moyenne des écarts quadratiques en prenant en compte le nombre total de clusters et la volumétrie par cluster

compute_inertia_ahc <- function(data, max_clusters = 10)
{
  
  n <- dim(data)[1]
  
  intergroup_inertia_ahc <- c()
  
  model_ahc <- hclust(d = dist(data), method = "ward.D")

  for (i in 2: max_clusters)
   {
  
    ahc_clusters <- cutree(tree = model_ahc, k = i)
  
    centroids <- sapply(unique(ahc_clusters), clust.centroid, data, ahc_clusters)
  
    centroids <- centroids %>% cbind(rowMeans(centroids))
  
    squares <- (centroids[, -(i+1)] - centroids[, (i+1)])^2
  
    frequencies <- as.vector(table(ahc_clusters)) / n
    
    intergroup_inertia_ahc <- c(intergroup_inertia_ahc, sum(apply(squares, 2, sum)*frequencies))
  
   }

  return(intergroup_inertia_ahc)

}

# data <- mtcars
# str(data)
# 
# compute_inertia_ahc(data = data)

### RESTE A FAIRE
# Question de l'évolution de l'inertie qui n'est pas stricte lorsque le nombre de cluster augmente 