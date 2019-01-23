###################################################################################################################################################
#######                                                                                                                                      ######
#######                                                       Fonctions utiles                                                               ######
#######                                                     S.CORDE Décembre 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Fonctions codées dans ce script

# -	Procédure permettant d'obtenir des clusters respectant une certaine volumétrie via l'algorithme kmeans

rm(list = ls())

### Chargement de packages

library(ggplot2)
library(dplyr)

###########################################
###  Fonction pour la procédure Kmeans  ###
###########################################

kmeans_procedure <- function(data, columns, threshold_min, threshold_max, verbose = FALSE){
  
  
  set.seed(42)
  
  i <- 1
  b <- 1
  k <- 2
  c <- 1
  
  test <- FALSE
  
  n_observation <- 1:dim(data)[1]
  cluster <- NA
  
  
  liste_clusterise <- data.frame(n_observation, cluster)
  table(liste_clusterise$cluster)
  
  ## Tant que tous les clusters ne sont pas calculés
  
  while (test == FALSE){
    
    if (verbose == TRUE){
      
    print(paste("boucle : " , b, " nb de clusters : ", k, " N° cluster : ", c))
    
    }
    
    
    liste_temp <- liste_clusterise %>% 
      filter(is.na(cluster)) %>% 
      rename(cluster_bis = cluster) %>% 
      select(n_observation, cluster_bis)
    
    
    ## On clusterise les PDV qui n'ont pas encore de cluster (= NA)
    set.seed(42)
    
    assign(paste("model_kmean_", b, ".", k, sep = ""), 
           kmeans(data[liste_temp$n_observation, columns], centers = k, iter.max = 50, nstart = 5, algorithm = "Lloyd"))
    
    assign(paste("volumetrie_cluster_", b, ".", k, sep = ""), 
           table(eval(as.name(paste("model_kmean_", b, ".", k, sep = "")))$cluster))
    
    eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = "")))
    
    # Si au moins un des clusters est compris entre le seuil minimum et le seuil maximum
    if (any(between(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))), threshold_min, threshold_max))) {
      
      # On fige le(s) cluster(s) concerné(s)
      for (i in 1:k){
        
        #print(paste("boucle : " , b, " nb de clusters : ", k, " N° cluster : ", c)) 
        
        # On fige également les clusters qui seraient passés au dessous du seuil minimum pluôt que de revenir au kmeans précédent
        if (eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = "")))[i] <= threshold_max) {
          
          liste_temp$cluster_bis[eval(as.name(paste("model_kmean_", b, ".", k, sep = "")))$cluster==i] <- c
          
          liste_clusterise <- liste_clusterise %>% 
            left_join(liste_temp, by = "n_observation") %>% 
            mutate(cluster = ifelse(is.na(cluster), cluster_bis, cluster)) %>% 
            select(-cluster_bis)
          
          c <- c + 1
          
        }
        
      }
      
      b <- b + 1
      k <- 2
      
    }          
    
    # Si tous les clusters sont au dessus du seuil maximum, on continue le clustering
    else if (all(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))) > threshold_max)){  
      
      k <- k + 1 
      
    } 
    
    # Si un cluster est en dessous du seuil minimum (et donc que tous les autres sont au dessus du seuil maximum)
    else if (any(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))) < threshold_min)){
      
      # On fige le(s) cluster(s) concerné(s)
      for (i in 1:(k-1)){
        liste_temp$cluster_bis[eval(as.name(paste("model_kmean_", b, ".", k-1, sep = "")))$cluster==i] <- c
        
        liste_clusterise <- liste_clusterise %>% 
          left_join(liste_temp, by = "n_observation") %>% 
          mutate(cluster = ifelse(is.na(cluster), cluster_bis, cluster)) %>% 
          select(-cluster_bis)
        
        c <- c + 1
        
      }
      
    }
    
    else{# test pour savoir si nous avons oublié un cas
      
      print("Attention un autre cas non pris en compte existe !")
      
    }
    
    test <- sum(is.na(liste_temp$cluster)) == 0     
    
    if (verbose  == TRUE){
      
      print(table(liste_clusterise$cluster))
      
    }
    
  }

  results <- list()
  results$clusters <- liste_clusterise$cluster
  results$frequencies <- table(liste_clusterise$cluster)
  
  return(results)
  
}

##################################################
###  Exemple de fonctionnement de la fonction  ###
##################################################

data()

data <- trees
str(data)

final <- kmeans_procedure(data = data, columns = 1:3, threshold_min = 3, threshold_max = 5)
final
