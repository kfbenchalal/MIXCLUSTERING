#' Affichage des graphes
#'

#' @export


library(factoextra)
library(FactoMineR)


affiche_graphe <- function(x,resultat_mixclust){
  res.famd <- FAMD (x, ncp = 5, graph = FALSE)
  plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
       # col = res$z, pch=19, xlab = "Composante 1", ylab = "Composante 2")
       col =  resultat_mixclust$z, pch=19, xlab = "Composante 1", ylab = "Composante 2")
}



# x <- mtcars
# x$vs = as.factor(x$vs)
# x$am = as.factor(x$am)
# x$gear = as.factor(x$gear)
#
# mix_clust_kmeans <- My_Mix_clustering(x, 3, 20, 'kmeans')
# mix_clust_random <- My_Mix_clustering(x, 3, 20, 'random')


# set.seed(123)
# par(mfrow = c(1,2))
# affiche_graphe(x, mix_clust_kmeans)
# affiche_graphe(x, mix_clust_random)
#
# par(mfrow = c(1,2))
# plot(mix_clust_kmeans$log_vrai, main = "Log_vraissemblance par Kmeans")
# plot(mix_clust_random$log_vrai, main = "Log_vraissemblance par Random")
# res.famd <- FAMD (x, ncp = 5, graph = FALSE)
#
#
# par(mfrow = c(1,2))
# plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
#      col = mix_clust_kmeans$z, pch=19, xlab = "Composante 1", ylab = "Composante 2", main = "Mix Clustering par Kmeans")
# plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
#      col = mix_clust_random$z, pch=19, xlab = "Composante 1", ylab = "Composante 2", main = "Mix Clustering par Random")
#
#
#
#
