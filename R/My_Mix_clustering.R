#' Mix Clustering


#' @export

library(dplyr)
library(tidyverse)
library(gtools)
library(FactoMineR)
library(bayess)
library(mvtnorm)
library(devtools)



My_Mix_clustering <-
  function(X, clust, iterations, initialisation) {
    set.seed(123)
    n <- nrow(X)    # nombre de lignes
    col <- ncol(X)  # nombre de colonnes


    ## S?paration des variables qualitatives et variables quantitatives:


    ## Varibales quantitatives
    donnees_quati <- as.matrix(X %>% select_if(is.numeric))
    col <- ncol(donnees_quati)
    ## Variables qualitatives
    donnees_quali <- X %>% select_if(is.factor)
    col_quali <- ncol(donnees_quali)

    mod <- sapply(seq(col_quali), function(i) {
      length(levels(donnees_quali[, i]))
    })
    # initialisation des objets
    prop <- matrix(NA, iterations + 1, clust)
    mu <- array(NA, dim = c(iterations + 1, clust, col))
    sigma <- array(NA, dim = c(iterations + 1, clust, col, col))
    alpha <- array(NA, dim = c(iterations + 1, clust, col_quali))
    mode(alpha) <- "list"
    log_vrai <- rep(0, iterations + 1)
    # initialisation de l'algorithme => random/kmeans
    if (initialisation == 'random') {
      prop[1,] <- rdirichlet(1, par = rep(1, clust))
      mu[1, ,] <- donnees_quati[sample(1:n, clust),]
      for (k in 1:clust)
        sigma[1, k, ,] <- rWishart(1, 8, var(donnees_quati))
    }
    if (initialisation == 'kmeans') {
      z <- kmeans(donnees_quati, clust)$clust
      for (k in 1:clust) {
        prop[1, k] <- mean(z == k)
        mu[1, k,] <- colMeans(donnees_quati[which(z == k),])
        sigma[1, k, ,] <- var(donnees_quati[which(z == k),])
      }
    }
    # initialisation des parametres
    for (k in 1:clust) {
      for (i in 1:col_quali) {
        alpha[1, k, i] <- list(rdirichlet(1, rep(1, mod[i])))
        names(alpha[1, k, i][[1]]) <- levels(donnees_quali[, i])
      }
    }
    # calcul de log de vraisemblance
    for (i in 1:n) {
      tmp <- 0
      for (k in 1:clust) {
        fk <- 1
        for (j in 1:col_quali) {
          fk <-fk * alpha[1, k, j][[1]][donnees_quali[i, j]]
        }
        tmp <-
          tmp + prop[1, k] * (fk * dmvnorm(donnees_quati[i,], mu[1, k,], sigma[1, k, ,]))
      }
      log_vrai[1] <- log_vrai[1] + log(tmp)
    }
    # algorithme EM
    for (iter in 1:iterations) {
      #E-step
      tik <- matrix(NA, n, clust)
      for (k in 1:clust) {
        fk <- 1
        for (i in 1:col_quali) {
          fk <- fk * alpha[iter, k, i][[1]][donnees_quali[, i]]
        }
        tik[, k] <-
          prop[iter, k] * (fk + dmvnorm(donnees_quati, mu[iter, k,], sigma[iter, k, ,]))
      }
      tik <- tik / rowSums(tik)
      #M-step
      for (k in 1:clust) {
        nk <- sum(tik[, k])
        prop[iter + 1, k] <- nk / n
        mu[iter + 1, k,] <- colSums(tik[, k] * donnees_quati) / nk
        sigma[iter + 1, k, ,] <- Reduce('+', lapply(1:n, function(m) {
          tik[m, k] * (donnees_quati[m,] - mu[iter + 1, k,]) %*% t(donnees_quati[m,] - mu[iter + 1, k,]) / nk
        }))
        for (i in 1:col_quali) {
          alpha[iter + 1, k, i] <- list(sapply(1:mod[i], function(a) {
            sum(tik[, k] * (donnees_quali[, i] == levels(donnees_quali[, i])[a])) / nk
          }))
          names(alpha[iter + 1, k, i][[1]]) <- levels(donnees_quali[, i])
        }
      }
      #calcul de log vraisemblance
      for (i in 1:n) {
        tmp <- 0
        for (k in 1:clust) {
          fk <- 1
          for (j in 1:col_quali) {
            fk <-fk * alpha[iter + 1, k, j][[1]][donnees_quali[i, j]]
          }
          tmp <-
            tmp + prop[iter + 1, k] * (fk * dmvnorm(donnees_quati[i,], mu[iter + 1, k,], sigma[iter + 1, k, ,]))
        }
        log_vrai[iter + 1] <- log_vrai[iter + 1] + log(tmp)
      }
    }
    z <- max.col(tik)
    BIC <- log_vrai[iterations + 1] - clust / 2 * log(n)
    ICL <-  BIC - sum(tik * log(tik), na.rm = TRUE)

    return(
      list(
        prop = prop,
        mu = mu,
        sigma = sigma,
        clust = clust,
        log_vrai = log_vrai,
        z = z,
        BIC = BIC,
        ICL = ICL
      )
    )
  }


