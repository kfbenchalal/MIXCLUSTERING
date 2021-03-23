\name{My_Mix_clustering}
\alias{My_Mix_clustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Clustering of Mixed Data with Mixture Model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
    The funct make clusters, it takes as entry the data, number of clusters, number of iterations of EM algorithm, initialization clustering algorithm. It outputs the clusters, AIC BIC values, posterioti probabilities

}
\usage{
My_Mix_clustering(X, clust, iterations, initialisation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
 Dataframe, X which we apply clustering
}
  \item{clust}{
%%     ~~Describe \code{clust} here~~
integer, number of clusters the funct should ouput
}
  \item{iterations}{
%%     ~~Describe \code{nb_iter} here~~
integer, number of iterations of EM algorithm
}
  \item{initialisation}{
%%     ~~Describe \code{init} here~~
 "kmeans" or "random", the clustering algorithm for initialization
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{composante1 }{Description of 'composante1'}
%%  \item{composante2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
    Nour-Dass HAMMADI & Farida BENCHENLAL students M2 Data Mining at Lyon 2 Lumiere university

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

library(bayess)
library(MASS)
library(mvtnorm)
library(dplyr)
library(FactoMineR)
library(factoextra)

x <- mtcars
  x$vs = as.factor(x$vs)
  x$am = as.factor(x$am)
  x$gear = as.factor(x$gear)


## The function is currently defined as
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


mix_clust_kmeans <- My_Mix_clustering(x, 3, 20, 'kmeans')
mix_clust_random <- My_Mix_clustering(x, 3, 20, 'random')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
