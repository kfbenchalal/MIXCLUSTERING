\name{affiche_graphe}
\alias{affiche_graphe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
    affiche_graphe du clustering results
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
the function outputs the loglikelihood and the clusters in 2D plot.
}
\usage{
affiche_graphe(resultat_mixclust)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{resultat_mixclust}{
%%     ~~Describe \code{clustersRes} here~~
      the output of MIXCLUSTERING.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Nour-Dass HAMMADI & Farida BENCHENLAL Student M2 Lyon 2 Lumiere University
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


function(resultat_mixclust){
  res.famd <- FAMD (x, ncp = 5, graph = FALSE)
  plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
       # col = res$z, pch=19, xlab = "Composante 1", ylab = "Composante 2")
       col =  resultat_mixclust$z, pch=19, xlab = "Composante 1", ylab = "Composante 2")
}





set.seed(123)

mix_clust_kmeans <- My_Mix_clustering(x, 3, 20, 'kmeans')
mix_clust_random <- My_Mix_clustering(x, 3, 20, 'random')

par(mfrow = c(1,2))
affiche_graphe(mix_clust_kmeans)
affiche_graphe(mix_clust_random)

par(mfrow = c(1,2))
plot(mix_clust_kmeans$log_vrai, main = "Log_vraissemblance par Kmeans")
plot(mix_clust_random$log_vrai, main = "Log_vraissemblance par Random")
res.famd <- FAMD (x, ncp = 5, graph = FALSE)


par(mfrow = c(1,2))
plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
     col = mix_clust_kmeans$z, pch=19, xlab = "Composante 1", ylab = "Composante 2", main = "Mix Clustering par Kmeans")
plot(res.famd$ind$coord[,1], res.famd$ind$coord[,2],
     col = mix_clust_random$z, pch=19, xlab = "Composante 1", ylab = "Composante 2", main = "Mix Clustering par Random")








% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
