\name{x}
\alias{x}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
    DataFrame stadard of R, which represents cars
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
    DataFrame of carts wich containts continious and categorical features

}
\usage{data("x")}
\format{
  A data frame with 32 observations on the following 11 variables.
  \describe{
    \item{\code{mpg}}{a numeric vector}
    \item{\code{cyl}}{a numeric vector}
    \item{\code{disp}}{a numeric vector}
    \item{\code{hp}}{a numeric vector}
    \item{\code{drat}}{a numeric vector}
    \item{\code{wt}}{a numeric vector}
    \item{\code{qsec}}{a numeric vector}
    \item{\code{vs}}{a factor with levels \code{0} \code{1}}
    \item{\code{am}}{a factor with levels \code{0} \code{1}}
    \item{\code{gear}}{a factor with levels \code{3} \code{4} \code{5}}
    \item{\code{carb}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(x)
x <- mtcars
x$vs = as.factor(x$vs)
x$am = as.factor(x$am)
x$gear = as.factor(x$gear)
## maybe str(x) ; plot(x) ...
}
\keyword{datasets}
