\name{ExceedanceProb}
\alias{ExceedanceProb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exceedance Probability
}
\description{
Computes exceedance probability at the hotspot from the INLA output.
}
\usage{
ExceedanceProb(INLAoutput, dist, quantile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{INLAoutput}{
INLA model object obtained using inla().
}
  \item{dist}{
The INLA likelihood distribution used in the modeling.
}
  \item{quantile}{
The quantile to which exceedance probability is to be computed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns exceedance probability, highest posterior density of the hotspot, and posterior mean.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}