\name{QuantResidTMS}
\alias{QuantResidTMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile residual
}
\description{
Computes the quantile residual from INLA output object.
}
\usage{
QuantResid(INLAoutput,y,dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{INLAoutput}{
INLA model object obtained using inla().
}
\item{y}{
The target variable.
}
\item{dist}{
The INLA likelihood distribution used in the modeling.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns quantile residuals.
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
