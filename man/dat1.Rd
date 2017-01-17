\name{dat1}
\alias{dat1}
\docType{data}

\title{A sample data object}

\description{
A data frame from the protein-corona data set [Walkey et al., 2014]
}

\usage{data("dat1")}

\format{
  A list of two objects
  \describe{
    \item{datasetURI}{a character vector- ambit data set uri}
    \item{dataEntry}{a data frame containing two columns: compound and values. Compound is a character vector with all compound ambit uris, and values is a data frame with all numberic values of the protein corona data set (compounds by features). One of the columns is the dependent variable (Net.cell.association) which has some null values- the experimental design algorithm will select some/all of them for next suggested trials.}
  }
}

\details{
Please see Walkey et al., 2014
}

\source{Walkey, Carl D., et al. "Protein corona fingerprinting predicts the cellular interaction of gold and silver nanoparticles." ACS nano 8.3 (2014): 2439-2455.}

%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\examples{
data(dat1)
## maybe str(dat1) ; plot(dat1) ...
}

\keyword{datasets}

