\name{IntPROAST61v5-package}
\alias{IntPROAST61v5-package}
\alias{IntPROAST61v5}
\docType{package}
\title{
\packageTitle{IntPROAST61v5}
}

\description{Implementation of two PROAST61v5 dose-response models for continuous and categorical data scenarios.}

\details{

The DESCRIPTION file:
\packageDESCRIPTION{IntPROAST61v5}
\packageIndices{IntPROAST61v5}
Our plan is to implement functions that can adopt the specifications of particular analysis scenarios for continuous and categorical data. 
}

\author{
\packageAuthor{IntPROAST61v5}

Maintainer: \packageMaintainer{IntPROAST61v5}
}

\references{http://www.rivm.nl/en/}

\keyword{ package }

\seealso{http://www.rivm.nl/en/}

\examples{
data("dat1")

responseF<- "https://apps.ideaconsult.net/ambit2/feature/22137"
doseF<- list(indiVariable=c("https://apps.ideaconsult.net/ambit2/feature/22127"))

#.ypos<- 95
example1<- enm.proast61(dat1,responseF,doseF) 
}
