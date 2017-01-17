\name{enm.proast61}
\alias{enm.proast61}

\title{PROAST61.5 functionalities}

\description{Implementation of a continuous variable dose-response PROAST61.5 model scenario.}

\usage{
enm.proast61(dataset, predictionFeature, parameters)
}

\arguments{
  \item{dataset}{list of 2 objects, datasetURI:= character sring, code name of dataset, dataEntry:= data frame with 2 columns 
}
  \item{predictionFeature}{ character string specifying which is the prediction feature in dataEntry 
}
  \item{parameters}{ a list of one object called indiVariable to indicate the 'dose' variable. }
}
\details{Currently regression, exponential and Hill models are included for continuous data. For more details please see the PROAST61.5 manual file.
}
\value{A List 
  \item{singleCalculations}{ A print out of the PROAST report in screen.}
  \item{arrayCalculations}{ A pmml data frame with likelihood values for all models}
  \item{figures}{ Three graphs as produced by the PROAST 61.3 software}

}

\references{http://www.rivm.nl/en/}

\author{Georgia Tsiliki}

\note{Extra notes coming soon}

\seealso{http://www.rivm.nl/en/}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

data("dat1")

responseF<- "https://apps.ideaconsult.net/ambit2/feature/22137"
doseF<- list(indiVariable=c("https://apps.ideaconsult.net/ambit2/feature/22127"))

#.ypos<- 95
example1<- enm.proast61(dat1,responseF,doseF) 

}

\keyword{ enmPROAST }
