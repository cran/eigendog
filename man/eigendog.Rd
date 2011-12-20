\name{eigendog}
\title{eigendog}

\alias{eigendog}
\alias{eigendog.trainResult}
\alias{eigendog.predict}

\description{Load and evaluate a binary classification model produced by
  EigenDog's web-service.  For the latest version of this code, see
  \url{https://github.com/EigenDog/decision-trees}. }

\usage{
eigendog.trainResult( fileName )
eigendog.predict( model, X, checkOrdinalDomain )
}

\arguments{
  \item{fileName}{ file system path of an EigenDog train result file }
  \item{model}{ EigenDog model, a component of the list resturned by
    \code{ eigendog.trainResult} }
  \item{X}{ a list containing values for each of the model's
    features }
  \item{checkOrdinalDomain}{ whether to complain when an ordinal feature value
    lies beyond the domain observed in the training set }
}

\author{The EigenDog Team, \url{https://www.eigendog.com} }

\examples{
# see https://www.eigendog.com/#tutorial for an explanation of this model
blobsMod <- system.file( "blobs.mod", package="eigendog" );
tr <- eigendog.trainResult( blobsMod );
print( tr$model$features );
X <- list( x=10 , y=11, image="c" );
p <- eigendog.predict( tr$model, X, TRUE );
print( paste("probability that pixel is", tr$model$positiveCategory, "=", p ) );
}

