\name{prepare_geodemographic}
\alias{prepare_geodemographic}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Preparation
}
\description{
This function formats data for creating a geodemographic, performing specified transformations. Information about these are stored in attributes using input_geodemographic. The return value is a matrix of index scores, percentages and or non count data, that are standardised and ready to be clustered.
}
\usage{
prepare_geodemographic(data, compare = NULL, remove = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the data to be transformed.
}
  \item{compare}{
the method used to produce information that helps you to compare variables after the transformation. Choose from a correlation matrix ("correlation_matrix") and an enhanced heatmap ("heatmap"), default is NULL.
}
  \item{remove}{
this option removes specified variables from the transformed data frame. Must be a character vector specifying individual variable names for removal as character strings.
}
}


\value{
A matrix of scaled values that can be used to create clusters..
}

\author{
Alex D. Singleton, Liam Bratley
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{input_geodemographic}}
}
\examples{

## It is imperative that input_geodemographic is run first on the data
## The resulting value should be entered as the "data" parameter here
data(specification)
data(liverpool_census)
data<-input_geodemographic(liverpool_census,specification)

## This will produce a heatmap so that variables can be compared after transformation

transformed_values<-prepare_geodemographic(data,compare="heatmap")

## If unhappy with certain variables, specify them in the remove parameter and run the 
## function again

transformed_values_2<-prepare_geodemographic(data,remove=c("age15"))

## Now transformed_values_2 will have the specified variables removed from the output
}

