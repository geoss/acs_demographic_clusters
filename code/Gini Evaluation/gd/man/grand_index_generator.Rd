\name{grand_index_generator}
\alias{grand_index_generator}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grand Index Generator
}
\description{
This function will produce a data frame with the index scores for each of the specified variables in the data set, organised by either Groups or Types.
}
\usage{
grand_index_generator(clusters, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusters}{
a vector or one column data frame of the clusters for each area.
}
  \item{data}{
the data as imported by \code{\link{input_geodemographic}}
}
}
\details{
This  grand index function creates index scores for each cluster (Group or Type) within the dataset, thus providing quick reference to the over or under represented characteristics. Numerators and denominators are those specified in the original data input.}
\value{
a data frame containing rows for each Group or Type cluster and columns for each numerator input variable
}

\author{
Alex Singleton, Liam Bratley
}


\examples{
data(liverpool_census)
data(specification)

data<-input_geodemographic(liverpool_census,specification)
prepared<-prepare_geodemographic(data)
types<-cluster_types(prepared,c(30,31),30)
type_30<-types[[1]]
type_30_clusters<-type_30$cluster
indexes<-grand_index_generator(type_30_clusters,data)
}

