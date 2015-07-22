\name{split_types}
\alias{split_types}
\title{
Split Type Level Clusters
}
\description{
This function splits types (such as those with large populations)  into smaller clusters
}
\usage{
split_types(data, clusters, clusters_to_split, number_splits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
this is the data after it has been inputted by \code{\link{input_geodemographic}}, i.e. with its' attributes set.
}
  \item{clusters}{
a one column data frame giving all of the clusters for each of the rows. Note that this will be outputted to the workspace when \code{\link{cluster_types}} is called.
}
  \item{clusters_to_split}{
a vector of the clusters to be splitted
}
  \item{number_splits}{
a vector of equal length to the clusters to split vector, where each element represents how many splits to do on each cluster.
}
}
\details{
For each cluster specified for splitting, kmeans is performed again for this subset of records, with k taking values from the number split vector. The new clusters will be given the numbers ascending from the original K.}
\value{
a vector of the new clusters.
}

\author{
Alex D. Singleton, Liam Bratley
}


\seealso{
\code{\link{merge}}
}
\examples{
data(specification)
data(liverpool_census)
data<-input_geodemographic(liverpool_census,specification)
standardised<-prepare_geodemographic(data)
## This runs kmeans 100 times (default) with k=62, outputting the best result in a list
K_means_list_1<-cluster_types(standardised,62)

K_62<-K_means_list_1[[1]]
K_62_clusters<-as.vector(K_62$cluster)
## Now if cluster 27 looks too big, can split it into 2 using split_types
ts_split<-split_types(data,K_62_clusters,27,2)
## Cluster 27 has now become 61 and 61
}

