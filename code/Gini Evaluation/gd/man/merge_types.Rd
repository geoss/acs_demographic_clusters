\name{merge_types}
\alias{merge_types}

\title{
Cluster Merger
}
\description{
This function allows the user to merge together two or more Type level clusters. This could be used, for example, to amalgamate two small and/or similar clusters.}
\usage{
merge_types(clusters, clusters_to_merge)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusters}{
a vector or one column data frame giving the cluster number assigned to each of the rows. Note that this will be outputted to the workspace when cluster_types is called.
}

  \item{clusters_to_merge}{
this is a vector of the clusters to merge
}
}
\details{
The clusters to be merged will form a new cluster that is given the number of the original K+1, and the old cluster numbers will not feature in the output.
}
\value{
a vector of the new clusters.
}

\author{
Alex D. Singleton, Liam Bratley
}

\examples{
## First get the data in the right format
data(specification)
data(liverpool_census)
data<-input_geodemographic(liverpool_census,specification)
standardised<-prepare_geodemographic(data)
## This runs kmeans 100 times (default) with k=62, outputting the best result in a list
K_means_list_1<-cluster_types(standardised,62)

K_62<-K_means_list_1[[1]]
K_62_clusters<-as.vector(K_62$cluster)
## Say, for example, clusters 16, 17 and 18 look similar, merge them.
merged<-merge_types(Clusters_for_K_62,c(16,17,18))
## Clusters 16, 17 and 18 have now become cluster number 63.

}

