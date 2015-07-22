\name{cluster_lookup}
\alias{cluster_lookup}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create A Lookup Table From The Results Of The Cluster Analysis
}
\description{
This function produces a data frame, showing each area with its associated Group and Type.
}
\usage{
cluster_lookup(results)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
a data frame or matrix of cluster lookups, where each row corresponds to each area, the first column represents the Type level clustering, and the second column represents the Group clustering.
}
}
\details{
Please note that this function will take the the row names as the area names/codes, and this is what will appear in the output data frame created by the function. 
}
\value{
a data frame with three columns, the first column corresponds to the first column of original data (area ID), the second column will be Group, and the third the Type. Groups are named using letters, and Types are attributed their parent Group letter followed by a sequential number.
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
clusters<-K_62$cluster
## Produces cluster results for 10 larger clusters, i.e. groups
cluster_results<-cluster_groups(clusters,data,10)

## Produces a data frame of the two results together. Note the numbers will go up to 62 
## and the letters will go up to J (the 10th letter of the alphabet).

final_table<-cluster_lookup(cluster_results)
}

