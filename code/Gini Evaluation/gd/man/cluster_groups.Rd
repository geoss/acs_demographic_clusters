\name{cluster_groups}
\alias{cluster_groups}


\title{
Create Group Level Using Hierarchical Clustering
}
\description{
Once the finest level of the classification is built using cluster_types, this function creates Groups using hierarchical clustering.
}
\usage{
cluster_groups(clusters, data, cutoff, new_data_frame = FALSE, 
               specification = NULL)
}

\arguments{
  \item{clusters}{
a vector (or a one column data frame) of the clusters. \code{\link{cluster_types}} will produce these and they will be in the workspace.
}
  \item{data}{
the data, either in the form originally produced by \code{\link{input_geodemographic}} or, a new data set where each row corresponds to those rows of the original data set (i.e. would have the same area ID). If a new data set is specified, new_data_frame must be TRUE and an appropriate specification file must be supplied. For more information about this, see \code{\link{input_geodemographic}}.
}
  \item{cutoff}{
the frequency of clusters required for the Group level clustering
}
  \item{new_data_frame}{
If the groupings are being made on a new data frame, this parameter must be TRUE. Otherwise it can be ignored.
}
  \item{specification}{
If a new data set is being grouped, a corresponding specification file is required.
}
}
\details{
This function plots a dendrogram, highlighting the breakpoints for the Group level clusters, and additionally returns a data frame containing the Type and Group for each observation in the data.


When creating the Group level classification, the function re-creates the input measures at Type level, using these cases as input to the Ward based clustering.
}
\value{
A data frame containing the Group and Type level clustering for each observation in the data. The rows represent the original zones, the first column is results of the type cluster, the second column is the result of the group clustering.
}

\author{
Alex D. Singleton, Liam Bratley
}
\seealso{
\code{\link{input_geodemographic}}
\code{\link{prepare_geodemographic}}
\code{\link{cluster_groups}}
}
\examples{
# First get the data in the right format
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


}

