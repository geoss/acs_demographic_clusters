\name{cluster_types}
\alias{cluster_types}


\title{Create Types Using K means Clustering}
\description{
This function creates the "Type" level of a geodemographic classification using k-means. In a single run of the function, a range of k values (cluster frequencies) can be specified, and for each of these  k values, multiple runs also set. It also produces a dataset of the results of the runs.
}
\usage{
cluster_types(standardized_data, k_values = 60, n_iter = 100)
}

\arguments{
  \item{standardized_data}{
a dataframe containing a set of standardised data (i.e. with the percentages/index scores of numerators already calculated), such as that created by using both \code{\link{input_geodemographic}} and \code{\link{prepare_geodemographic}}
}
  \item{k_values}{
a vector or single value specifying the frequency of clusters required
}
  \item{n_iter}{
the number of times to re-run each k value. More runs will likely result in a more stable classification, however, increase the time taken to run the function. The default value is 100.
}

}
\details{
Within each set of runs for a specified k value, total within sum of squares is calculated and compared to assess how well each result represents the input data. Repeat runs are necessary for each value of k because the algorithm is unstable, being initially set with random seeds.

Returned from the function is a list of the "best" k-means objects for each specified value of K; containing a vector of initial seed locations for reproducibility and an area identifier to cluster lookup table. The final element of this list is a data frame with the total within sum of squares for each individual run. Additional printed output includes size of the clusters, means of the clusters and the cluster numbers in a vector. 
}
\value{
List K-Means objects.
\item{kmeans1 }{Best run for first specified k-value}
\item{kmeans2 }{Best run for second specified k-value}
...
\item{TWSS_data}{data about each run}
}
\author{
Alex D. Singleton, Liam Bratley
}



\seealso{
\code{\link{input_geodemographic}}
\code{\link{prepare_geodemographic}}

}
\examples{
## First get the data in the right format
data(specification)
data(liverpool_census)
data<-input_geodemographic(liverpool_census,specification)
standardised<-prepare_geodemographic(data)
## This runs kmeans 100 times (default) with k=62, outputting the best result in a list
K_means_list_1<-cluster_types(standardised,62)
}

