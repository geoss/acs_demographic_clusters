\name{evaluate_classification}
\alias{evaluate_classification}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate A Geodemographic Classification
}
\description{
This function produces a data frame containing a gini coefficient for each variable. It also prints a  Lorenz curve for variables specified, and a bar chart showing the gini coefficient for each variable in descending order.
}
\usage{
evaluate_classification(data, cluster_lookup, groups_or_types, variables)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the data, in the form produced by \code{\link{input_geodemographic}}
}
  \item{cluster_lookup}{
a dataframe of cluster lookups, This should be the result of \code{\link{cluster_lookup}}.
}
\item{groups_or_types}{
Choose which classification you would like to evaluate. One of "groups" or "types" must be specified, or a warning message will be generated.
}
  \item{variables}{
a list of variables used for calculating Gini coefficients and Lorenz curves 
}
}
\value{
'eval' list object with three elements:

eval[["variable_data"]][[1]] = list of the data frames containing calculation information for the calculation of the gini coefficients for each specified variable; the evaluation data for the first specified variable would be printed.
eval[["lorenz""]] = Lorenz curves for specified variables.
eval[["gini""]] = graph of gini coefficients for various variables.

}

\author{
Alex D. Singleton, Liam Bratley
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
lookup<-cluster_lookup(cluster_results)

## Now get some evaluatory output by calling
 eval<-evaluate_classification(data,lookup,"groups",c("age0_4","age10_14","single_pensioner"))
}

