\name{describe_types}
\alias{describe_types}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
describe_types
}
\description{
This function produces bar plots (for non count variables) and a heatmap for the count variables. 
}
\usage{
describe_types(cluster_lookup, data_set, spec_file=NULL,new_data_frame=FALSE,
               heatmap=TRUE,bar=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster_lookup}{
a vector (or a one column data frame) of the clusters. cluster_types will produce these and they will be in the workspace. Alternatively, a CSV file of clusters can be read in from the file. This must be a one column table listing the clusters for each row.
}
  \item{data_set}{
the data, either in the form originally produced by \code{\link{input_geodemographic}} or, a new data set where each row corresponds to those rows of the original data set (i.e. would have the same area ID). If a new data set is specified, new_data_frame must be TRUE and an appropriate specification file must be supplied. For more information about this, see \code{\link{input_geodemographic}}.
}
  \item{spec_file}{
If a new data set is being described, a new corresponding specification file is required. 
}
  \item{new_data_frame}{
  boolean: If TRUE, the paths to the new data set and spec file must be given, make sure the correct working directory is used. If false only the original data produced by \code{\link{input_geodemographic}} is required.
  }
    \item{heatmap}{
boolean: If TRUE, a heatmap of the count variables is produced. Defaults to TRUE.
}
  \item{bar}{
boolean: If TRUE, bar plots for each of the non-count variables is produced. Defaults to TRUE.
}
}
\details{
This function calculates the index scores for all count variables, and if specified, prints a heatmap. On the heatmap the y axis represents each of the Types and the x axis each of the variables. The range of values in the heatmap are:

Index < 50 = dark green
50 < Index < 80 = light green
80 < Index < 120 = grey
120 < Index < 150 = light blue
150 < Index = steel blue.

The function also produces bar plots of the averages for each non-count variable if specified.
}
\value{
'describe' object; the first part of which contains the heatmap of the count variables and the second part containing a further list of bar plots for the non-count variables.

Usage
describe[["counts"]] = heatmap of the count variables
describe[["noncounts"]][[1]] = first bar plot relating to the first non count variable. For other non-count variables, just change the numeric subscript.

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
## describe types
desc<-describe_types(clusters,data)
}
