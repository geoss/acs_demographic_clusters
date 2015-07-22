\name{visualise_classification}
\alias{visualise_classification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualise the results of the classification
}
\description{
This description function plots bar plots for each specified variable, indicating the index scores by Type and by Group. This function should be use to visualize the results of the Group level classification.
}
\usage{
visualise_classification(cluster_lookup, data_set, spec_file,new_data_frame,  
                         variables,colours)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster_lookup}{
a data frame; this is a lookup table whose expected format is identical to that produced by \code{\link{cluster_lookup}}. The first column is the area ID values, the second column the Group and Type labels respectively. These can also be read in from file, given that it is in the aforementioned format.
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
  \item{variables}{
  required: a character vector of the variable names that should be visualised.
  }
   \item{colours}{
  default is NULL, allows the user to select theyre own colours used to depict the groups in bar plots. Must be a character vector of colour names the same length as variables.
  }
}
\details{
This produces four bar plots per plot window. Each bar on the plot indicates the Types in order (1,2...) and the colour of each bar indicates the Group. The y-axis is the index scores.
}
\value{
A 'visualise' object; the first part contains the data frame with average index scores for the 'Type' classification, the second part contains a further list of all the coloured bar charts that show average index score for each variable, by Type and Group.

Usage
visualise[[1]] = data frame of average index scores
visualise[[2]][[1]] = produces a graph of the average index score by Type and Group for the first variable in the data set. The number in the second subscript can also be specified as a variable name.

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

#visualise results

more_results<-visualise_classification(final_table,data,
                                       variables=colnames(standardised)[1:5])
}
