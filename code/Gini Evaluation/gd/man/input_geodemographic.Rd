\name{input_geodemographic}
\alias{input_geodemographic}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data And Specification File Input}
\description{
This function reads an external specification text file that describes how a set of input data should be processed. Input data can either be a pre existing dataframe within the workspace or location of a CSV.  The specification file details those columns within the input dataframe that represent pairs of numerators and denominators or noncounts.
}
\usage{
input_geodemographic(data, spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the data, either as a .CSV file in a form ready to be read in by R (headers are assumed present), or as a preexisting dataframe that has already been inputted.
}
  \item{spec}{
the location of a text file providing information specific to each variable contained within the input data. How this should be formatted is strict: see below for further details.
}
}
\details{
A set of attributes are read from the specification file and attached to  the input dataframe columns. These include whether the column is an enumerator, denominator or a non-count; which enumerator-denominator columns are paired; whether index scores or percentages are required for the count data; and finally, what type of standardisation is required (log or box cox).

These attributes are used later for the automated transform in the function \code{\link{prepare_geodemographic}}

The specification file needs to be written according to a strict format so that the input_geodemographic function can parse it and assign the correct attributes to the columns within the dataframe. An example is shown below:

count ks0020002-ks0020017 | ks0020001  \cr
count ks0050002-ks0050008 | ks0050001  \cr
noncount density  \cr
transform ks0020002-ks0020017 | IND,LOG  \cr
transform ks0050002-ks0050005,ks0050008 | PCT,BC  \cr
transform ks0050006-ks0050007 | PCT,NONE  \cr
zonecodes zone_code \cr


Each variables that make up numerators in the geodemographic should be specified after count, with a "|" character seperating the range of numerators and their corresponding denominator. If specifying a range, seperate the first and last of the range by a "-", if selecting individual variables, please seperate these by a comma. In the above spec file, variables ks0020002-ks0020017 would be numerators all with denominator ks0020001.

Variables that are non-count data, for example population density, should be specified individually in one line, seperated by commas, after the word "noncount".

Any information regarding how the variables should be transformed before clustering takes place should also be written in the specification file on a new line and after the word transform. If a range of variables use the same transformation methods, a range can also be specified by separating the first and last variables in the range by a "-" character (as in the "count" lines).

The transformation methods for each of the variables should be specified after a bar ("|") character. The currently available options are PCT (calculate as a percentage using the numerator and denominator) or IND (calculated as an index score). A user can then choose one of three possibilities to scale the percentage or index score, whether it be a log transformation ("LOG"), box-cox transformation ("BC"), or none ("NONE").

The information about each zone is kept in the resulting data frame as the 'names' of its rows. This MUST be indicated in the spec file after the word 'zonecodes'.

It is often useful to initially examine non-transformed data with the prepare.geodemographic by specifying NONE for all variables in the input_geodemographic function. This will give an indication if and what type of transformation may be necessary. The input_geodemographic function can then be re-run with selected transformations.

The function also prints out a list of the following to the console:

1. Any variable that was not included in the specification file.

2. Any variable that was included in the specification file but without transform information.

The default transform information in the case of 2. is to set it to "NONE".
}
\value{
A dataframe is returned, with all information about variables and the transformations they need stored as attributes. 
}

\author{
Alex D. Singleton, Liam Bratley
}
\note{
The parsing element of this function reads in the spec file line by line, so it does not matter what order the lines are entered, as long as there is no whitespace or excess characters at the end of each line. 
}

\examples{

##Using example datasets
data(specification)
data(liverpool_census)
Data_with_attribute_info<- input_geodemographic(liverpool_census,specification)

}
