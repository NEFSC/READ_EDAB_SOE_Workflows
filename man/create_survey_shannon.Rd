% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_shannon.R
\name{create_survey_shannon}
\alias{create_survey_shannon}
\title{Calculates survey_shannon diversity index for automated workflow}
\usage{
create_survey_shannon(inputPathAlbatross, inputPathBigelow)
}
\arguments{
\item{inputPathAlbatross}{Character string. Full path to the Albatross data pull rds file}

\item{inputPathBigelow}{Character string. Full path to the Bigelow data pull rds file}
}
\value{
ecodata::survey_shannon data frame
}
\description{
This uses the survdat data pull from the survey package.
It is formatted exactly like the ecodata data object
This calculates the shannon diversity at the station level (by year, cruise etc)
Then takes the mean of the shannon indices for each year
}
\examples{
\dontrun{
# create the ecodata::survey_shannon indicator
create_aggregate_biomass(inputPathAlbatros = "path/to/albatross.rds"),
                         inputPathBigelow = "path/to/bigelow.rds")

}


}
