% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tvtools.R
\name{last.panel.gap}
\alias{last.panel.gap}
\title{last.panel.gap}
\usage{
last.panel.gap(
  dat,
  id.name,
  t1.name,
  t2.name,
  gap.name = "gap_before",
  first.value = 0,
  expected.gap.between = 0,
  append.to.table = FALSE
)
}
\arguments{
\item{dat}{A data frame structured as panel data.}

\item{id.name}{The character name of the identifying variable within dat, used to
track subjects across multiple rows of data.}

\item{t1.name}{The character name of the time variable within dat representing the
start (left endpoint) of observation intervals.}

\item{t2.name}{The character name of the time variable within dat representing the
end (right endpoint) of observation intervals.}

\item{gap.name}{A character value for the name of the variable indicating whether a
gap is observed before the record.}

\item{first.value}{The numeric value indicating the expected beginning time of the
observation period for each subject.}

\item{expected.gap.between}{The numeric value specifying the expected time between
the end of one record and the start of the next; defaults to zero for continuous observation.}

\item{append.to.table}{A logical indicating whether to append the identified last gap
times as a new column to the existing data.frame (TRUE) or return them as a separate data frame (FALSE, default).}
}
\value{
If append.to.table is FALSE, returns a data table with the time points of the last gap
for each subject, keyed by the id.name. Each row will correspond to a unique subject, including
the time point of their last observed gap, if any. If append.to.table is TRUE, the original data
table is returned with an additional column containing these time points for each subject. This
functionality aids in analyzing and understanding the patterns of missing observations or breaks
in data collection within the study period.
}
\description{
Identifies the time point of the last observed gap in observation for each unique
subject in a panel data structure. A gap represents a missing period between recorded observations,
important for assessing data completeness and continuity.
}
